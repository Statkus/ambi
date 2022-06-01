with Ada.Text_IO; use Ada.Text_IO;

package body Room is

   use Web_Methods.Websocket;

   use type Api.T_Api_Provider;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize
     (Name           : in String;
      Ambi_Database  : in not null Database.T_Database_Class_Access;
      Api_Dispatcher : in not null Api.Dispatcher.T_Dispatcher_Access;
      Websocket : in not null Web_Methods.Websocket.T_Websocket_Class_Access) return T_Room_Access
   is
      New_Room : constant T_Room_Access :=
        new T_Room'
          (Name_Length             => Name'Length,
           Name                    => Name,
           Db                      => Ambi_Database,
           Api_Dispatcher          => Api_Dispatcher,
           Websocket               => Websocket,
           Client_List             => Client.List.Initialize,
           Auto_Playback_Requested => False,
           Current_Song            => Song.Initialize,
           Playlist                => Song.Item.List.Initialize,
           Song_Suggestions        => Song.List.Initialize,
           Current_Item_Id         => Song.Item.T_Item_Id'First,
           Next_Song_Votes         => Natural'First,
           Sync_Task               => null,
           Next_Song_Ready         => False,
           Last_Request_Time       => Ada.Real_Time.Clock,
           Block_Websocket         => False,
           Global_Mutex            => <>,
           Chat_Log                => Null_Unbounded_String);
   begin
      New_Room.Db.Add_To_Rooms (Name);

      New_Room.Sync_Task := new T_Sync_Task (New_Room);

      return New_Room;
   end New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Equality operator
   -------------------------------------------------------------------------------------------------
   function "=" (Left, Right : in T_Room) return Boolean is (Left.Name = Right.Name);

   -------------------------------------------------------------------------------------------------
   -- Lock
   -------------------------------------------------------------------------------------------------
   procedure Lock (This : in out T_Room) is
   begin
      select
         This.Global_Mutex.Lock;
      or
         delay 10.0;
      end select;
   end Lock;

   -------------------------------------------------------------------------------------------------
   -- Unlock
   -------------------------------------------------------------------------------------------------
   procedure Unlock (This : in out T_Room) is
   begin
      This.Global_Mutex.Unlock;
   end Unlock;

   -------------------------------------------------------------------------------------------------
   -- Add_Client
   -------------------------------------------------------------------------------------------------
   procedure Add_Client (This : in out T_Room; Session_Id : in Aws.Session.Id) is
   begin
      This.Remove_Disconnected_Client;

      -- Set a parameter to this session ID to register it, the parameter is a unique ID
      Aws.Session.Set (Session_Id, "ID", Aws.Session.Image (Session_Id));

      This.Client_List.Append (Client.New_And_Initialize (Session_Id));
      This.Auto_Playback_Requested := True;

      This.Websocket.Send_Room_Request (This.Name, Update_Nb_Clients);
      This.Websocket.Send_Room_Request (This.Name, Update_Next_Song_Votes);

      This.Last_Request_Time := Ada.Real_Time.Clock;

      Put_Line
        ("Room " &
         This.Name &
         ", new client " &
         Aws.Session.Image (Session_Id) &
         ", number of clients:" &
         This.Client_List.Length'Img);
   end Add_Client;

   -------------------------------------------------------------------------------------------------
   -- Display_Client_Player
   -------------------------------------------------------------------------------------------------
   procedure Display_Client_Player
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id;
      Display    : in     Boolean)
   is
   begin
      This.Get_Client (Session_Id).Display_Player (Display => Display);

      This.Update_Auto_Playback_Requested;
   end Display_Client_Player;

   -------------------------------------------------------------------------------------------------
   -- Sync_Client_With_Room
   -------------------------------------------------------------------------------------------------
   procedure Sync_Client_With_Room
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id;
      Sync       : in     Boolean)
   is
   begin
      This.Get_Client (Session_Id).Sync_With_Room
      (Synced => Sync, Room_Current_Song => This.Current_Song, Room_Playlist => This.Playlist);

      This.Update_Auto_Playback_Requested;
   end Sync_Client_With_Room;

   -------------------------------------------------------------------------------------------------
   -- Add_Song_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Add_Song_To_Playlist
     (This         : in out T_Room;
      Session_Id   : in     Aws.Session.Id;
      New_Song     : in     Song.T_Song;
      Low_Priority : in     Boolean := False)
   is
      Item : constant Song.Item.T_Item :=
        Song.Item.Initialize
          (Id        => This.Current_Item_Id,
           Item_Song => New_Song,
           Client_Id => Session_Id);
   begin
      if New_Song.Get_Provider /= Api.No_Provider_Api then
         if This.Playlist.Is_Empty and This.Current_Song.Get_Provider = Api.No_Provider_Api then
            This.Current_Song := New_Song;

            This.Db.Add_To_Room_History (This.Name, New_Song);

            This.Sync_Task.Start_Playing;
         else
            This.Playlist.Append (Item);
         end if;

         This.Client_List.Add_Item_To_Playlist (Item);

         if not Low_Priority then
            This.Playlist.Up_Vote (This.Current_Item_Id);
            This.Client_List.Up_Vote_Playlist_Item (This.Current_Item_Id);
         end if;

         Song.Item.Next (This.Current_Item_Id);

         if not This.Block_Websocket then
            This.Websocket.Send_Room_Request (This.Name, Update_Playlist);
         end if;
      end if;
   end Add_Song_To_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Remove_Item_From_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Remove_Item_From_Playlist (This : in out T_Room; Item_Id : in Song.Item.T_Item_Id) is
   begin
      This.Playlist.Delete (Item_Id);
      This.Client_List.Remove_Item_From_Playlist (Item_Id);

      This.Websocket.Send_Room_Request (This.Name, Update_Playlist);
   end Remove_Item_From_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote_Playlist_Item (This : in out T_Room; Item_Id : in Song.Item.T_Item_Id) is
   begin
      This.Playlist.Up_Vote (Item_Id);
      This.Client_List.Up_Vote_Playlist_Item (Item_Id);

      This.Websocket.Send_Room_Request (This.Name, Update_Playlist);
   end Up_Vote_Playlist_Item;

   -------------------------------------------------------------------------------------------------
   -- Add_Like
   -------------------------------------------------------------------------------------------------
   procedure Add_Like (This : in out T_Room; New_Song : in Song.T_Song) is
   begin
      This.Db.Add_To_Room_Likes (This.Name, New_Song);

      This.Websocket.Send_Room_Request (This.Name, Update_Playlist);
      This.Websocket.Send_Room_Request (This.Name, Update_History);
      This.Websocket.Send_Room_Request (This.Name, Update_Likes);
      This.Websocket.Send_Room_Request (This.Name, Update_Suggestions);
      This.Last_Request_Time := Ada.Real_Time.Clock;
   end Add_Like;

   -------------------------------------------------------------------------------------------------
   -- Remove_Like
   -------------------------------------------------------------------------------------------------
   procedure Remove_Like (This : in out T_Room; Old_Song : in Song.T_Song) is
   begin
      This.Db.Remove_From_Room_Likes (This.Name, Old_Song);

      This.Websocket.Send_Room_Request (This.Name, Update_Playlist);
      This.Websocket.Send_Room_Request (This.Name, Update_History);
      This.Websocket.Send_Room_Request (This.Name, Update_Likes);
      This.Websocket.Send_Room_Request (This.Name, Update_Suggestions);
      This.Last_Request_Time := Ada.Real_Time.Clock;
   end Remove_Like;

   -------------------------------------------------------------------------------------------------
   -- Next_Song
   -------------------------------------------------------------------------------------------------
   procedure Next_Song (This : in out T_Room) is
      use type Ada.Real_Time.Time;

      Next_Song_Request_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      if This.Current_Song.Get_Provider /= Api.No_Provider_Api then
         This.Next_Song_Votes := This.Next_Song_Votes + 1;

         if This.Next_Song_Votes > This.Client_List.Length / 2 then
            This.Next_Song_Ready := False;
            This.Next_Song_Votes := Natural'First;

            This.Sync_Task.Skip_Song;

            while not This.Next_Song_Ready and
              Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Next_Song_Request_Time) < 10.0
            loop
               null;
            end loop;

            This.Websocket.Send_Room_Request (This.Name, Force_Next_Song);
         end if;

         This.Websocket.Send_Room_Request (This.Name, Update_Next_Song_Votes);
      end if;
   end Next_Song;

   -------------------------------------------------------------------------------------------------
   -- Add_Chat_Message
   -------------------------------------------------------------------------------------------------
   procedure Add_Chat_Message (This : in out T_Room; Message : in String) is
   begin
      if Message'Length /= Natural'First then
         if This.Chat_Log = Null_Unbounded_String then
            Append (Source => This.Chat_Log, New_Item => Message);
         else
            Append (Source => This.Chat_Log, New_Item => "<br/>" & Message);
         end if;
      end if;

      This.Websocket.Send_Room_Request (This.Name, Update_Chat_Log);
   end Add_Chat_Message;

   -------------------------------------------------------------------------------------------------
   -- Get_Name
   -------------------------------------------------------------------------------------------------
   function Get_Name (This : in T_Room) return String is (This.Name);

   -------------------------------------------------------------------------------------------------
   -- Is_Client_Registered
   -------------------------------------------------------------------------------------------------
   function Is_Client_Registered
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return Boolean
   is
      use type Client.T_Client_Access;
   begin
      return This.Get_Client (Session_Id) /= null;
   end Is_Client_Registered;

   -------------------------------------------------------------------------------------------------
   -- Get_Client
   -------------------------------------------------------------------------------------------------
   function Get_Client
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return Client.T_Client_Access is
     (This.Client_List.Get_Client (Session_Id));

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Of_Clients
   -------------------------------------------------------------------------------------------------
   function Get_Number_Of_Clients (This : in T_Room) return Natural is (This.Client_List.Length);

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Song
   -------------------------------------------------------------------------------------------------
   function Get_Current_Song (This : in T_Room) return Song.T_Song is (This.Current_Song);

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (This : in T_Room) return Song.Item.List.T_Item_List is
   begin
      return Item_List : Song.Item.List.T_Item_List do
         Item_List.Set (This.Playlist.Get);
      end return;
   end Get_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Get_Suggestions
   -------------------------------------------------------------------------------------------------
   function Get_Suggestions
     (This : in T_Room) return Song.List.T_Song_List is
     (This.Song_Suggestions);

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in out T_Room;
      Session_Id   : in     Aws.Session.Id;
      Search_Input : in     String) return Song.List.T_Song_List
   is
      use Song.List;

      ----------------------------------------------------------------------------------------------
      -- Add_Playlist
      ----------------------------------------------------------------------------------------------
      procedure Add_Playlist (Element : in Song.T_Song) is
      begin
         This.Add_Song_To_Playlist (Session_Id, Element, True);
      end Add_Playlist;

      Search_Type : Api.T_Search_Type;
      Song_List   : Song.List.T_Song_List :=
        This.Api_Dispatcher.Get_Song_Search_Results (Api.Youtube_Api, Search_Input, Search_Type);
   begin
      case Search_Type is
         when Api.Video_Link =>
            if not Song_List.Is_Empty then
               This.Add_Song_To_Playlist (Session_Id, Song_List.First_Element);
               Song_List := Song.List.Initialize;

               This.Websocket.Send_Room_Request (This.Name, Clear_Search_Input);
            end if;

         when Api.Playlist_Link =>
            if not Song_List.Is_Empty then
               This.Block_Websocket := True;

               Song_List.Iterate (Add_Playlist'Access);
               Song_List := Song.List.Initialize;

               This.Block_Websocket := False;
               This.Websocket.Send_Room_Request (This.Name, Clear_Search_Input);
               This.Websocket.Send_Room_Request (This.Name, Update_Playlist);
            end if;

         when Api.Words =>
            null;
      end case;

      return Song_List;
   end Get_Song_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Get_History
   -------------------------------------------------------------------------------------------------
   function Get_History
     (This : in T_Room) return Song.List.T_Song_List is
     (This.Db.Get_Room_History (This.Name));

   -------------------------------------------------------------------------------------------------
   -- Get_Likes
   -------------------------------------------------------------------------------------------------
   function Get_Likes
     (This : in T_Room) return Song.List.T_Song_List is
     (This.Db.Get_Room_Likes (This.Name));

   -------------------------------------------------------------------------------------------------
   -- Is_Song_Liked
   -------------------------------------------------------------------------------------------------
   function Is_Song_Liked
     (This          : in T_Room;
      Song_To_Check : in Song.T_Song) return Boolean is
     (This.Db.Is_Room_Song_Liked (This.Name, Song_To_Check));

   -------------------------------------------------------------------------------------------------
   -- Is_Auto_Playback_Requested
   -------------------------------------------------------------------------------------------------
   function Is_Auto_Playback_Requested
     (This : in T_Room) return Boolean is
     (This.Auto_Playback_Requested);

   -------------------------------------------------------------------------------------------------
   -- Get_Next_Song_Votes
   -------------------------------------------------------------------------------------------------
   function Get_Next_Song_Votes (This : in T_Room) return Natural is (This.Next_Song_Votes);

   -------------------------------------------------------------------------------------------------
   -- Get_Chat_Log
   -------------------------------------------------------------------------------------------------
   function Get_Chat_Log (This : in T_Room) return String is (To_String (This.Chat_Log));

   -------------------------------------------------------------------------------------------------
   -- T_Sync_Task
   -------------------------------------------------------------------------------------------------
   task body T_Sync_Task is
      Playlist_Empty : Boolean := False;
   begin
      loop
         -- Wait for the start of a playlist
         select
            accept Start_Playing;
         or
            terminate;
         end select;

         Playlist_Empty := False;
         This.Websocket.Send_Room_Request (This.Name, Show_Next_Room_Song_Button);
         loop
            if This.Current_Song.Get_Provider /= Api.No_Provider_Api then
               This.Song_Suggestions :=
                 (This.Api_Dispatcher.Get_Related_Songs
                  (This.Current_Song).Select_First_Songs_Not_In_Exclusion_List
                  (Number_Of_Songs                         =>
                     Number_Of_Suggestions, Exclusion_List =>
                     This.Db.Get_Room_Last_Songs (This.Name, Number_Of_Excluded_Songs)));
            end if;

            This.Next_Song_Ready := True;
            This.Next_Song_Votes := Natural'First;

            This.Websocket.Send_Room_Request (This.Name, Update_Room_Current_Song);
            This.Last_Request_Time := Ada.Real_Time.Clock;

            exit when Playlist_Empty;

            -- Wait for the duration of the current song or next song entry
            select
               accept Skip_Song;
            or
               delay Duration (This.Api_Dispatcher.Get_Song_Duration (This.Current_Song));
            end select;

            -- Remove all expired sessions
            This.Remove_Disconnected_Client;

            if not This.Playlist.Is_Empty then
               -- If the playlist is not empty, select the next song
               This.Current_Song := This.Playlist.First_Element.Get_Song;

               This.Playlist.Delete_First;

               -- Add the current song to the history
               This.Db.Add_To_Room_History (This.Name, This.Current_Song);
            else
               if This.Auto_Playback_Requested and not This.Song_Suggestions.Is_Empty then
                  -- Auto playback is requested, play a song following suggestion
                  This.Current_Song := This.Song_Suggestions.First_Element;

                  if This.Current_Song.Get_Provider = Api.No_Provider_Api then
                     -- There is no suggestion, go back at waiting for the start of a new playlist
                     Playlist_Empty := True;
                     This.Websocket.Send_Room_Request (This.Name, Hide_Next_Room_Song_Button);
                  else
                     -- Add the current song to the history
                     This.Db.Add_To_Room_History (This.Name, This.Current_Song);
                  end if;
               else
                  -- The playlist is empty and there is no auto playback request, go back at waiting
                  -- for the start of a new playlist
                  This.Current_Song := Song.Initialize;

                  Playlist_Empty := True;
                  This.Websocket.Send_Room_Request (This.Name, Hide_Next_Room_Song_Button);
               end if;
            end if;
         end loop;
      end loop;
   end T_Sync_Task;

   -------------------------------------------------------------------------------------------------
   -- Update_Auto_Playback_Requested
   -------------------------------------------------------------------------------------------------
   procedure Update_Auto_Playback_Requested (This : in out T_Room) is
      Previous_Auto_Playback_Requested : constant Boolean := This.Auto_Playback_Requested;
   begin
      This.Auto_Playback_Requested := This.Client_List.Is_Auto_Playback_Requested;

      if This.Auto_Playback_Requested /= Previous_Auto_Playback_Requested and
        This.Playlist.Is_Empty and
        This.Current_Song.Get_Provider /= Api.No_Provider_Api
      then
         This.Websocket.Send_Room_Request (This.Name, Update_Playlist);
      end if;
   end Update_Auto_Playback_Requested;

   -------------------------------------------------------------------------------------------------
   -- Remove_Disconnected_Client
   -------------------------------------------------------------------------------------------------
   procedure Remove_Disconnected_Client (This : in out T_Room) is
      Previous_Number_Of_Clients : constant Natural := This.Client_List.Length;
   begin
      This.Client_List.Remove_Disconnected_Client (This.Last_Request_Time);

      if Previous_Number_Of_Clients /= This.Client_List.Length then
         This.Websocket.Send_Room_Request (This.Name, Update_Nb_Clients);
         This.Websocket.Send_Room_Request (This.Name, Update_Next_Song_Votes);

         This.Last_Request_Time := Ada.Real_Time.Clock;

         This.Update_Auto_Playback_Requested;

         if This.Client_List.Length = 0 then
            This.Chat_Log := Null_Unbounded_String;
         end if;

         Put_Line
           ("Room " &
            This.Name &
            ", removed " &
            Natural'Image (Previous_Number_Of_Clients - This.Client_List.Length) &
            " clients, number of clients:" &
            This.Client_List.Length'Img);
      end if;
   end Remove_Disconnected_Client;

end Room;
