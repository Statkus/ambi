with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Aws.Net.Websocket.Registry;
with Aws.Session; use Aws.Session;

with Api;
with Client; use Client;

package body Room is

   procedure Free_Client is new Ada.Unchecked_Deallocation (T_Client'Class, T_Client_Class_Access);

   -------------------------------------------------------------------------------------------------
   -- T_Room_Sync_Task
   -------------------------------------------------------------------------------------------------
   task body T_Room_Sync_Task is
      use type Api.T_Api_Provider;

      Playlist_Empty : Boolean := False;

      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & This.Get_Name & "Socket");
   begin
      loop
         -- Wait for the start of a playlist
         select
            accept Start_Room_Playlist;
         or
            terminate;
         end select;

         Playlist_Empty := False;
         loop
            -- Send request to the clients, the current room song has changed
            Aws.Net.Websocket.Registry.Send (Rcp, "update_room_current_video_request");
            This.Last_Request_Time := Ada.Real_Time.Clock;

            exit when Playlist_Empty;

            -- Wait for the duration of the current song or next room song entry
            select
               accept Next_Room_Song;
            or
               delay Duration (This.Api_Dispatcher.Get_Song_Duration (This.Get_Song));
            end select;

            -- Remove all expired sessions
            This.Remove_Disconnected_Client;

            if not This.Get_Playlist_Is_Empty then
               -- If the playlist is not empty, select the next song
               This.Set_Song (This.Get_Playlist_First.Get_Song);
               This.Playlist_Delete_First;

               -- Add the current song to the historic
               This.Db.Add_To_Room_Historic (This.Get_Name, This.Get_Song);
            else
               if This.Is_Client_Sync_And_Play then
                  -- There is at least one client sync with the room with the player activated, play
                  -- a song following Youtube suggestion
                  This.Set_Song
                  (This.Select_Related_Song
                   (This.Api_Dispatcher.Get_Related_Songs (This.Get_Song)));

                  if This.Get_Song.Get_Provider = Api.No_Provider_Api then
                     -- There is no suggestion, go back at waiting for the start of a new
                     -- playlist
                     This.Room_Current_Song_Active := False;
                     Playlist_Empty                := True;
                  else
                     -- Add the current song to the historic
                     This.Db.Add_To_Room_Historic (This.Get_Name, This.Get_Song);
                  end if;
               else
                  -- The playlist is empty and there is no sync client, go back at waiting for the
                  -- start of a new playlist
                  This.Room_Current_Song_Active := False;
                  This.Set_Song (Song.Constructors.Initialize);

                  Playlist_Empty := True;
               end if;
            end if;

            This.Update_No_Player_Clients;
            This.Room_Next_Song_Ready := True;
         end loop;
      end loop;
   end T_Room_Sync_Task;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize
     (Name           : in String;
      Db             : in not null Database.T_Database_Access;
      Api_Dispatcher : in Api.Dispatcher.T_Dispatcher_Access) return T_Room_Class_Access
   is
      New_Room : constant T_Room_Class_Access :=
        new T_Room'
          (Name           => To_Unbounded_String (Name),
           Db             => Db,
           Api_Dispatcher => Api_Dispatcher,
           others         => <>);
   begin
      Db.Add_To_Rooms (Name);

      New_Room.Set_Room_Sync_Task (new Room.T_Room_Sync_Task (New_Room));

      return New_Room;
   end New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Set_Room_Sync_Task
   -------------------------------------------------------------------------------------------------
   procedure Set_Room_Sync_Task
     (This      : in out T_Room;
      Sync_Task : in     not null T_Room_Sync_Task_Access)
   is
   begin
      This.Room_Sync_Task := Sync_Task;
   end Set_Room_Sync_Task;

   -------------------------------------------------------------------------------------------------
   -- Lock
   -------------------------------------------------------------------------------------------------
   procedure Lock (This : in out T_Room) is
   begin
      This.Room_Callback_Mutex.Seize;
   end Lock;

   -------------------------------------------------------------------------------------------------
   -- Unlock
   -------------------------------------------------------------------------------------------------
   procedure Unlock (This : in out T_Room) is
   begin
      This.Room_Callback_Mutex.Release;
   end Unlock;

   -------------------------------------------------------------------------------------------------
   -- Add_Client
   -------------------------------------------------------------------------------------------------
   procedure Add_Client (This : in out T_Room; Session_Id : in Aws.Session.Id) is
      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & This.Get_Name & "Socket");
   begin
      -- Remove all expired sessions
      This.Remove_Disconnected_Client;

      -- Set a parameter to this session ID to register it, the parameter is a unique ID
      Aws.Session.Set (Session_Id, "ID", Aws.Session.Image (Session_Id));

      -- Add the new client to the list and set his session ID
      This.Client_List.Append (new Client.T_Client);
      This.Client_List.Last_Element.Set_Session_Id (Session_Id);

      This.Client_List.Last_Element.Set_Current_Song (This.Get_Song);
      This.Client_List.Last_Element.Set_Playlist (This.Get_Playlist);

      -- Send update request for the number of clients
      Aws.Net.Websocket.Registry.Send (Rcp, "update_nb_clients");

      Put_Line
        ("Room " &
         This.Get_Name &
         ", new client " &
         Aws.Session.Image (Session_Id) &
         ", number of clients:" &
         This.Client_List.Length'Img);
   end Add_Client;

   -------------------------------------------------------------------------------------------------
   -- Set_Client_Last_Request_Time
   -------------------------------------------------------------------------------------------------
   procedure Set_Client_Last_Request_Time (This : in out T_Room; Session_Id : in Aws.Session.Id) is
   begin
      This.Find_Client_From_Session_Id (Session_Id).Set_Last_Request_Time;
   end Set_Client_Last_Request_Time;

   -------------------------------------------------------------------------------------------------
   -- Is_Registered
   -------------------------------------------------------------------------------------------------
   function Is_Registered
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id) return Boolean is
     (This.Find_Client_From_Session_Id (Session_Id) /= null);

   -------------------------------------------------------------------------------------------------
   -- Add_Song_To_Playlists
   -------------------------------------------------------------------------------------------------
   procedure Add_Song_To_Playlists
     (This         : in out T_Room;
      Session_Id   : in     Aws.Session.Id;
      New_Song     : in     T_Song;
      Low_Priority : in     Boolean := False)
   is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      -- Add the song to the room playlist for room sync
      if This.Get_Playlist_Is_Empty and not This.Room_Current_Song_Active then
         This.Room_Current_Song_Active := True;
         This.Set_Song (New_Song);

         -- Add the current song to the historic
         This.Db.Add_To_Room_Historic (This.Get_Name, New_Song);

         This.Room_Sync_Task.Start_Room_Playlist;
      else
         This.Playlist_Append
         (Playlist_Item.Constructors.Initialize
            (Item_Song => New_Song,
             Id        => This.Current_Playlist_Item_Id,
             Client_Id => Session_Id));
      end if;

      -- Add the song to all the clients playlist
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if not Client_Vectors.Element (Client_List_Cursor).Get_Display_Player or
           Client_Vectors.Element (Client_List_Cursor).Get_Sync_With_Room
         then
            -- If the client is sync with the room (sync player or no player) then sync it
            Client_Vectors.Element (Client_List_Cursor).Set_Current_Song (This.Get_Song);
            Client_Vectors.Element (Client_List_Cursor).Set_Playlist (This.Get_Playlist);
         else
            -- Otherwise only add the song to the playlist client
            Client_Vectors.Element (Client_List_Cursor).Add_Item_To_Playlist
            (Playlist_Item.Constructors.Initialize
               (Item_Song => New_Song,
                Id        => This.Current_Playlist_Item_Id,
                Client_Id => Session_Id));
         end if;

         Client_Vectors.Next (Client_List_Cursor);
      end loop;

      if not Low_Priority then
         This.Up_Vote_Playlist_Item (This.Current_Playlist_Item_Id);
      end if;

      This.Current_Playlist_Item_Id := This.Current_Playlist_Item_Id + 1;
   end Add_Song_To_Playlists;

   -------------------------------------------------------------------------------------------------
   -- Remove_From_Playlists
   -------------------------------------------------------------------------------------------------
   procedure Remove_From_Playlists (This : in out T_Room; Item_Id : in T_Playlist_Item_Id) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      -- Remove the song from the room playlist for room sync
      This.Playlist_Remove_Item (Item_Id);

      -- Remove the song from all the clients playlist
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         Client_Vectors.Element (Client_List_Cursor).Remove_Item_From_Playlist (Item_Id);
         Client_Vectors.Next (Client_List_Cursor);
      end loop;
   end Remove_From_Playlists;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote_Playlist_Item (This : in out T_Room; Item_Id : in T_Playlist_Item_Id) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      This.Playlist_Up_Vote_Item (Item_Id);

      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         Client_Vectors.Element (Client_List_Cursor).Up_Vote_Playlist_Item (Item_Id);
         Client_Vectors.Next (Client_List_Cursor);
      end loop;
   end Up_Vote_Playlist_Item;

   -------------------------------------------------------------------------------------------------
   -- Add_Like
   -------------------------------------------------------------------------------------------------
   procedure Add_Like (This : in out T_Room; New_Song : in T_Song) is
   begin
      This.Db.Add_To_Room_Likes (This.Get_Name, New_Song);
   end Add_Like;

   -------------------------------------------------------------------------------------------------
   -- Remove_Like
   -------------------------------------------------------------------------------------------------
   procedure Remove_Like (This : in out T_Room; Old_Song : in T_Song) is
   begin
      This.Db.Remove_From_Room_Likes (This.Get_Name, Old_Song);
   end Remove_Like;

   -------------------------------------------------------------------------------------------------
   -- Next_Room_Song
   -------------------------------------------------------------------------------------------------
   procedure Next_Room_Song (This : in out T_Room) is
   begin
      This.Room_Next_Song_Ready := not This.Room_Current_Song_Active;

      if This.Room_Current_Song_Active then
         This.Room_Sync_Task.Next_Room_Song;
      end if;
   end Next_Room_Song;

   -------------------------------------------------------------------------------------------------
   -- Next_Client_Song
   -------------------------------------------------------------------------------------------------
   procedure Next_Client_Song (This : in out T_Room; Session_Id : in Aws.Session.Id) is
      Current_Client : constant Client.T_Client_Class_Access :=
        This.Find_Client_From_Session_Id (Session_Id);
   begin
      if Current_Client.Get_Sync_With_Room then
         -- Synchronized the client playlist and current song with the room ones
         Current_Client.Set_Current_Song (This.Get_Song);
         Current_Client.Set_Playlist (This.Get_Playlist);
      else
         -- Set the first client song in the playlist as the current client song and remove
         -- it from the playlist
         Current_Client.Set_Current_Song;
         Current_Client.Remove_First_Playlist_Item;
      end if;
   end Next_Client_Song;

   -------------------------------------------------------------------------------------------------
   -- Set_Client_Display_Player
   -------------------------------------------------------------------------------------------------
   procedure Set_Client_Display_Player
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id;
      Display    : in     Boolean)
   is
      Current_Client : constant Client.T_Client_Class_Access :=
        This.Find_Client_From_Session_Id (Session_Id);
   begin
      Current_Client.Set_Display_Player (Display);

      if not Display then
         -- Synchronized the client playlist and current song with the room ones
         Current_Client.Set_Sync_With_Room (True);
         Current_Client.Set_Current_Song (This.Get_Song);
         Current_Client.Set_Playlist (This.Get_Playlist);
      end if;

      -- Count number of sync clients
      This.Number_Of_Clients_Sync := This.Count_Number_Of_Clients_Sync;
   end Set_Client_Display_Player;

   -------------------------------------------------------------------------------------------------
   -- Set_Client_Sync_With_Room
   -------------------------------------------------------------------------------------------------
   procedure Set_Client_Sync_With_Room
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id;
      Sync       : in     Boolean)
   is
      Current_Client : constant Client.T_Client_Class_Access :=
        This.Find_Client_From_Session_Id (Session_Id);
   begin
      Current_Client.Set_Sync_With_Room (Sync);

      if Sync then
         -- Synchronized the client playlist and current song with the room ones
         Current_Client.Set_Current_Song (This.Get_Song);
         Current_Client.Set_Playlist (This.Get_Playlist);
      end if;

      -- Count number of sync clients
      This.Number_Of_Clients_Sync := This.Count_Number_Of_Clients_Sync;
   end Set_Client_Sync_With_Room;

   -------------------------------------------------------------------------------------------------
   -- Get_Name
   -------------------------------------------------------------------------------------------------
   function Get_Name (This : in T_Room) return String is (To_String (This.Name));

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Song
   -------------------------------------------------------------------------------------------------
   function Get_Current_Song (This : in out T_Room) return T_Song is (This.Get_Song);

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in out T_Room;
      Session_Id   : in     Aws.Session.Id;
      Search_Input : in     String;
      Direct_Link  :    out Boolean) return T_Song_Vector
   is
      Search_Type : Api.T_Search_Type;
      Songs       : T_Song_Vector :=
        This.Api_Dispatcher.Get_Song_Search_Results (Api.Youtube_Api, Search_Input, Search_Type);
      Songs_Cursor : Song_Vectors.Cursor := Songs.First;
   begin
      case Search_Type is
         when Api.Video_Link =>
            if not Songs.Is_Empty then
               This.Add_Song_To_Playlists (Session_Id, Songs.First_Element, False);
               Direct_Link := True;
               Songs       := Song_Vector.Constructors.Initialize;
            else
               Direct_Link := False;
            end if;

         when Api.Playlist_Link =>
            while Song_Vectors.Has_Element (Songs_Cursor) loop
               This.Add_Song_To_Playlists (Session_Id, Song_Vectors.Element (Songs_Cursor), True);

               Song_Vectors.Next (Songs_Cursor);
            end loop;
            Direct_Link := True;
            Songs       := Song_Vector.Constructors.Initialize;

         when Api.Words =>
            Direct_Link := False;
      end case;

      return Songs;
   end Get_Song_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Get_Historic
   -------------------------------------------------------------------------------------------------
   function Get_Historic
     (This : in T_Room) return T_Song_Vector is
     (This.Db.Get_Room_Historic (This.Get_Name));

   -------------------------------------------------------------------------------------------------
   -- Get_Likes
   -------------------------------------------------------------------------------------------------
   function Get_Likes
     (This : in T_Room) return T_Song_Vector is
     (This.Db.Get_Room_Likes (This.Get_Name));

   -------------------------------------------------------------------------------------------------
   -- Is_Song_Liked
   -------------------------------------------------------------------------------------------------
   function Is_Song_Liked
     (This          : in T_Room;
      Song_To_Check : in T_Song) return Boolean is
     (This.Db.Is_Room_Song_Liked (This.Get_Name, Song_To_Check));

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Client_Song
   -------------------------------------------------------------------------------------------------
   function Get_Current_Client_Song
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return T_Song is
     (This.Find_Client_From_Session_Id (Session_Id).Get_Current_Song);

   -------------------------------------------------------------------------------------------------
   -- Get_Client_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Client_Playlist
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return T_Playlist is
     (This.Find_Client_From_Session_Id (Session_Id).Get_Playlist);

   -------------------------------------------------------------------------------------------------
   -- Get_Client_Display_Player
   -------------------------------------------------------------------------------------------------
   function Get_Client_Display_Player
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return Boolean is
     (This.Find_Client_From_Session_Id (Session_Id).Get_Display_Player);

   -------------------------------------------------------------------------------------------------
   -- Get_Client_Sync_With_Room
   -------------------------------------------------------------------------------------------------
   function Get_Client_Sync_With_Room
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return Boolean is
     (This.Find_Client_From_Session_Id (Session_Id).Get_Sync_With_Room);

   -------------------------------------------------------------------------------------------------
   -- Client_Has_Nothing_To_Play
   -------------------------------------------------------------------------------------------------
   function Client_Has_Nothing_To_Play
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id) return Boolean
   is
      use type Api.T_Api_Provider;

      Current_Client : constant Client.T_Client_Class_Access :=
        This.Find_Client_From_Session_Id (Session_Id);
      Nothing_To_Play : Boolean := False;
   begin
      if Current_Client.Get_Sync_With_Room then
         if This.Get_Song.Get_Provider = Api.No_Provider_Api then
            Nothing_To_Play := True;
         end if;
      else
         Nothing_To_Play := Current_Client.Has_Nothing_To_Play;
      end if;

      return Nothing_To_Play;
   end Client_Has_Nothing_To_Play;

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Clients
   -------------------------------------------------------------------------------------------------
   function Get_Number_Clients
     (This : in T_Room) return Natural is
     (Natural (This.Client_List.Length));

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Clients_Sync
   -------------------------------------------------------------------------------------------------
   function Get_Number_Clients_Sync
     (This : in T_Room) return Natural is
     (This.Number_Of_Clients_Sync);

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Next_Song_Ready
   -------------------------------------------------------------------------------------------------
   function Get_Room_Next_Song_Ready
     (This : in T_Room) return Boolean is
     (This.Room_Next_Song_Ready);

   -------------------------------------------------------------------------------------------------
   -- Update_No_Player_Clients
   -------------------------------------------------------------------------------------------------
   procedure Update_No_Player_Clients (This : in out T_Room) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if not Client_Vectors.Element (Client_List_Cursor).Get_Display_Player then
            Client_Vectors.Element (Client_List_Cursor).Set_Current_Song (This.Get_Song);
            Client_Vectors.Element (Client_List_Cursor).Set_Playlist (This.Get_Playlist);
         end if;

         Client_Vectors.Next (Client_List_Cursor);
      end loop;
   end Update_No_Player_Clients;

   -------------------------------------------------------------------------------------------------
   -- Count_Number_Of_Clients_Sync
   -------------------------------------------------------------------------------------------------
   function Count_Number_Of_Clients_Sync (This : in T_Room) return Natural is
      Client_List_Cursor     : Client_Vectors.Cursor := This.Client_List.First;
      Number_Of_Clients_Sync : Natural               := 0;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Sync_With_Room then
            Number_Of_Clients_Sync := Number_Of_Clients_Sync + 1;
         end if;

         Client_Vectors.Next (Client_List_Cursor);
      end loop;

      return Number_Of_Clients_Sync;
   end Count_Number_Of_Clients_Sync;

   -------------------------------------------------------------------------------------------------
   -- Find_Client_From_Session_ID
   -------------------------------------------------------------------------------------------------
   function Find_Client_From_Session_Id
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return Client.T_Client_Class_Access
   is
      Client_List_Cursor : Client_Vectors.Cursor        := This.Client_List.First;
      Client_To_Find     : Client.T_Client_Class_Access := null;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) and Client_To_Find = null loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Session_Id = Session_Id then
            Client_To_Find := Client_Vectors.Element (Client_List_Cursor);
         end if;

         Client_Vectors.Next (Client_List_Cursor);
      end loop;

      return Client_To_Find;
   end Find_Client_From_Session_Id;

   -------------------------------------------------------------------------------------------------
   -- Is_Client_Sync_And_Play
   -------------------------------------------------------------------------------------------------
   function Is_Client_Sync_And_Play (This : in out T_Room) return Boolean is
      Client_List_Cursor   : Client_Vectors.Cursor := This.Client_List.First;
      Client_Sync_And_Play : Boolean               := False;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) and not Client_Sync_And_Play loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Sync_With_Room and
           Client_Vectors.Element (Client_List_Cursor).Get_Display_Player
         then
            Client_Sync_And_Play := True;
         end if;

         Client_Vectors.Next (Client_List_Cursor);
      end loop;

      return Client_Sync_And_Play;
   end Is_Client_Sync_And_Play;

   -------------------------------------------------------------------------------------------------
   -- Remove_Disconnected_Client
   -------------------------------------------------------------------------------------------------
   procedure Remove_Disconnected_Client (This : in out T_Room) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
      Client_To_Remove   : T_Client_Class_Access := null;
      Clients_Removed    : Boolean               := False;

      Number_Of_Clients_Sync : Natural := 0;

      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & This.Get_Name & "Socket");
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if not Aws.Session.Exist (Client_Vectors.Element (Client_List_Cursor).Get_Session_Id) or
           (This.Last_Request_Time >
            Client_Vectors.Element (Client_List_Cursor).Get_Last_Request_Time
            and then
              Ada.Real_Time.To_Duration
                (This.Last_Request_Time -
                 Client_Vectors.Element (Client_List_Cursor).Get_Last_Request_Time) >
              120.0)
         then
            Client_To_Remove := Client_Vectors.Element (Client_List_Cursor);
            This.Client_List.Delete (Client_List_Cursor);

            Put_Line
              ("Room " &
               This.Get_Name &
               ", remove client " &
               Aws.Session.Image (Client_To_Remove.Get_Session_Id) &
               ", number of clients:" &
               This.Client_List.Length'Img);

            Free_Client (Client_To_Remove);

            Clients_Removed := True;

            Client_List_Cursor := This.Client_List.First;
         end if;

         Client_Vectors.Next (Client_List_Cursor);
      end loop;

      Number_Of_Clients_Sync := This.Count_Number_Of_Clients_Sync;

      if Clients_Removed or This.Number_Of_Clients_Sync /= Number_Of_Clients_Sync then
         -- Send update request for the number of clients
         Aws.Net.Websocket.Registry.Send (Rcp, "update_nb_clients");
      end if;

      This.Number_Of_Clients_Sync := Number_Of_Clients_Sync;
   end Remove_Disconnected_Client;

   -------------------------------------------------------------------------------------------------
   -- Select_Related_Song
   -------------------------------------------------------------------------------------------------
   function Select_Related_Song
     (This          : in out T_Room;
      Related_Songs : in     T_Song_Vector) return T_Song
   is
      use Song_Vector.Song_Vectors;

      Related_Songs_Cursor : Song_Vectors.Cursor    := Related_Songs.First;
      Related_Song_Found   : Boolean                := False;
      Last_Room_Songs      : constant T_Song_Vector :=
        This.Db.Get_Room_Last_Songs (This.Get_Name, Max_Last_Room_Songs);
      Related_Song : T_Song := Song.Constructors.Initialize;
   begin
      while not Related_Song_Found and Song_Vectors.Has_Element (Related_Songs_Cursor) loop
         if Last_Room_Songs.Find (Song_Vectors.Element (Related_Songs_Cursor)) =
           Song_Vectors.No_Element
         then
            Related_Song       := Song_Vectors.Element (Related_Songs_Cursor);
            Related_Song_Found := True;
         end if;

         Song_Vectors.Next (Related_Songs_Cursor);
      end loop;

      if not Related_Song_Found and Natural (Related_Songs.Length) > 0 then
         Related_Song := Song_Vectors.Element (Related_Songs.First);
      end if;

      return Related_Song;
   end Select_Related_Song;

   -------------------------------------------------------------------------------------------------
   -- Set_Song
   -------------------------------------------------------------------------------------------------
   procedure Set_Song (This : in out T_Room; Current_Song : in T_Song) is
   begin
      This.Room_Song_Playlist_Mutex.Seize;
      This.Room_Current_Song := Current_Song;
      This.Room_Song_Playlist_Mutex.Release;
   end Set_Song;

   -------------------------------------------------------------------------------------------------
   -- Playlist_Append
   -------------------------------------------------------------------------------------------------
   procedure Playlist_Append (This : in out T_Room; Item : in T_Playlist_Item) is
   begin
      This.Room_Song_Playlist_Mutex.Seize;
      This.Room_Playlist.Append (Item);
      This.Room_Song_Playlist_Mutex.Release;
   end Playlist_Append;

   -------------------------------------------------------------------------------------------------
   -- Playlist_Delete_First
   -------------------------------------------------------------------------------------------------
   procedure Playlist_Delete_First (This : in out T_Room) is
   begin
      This.Room_Song_Playlist_Mutex.Seize;
      This.Room_Playlist.Delete_First;
      This.Room_Song_Playlist_Mutex.Release;
   end Playlist_Delete_First;

   -------------------------------------------------------------------------------------------------
   -- Playlist_Remove_Item
   -------------------------------------------------------------------------------------------------
   procedure Playlist_Remove_Item (This : in out T_Room; Item_Id : in T_Playlist_Item_Id) is
      Item_Cursor : Playlist_Item_Vectors.Cursor := This.Room_Playlist.First;
   begin
      while Playlist_Item_Vectors.Has_Element (Item_Cursor) loop
         if Playlist_Item_Vectors.Element (Item_Cursor).Get_Id = Item_Id then
            This.Room_Song_Playlist_Mutex.Seize;
            This.Room_Playlist.Delete (Item_Cursor);
            This.Room_Song_Playlist_Mutex.Release;
         end if;

         Playlist_Item_Vectors.Next (Item_Cursor);
      end loop;
   end Playlist_Remove_Item;

   -------------------------------------------------------------------------------------------------
   -- Playlist_Up_Vote_Item
   -------------------------------------------------------------------------------------------------
   procedure Playlist_Up_Vote_Item (This : in out T_Room; Item_Id : in T_Playlist_Item_Id) is
   begin
      This.Room_Song_Playlist_Mutex.Seize;
      Up_Vote_Playlist_Item (This.Room_Playlist, Item_Id);
      This.Room_Song_Playlist_Mutex.Release;
   end Playlist_Up_Vote_Item;

   -------------------------------------------------------------------------------------------------
   -- Get_Song
   -------------------------------------------------------------------------------------------------
   function Get_Song (This : in out T_Room) return T_Song is
      Current_Song : T_Song;
   begin
      This.Room_Song_Playlist_Mutex.Seize;
      Current_Song := This.Room_Current_Song;
      This.Room_Song_Playlist_Mutex.Release;

      return Current_Song;
   end Get_Song;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (This : in out T_Room) return T_Playlist is
      Room_Playlist : T_Playlist := Playlist.Constructors.Initialize;
   begin
      This.Room_Song_Playlist_Mutex.Seize;
      Room_Playlist := This.Room_Playlist;
      This.Room_Song_Playlist_Mutex.Release;

      return Room_Playlist;
   end Get_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist_First
   -------------------------------------------------------------------------------------------------
   function Get_Playlist_First (This : in out T_Room) return T_Playlist_Item is
      Item : T_Playlist_Item;
   begin
      This.Room_Song_Playlist_Mutex.Seize;
      Item := Playlist_Item_Vectors.Element (This.Room_Playlist.First);
      This.Room_Song_Playlist_Mutex.Release;

      return Item;
   end Get_Playlist_First;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist_Is_Empty
   -------------------------------------------------------------------------------------------------
   function Get_Playlist_Is_Empty (This : in out T_Room) return Boolean is
      Is_Empty : Boolean;
   begin
      This.Room_Song_Playlist_Mutex.Seize;
      Is_Empty := This.Room_Playlist.Is_Empty;
      This.Room_Song_Playlist_Mutex.Release;

      return Is_Empty;
   end Get_Playlist_Is_Empty;

   -------------------------------------------------------------------------------------------------
   -- T_Mutex
   -------------------------------------------------------------------------------------------------
   protected body T_Mutex is

      entry Seize when not Owned is
      begin
         Owned := True;
      end Seize;

      procedure Release is
      begin
         Owned := False;
      end Release;

   end T_Mutex;

end Room;
