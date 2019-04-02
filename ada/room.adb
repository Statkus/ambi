with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Net.Websocket.Registry;
with AWS.Session; use AWS.Session;

with Client; use Client;
with YT_API;

package body Room is

   procedure Free_Client is new Ada.Unchecked_Deallocation (T_Client'Class, T_Client_Class_Access);

   -------------------------------------------------------------------------------------------------
   -- T_Room_Sync_Task
   -------------------------------------------------------------------------------------------------
   task body T_Room_Sync_Task is
      Playlist_Empty : Boolean := False;

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/" & This.Get_Name & "Socket");
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
            -- Send request to the clients, the current room video has changed
            AWS.Net.WebSocket.Registry.Send (Rcp, "update_room_current_video_request");
            This.Last_Request_Time := Ada.Real_Time.Clock;

            exit when Playlist_Empty;

            -- Wait for the duration of the current video or next room video entry
            select
               accept Next_Room_Video;
            or
               delay Duration (YT_API.Get_Video_Duration (This.Get_Video));
            end select;

            -- Remove all expired sessions
            This.Remove_Disconnected_Client;

            if not This.Get_Playlist_Is_Empty then
               -- If the playlist is not empty, select the next video
               This.Set_Video (This.Get_Playlist_First);
               This.Playlist_Delete_First;

               -- Add the current video to the historic
               This.DB.Add_To_Room_Historic (This.Get_Name, This.Get_Video);
            else
               if This.Is_Client_Sync_And_Play then
                  -- There is at least one client sync with the room with the player activated, play
                  -- a video following Youtube suggestion
                  This.Set_Video
                    (This.Select_Random_Video (YT_API.Get_Videos_Related (This.Get_Video)));

                  if To_String (This.Get_Video.Video_ID) = "" then
                     -- There is no youtube suggestion, go back at waiting for the start of a new
                     -- playlist
                     This.Room_Current_Video_Active := False;
                     Playlist_Empty := True;
                  else
                     -- Add the current video to the historic
                     This.DB.Add_To_Room_Historic (This.Get_Name, This.Get_Video);
                  end if;
               else
                  -- The playlist is empty and there is no sync client, go back at waiting for the
                  -- start of a new playlist
                  This.Room_Current_Video_Active := False;
                  This.Set_Video
                    ((Video_ID        => To_Unbounded_String (""),
                      Video_Title     => To_Unbounded_String ("no video played"),
                      Video_Thumbnail => To_Unbounded_String ("")));

                  Playlist_Empty := True;
               end if;
            end if;

            This.Update_No_Player_Clients;
            This.Room_Next_Video_Ready := True;
         end loop;
      end loop;
   end T_Room_Sync_Task;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize
     (Name : in String; DB : in not null Database.T_Database_Class_Access)
     return T_Room_Class_Access is
      New_Room : constant T_Room_Class_Access :=
        new T_Room'(Name => To_Unbounded_String (Name), DB => DB, others => <>);
   begin
      DB.Add_To_Rooms (Name);

      New_Room.Set_Room_Sync_Task (new Room.T_Room_Sync_Task (New_Room));

      return New_Room;
   end New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Set_Room_Sync_Task
   -------------------------------------------------------------------------------------------------
   procedure Set_Room_Sync_Task
     (This : in out T_Room; Sync_Task : in not null T_Room_Sync_Task_Access) is
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
   procedure Add_Client (This : in out T_Room; Session_ID : in AWS.Session.ID) is
   begin
      -- Remove all expired sessions
      This.Remove_Disconnected_Client;

      -- Set a parameter to this session ID to register it, the parameter is a unique ID
      AWS.Session.Set (Session_ID, "ID", AWS.Session.Image (Session_ID));

      -- Add the new client to the list and set his session ID
      This.Client_List.Append (new Client.T_Client);
      This.Client_List.Last_Element.Set_Session_ID (Session_ID);

      This.Client_List.Last_Element.Set_Current_Video (This.Get_Video);
      This.Client_List.Last_Element.Set_Playlist (This.Get_Playlist);

      Put_Line ("Room " & This.Get_Name & ", new client " & AWS.Session.Image (Session_ID)
        & ", number of clients:" & This.Client_List.Length'Img);
   end Add_Client;

   -------------------------------------------------------------------------------------------------
   -- Set_Client_Last_Request_Time
   -------------------------------------------------------------------------------------------------
   procedure Set_Client_Last_Request_Time (This : in out T_Room; Session_ID : in AWS.Session.ID) is
   begin
      This.Find_Client_From_Session_ID (Session_ID).Set_Last_Request_Time;
   end Set_Client_Last_Request_Time;

   -------------------------------------------------------------------------------------------------
   -- Is_Registered
   -------------------------------------------------------------------------------------------------
   function Is_Registered (This : in out T_Room; Session_ID : in AWS.Session.ID) return Boolean is
     (This.Find_Client_From_Session_ID (Session_ID) /= null);

   -------------------------------------------------------------------------------------------------
   -- Add_Video_To_Playlists
   -------------------------------------------------------------------------------------------------
   procedure Add_Video_To_Playlists (This : in out T_Room; Video : in T_Video) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      -- Add the video to the room playlist for room sync
      if This.Get_Playlist_Is_Empty and not This.Room_Current_Video_Active then
         This.Room_Current_Video_Active := True;
         This.Set_Video (Video);

         -- Add the current video to the historic
         This.DB.Add_To_Room_Historic (This.Get_Name, Video);

         This.Room_Sync_Task.Start_Room_Playlist;
      else
         This.Playlist_Append (Video);
      end if;

      -- Add the video to all the clients playlist
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if not Client_Vectors.Element (Client_List_Cursor).Get_Display_Player
           or Client_Vectors.Element (Client_List_Cursor).Get_Sync_With_Room then
            -- If the client is sync with the room (sync player or no player) then sync it
            Client_Vectors.Element (Client_List_Cursor).Set_Current_Video (This.Get_Video);
            Client_Vectors.Element (Client_List_Cursor).Set_Playlist (This.Get_Playlist);
         else
            -- Otherwise only add the video to the playlist client
            Client_Vectors.Element (Client_List_Cursor).Add_Video_To_Playlist (Video);
         end if;

         Client_Vectors.Next (Client_List_Cursor);
      end loop;
   end Add_Video_To_Playlists;

   -------------------------------------------------------------------------------------------------
   -- Add_Like
   -------------------------------------------------------------------------------------------------
   procedure Add_Like (This : in out T_Room; Video : in T_Video) is
   begin
      This.DB.Add_To_Room_Likes (This.Get_Name, Video);
   end Add_Like;

   -------------------------------------------------------------------------------------------------
   -- Remove_Like
   -------------------------------------------------------------------------------------------------
   procedure Remove_Like (This : in out T_Room; Video : in T_Video) is
   begin
      This.DB.Remove_From_Room_Likes (This.Get_Name, Video);
   end Remove_Like;

   -------------------------------------------------------------------------------------------------
   -- Next_Room_Video
   -------------------------------------------------------------------------------------------------
   procedure Next_Room_Video (This : in out T_Room) is
   begin
      This.Room_Next_Video_Ready := not This.Room_Current_Video_Active;

      if This.Room_Current_Video_Active then
         This.Room_Sync_Task.Next_Room_Video;
      end if;
   end Next_Room_Video;

   -------------------------------------------------------------------------------------------------
   -- Next_Client_Video
   -------------------------------------------------------------------------------------------------
   procedure Next_Client_Video (This : in out T_Room; Session_ID : in AWS.Session.ID) is
      Current_Client : constant Client.T_Client_Class_Access :=
        This.Find_Client_From_Session_ID (Session_ID);
   begin
      if Current_Client.Get_Sync_With_Room then
         -- Synchronized the client playlist and current video with the room ones
         Current_Client.Set_Current_Video (This.Get_Video);
         Current_Client.Set_Playlist (This.Get_Playlist);
      else
         -- Set the first client video in the playlist as the current client video and remove
         -- it from the playlist
         Current_Client.Set_Current_Video;
         Current_Client.Remove_First_Playlist_Video;
      end if;
   end Next_Client_Video;

   -------------------------------------------------------------------------------------------------
   -- Set_Client_Display_Player
   -------------------------------------------------------------------------------------------------
   procedure Set_Client_Display_Player
     (This : in out T_Room; Session_ID : in AWS.Session.ID; Display : in Boolean) is
      Current_Client : constant Client.T_Client_Class_Access :=
        This.Find_Client_From_Session_ID (Session_ID);
   begin
      Current_Client.Set_Display_Player (Display);

      if not Display then
         -- Synchronized the client playlist and current video with the room ones
         Current_Client.Set_Sync_With_Room (True);
         Current_Client.Set_Current_Video (This.Get_Video);
         Current_Client.Set_Playlist (This.Get_Playlist);
      end if;

      -- Count number of sync clients
      This.Number_Of_Clients_Sync := This.Count_Number_Of_Clients_Sync;
   end Set_Client_Display_Player;

   -------------------------------------------------------------------------------------------------
   -- Set_Client_Sync_With_Room
   -------------------------------------------------------------------------------------------------
   procedure Set_Client_Sync_With_Room
     (This : in out T_Room; Session_ID : in AWS.Session.ID; Sync : in Boolean) is
      Current_Client : constant Client.T_Client_Class_Access :=
        This.Find_Client_From_Session_ID (Session_ID);
   begin
      Current_Client.Set_Sync_With_Room (Sync);

      if Sync then
         -- Synchronized the client playlist and current video with the room ones
         Current_Client.Set_Current_Video (This.Get_Video);
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
   -- Get_Current_Video
   -------------------------------------------------------------------------------------------------
   function Get_Current_Video (This : in out T_Room) return T_Video is (This.Get_Video);

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Video_Search_Results (This : in T_Room; Search_Input : in String)
     return Video_Vectors.Vector is (YT_API.Get_Video_Search_Results (Search_Input));

   -------------------------------------------------------------------------------------------------
   -- Get_Historic
   -------------------------------------------------------------------------------------------------
   function Get_Historic (This : in T_Room) return Video_Vectors.Vector is
     (This.DB.Get_Room_Historic (This.Get_Name));

   -------------------------------------------------------------------------------------------------
   -- Get_Likes
   -------------------------------------------------------------------------------------------------
   function Get_Likes (This : in T_Room) return Video_Vectors.Vector is
     (This.DB.Get_Room_Likes (This.Get_Name));

   -------------------------------------------------------------------------------------------------
   -- Is_Video_Liked
   -------------------------------------------------------------------------------------------------
   function Is_Video_Liked (This : in T_Room; Video : in T_Video) return Boolean is
     (This.DB.Is_Room_Video_Liked (This.Get_Name, Video));

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Client_Video
   -------------------------------------------------------------------------------------------------
   function Get_Current_Client_Video (This : in T_Room; Session_ID : in AWS.Session.ID)
     return T_Video is (This.Find_Client_From_Session_ID (Session_ID).Get_Current_Video);

   -------------------------------------------------------------------------------------------------
   -- Get_Client_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Client_Playlist (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Video_Vectors.Vector is
       (This.Find_Client_From_Session_ID (Session_ID).Get_Playlist);

   -------------------------------------------------------------------------------------------------
   -- Get_Client_Display_Player
   -------------------------------------------------------------------------------------------------
   function Get_Client_Display_Player (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Boolean is (This.Find_Client_From_Session_ID (Session_ID).Get_Display_Player);

   -------------------------------------------------------------------------------------------------
   -- Get_Client_Sync_With_Room
   -------------------------------------------------------------------------------------------------
   function Get_Client_Sync_With_Room (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Boolean is (This.Find_Client_From_Session_ID (Session_ID).Get_Sync_With_Room);

   -------------------------------------------------------------------------------------------------
   -- Client_Has_Nothing_To_Play
   -------------------------------------------------------------------------------------------------
   function Client_Has_Nothing_To_Play (This : in out T_Room; Session_ID : in AWS.Session.ID)
     return Boolean is
      Current_Client : constant Client.T_Client_Class_Access :=
        This.Find_Client_From_Session_ID (Session_ID);
      Nothing_To_Play : Boolean := False;
   begin
      if Current_Client.Get_Sync_With_Room then
         if To_String (This.Get_Video.Video_Title) = "no video played" then
            Nothing_To_Play := True;
         end if;
      else
         Nothing_To_Play := Current_Client.Has_Nothing_To_Play;
      end if;

      return Nothing_To_Play;
   end Client_Has_Nothing_To_Play;

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Clients_Sync
   -------------------------------------------------------------------------------------------------
   function Get_Number_Clients_Sync (This : in T_Room) return Natural is
     (This.Number_Of_Clients_Sync);

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Next_Video_Ready
   -------------------------------------------------------------------------------------------------
   function Get_Room_Next_Video_Ready (This : in T_Room) return Boolean is
     (This.Room_Next_Video_Ready);

   -------------------------------------------------------------------------------------------------
   -- Update_No_Player_Clients
   -------------------------------------------------------------------------------------------------
   procedure Update_No_Player_Clients (This : in out T_Room) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if not Client_Vectors.Element (Client_List_Cursor).Get_Display_Player then
            Client_Vectors.Element (Client_List_Cursor).Set_Current_Video (This.Get_Video);
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
      Number_Of_Clients_Sync : Natural := 0;
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
   function Find_Client_From_Session_ID (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Client.T_Client_Class_Access is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
      Client_To_Find     : Client.T_Client_Class_Access := null;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) and Client_To_Find = null loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Session_ID = Session_ID then
            Client_To_Find:= Client_Vectors.Element (Client_List_Cursor);
         end if;

         Client_Vectors.Next (Client_List_Cursor);
      end loop;

      return Client_To_Find;
   end Find_Client_From_Session_ID;

   -------------------------------------------------------------------------------------------------
   -- Is_Client_Sync_And_Play
   -------------------------------------------------------------------------------------------------
   function Is_Client_Sync_And_Play (This : in out T_Room) return Boolean is
      Client_List_Cursor   : Client_Vectors.Cursor := This.Client_List.First;
      Client_Sync_And_Play : Boolean := False;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) and not Client_Sync_And_Play loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Sync_With_Room
           and Client_Vectors.Element (Client_List_Cursor).Get_Display_Player then
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

      Number_Of_Clients_Sync : Natural := 0;

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/" & This.Get_Name & "Socket");
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if not AWS.Session.Exist (Client_Vectors.Element (Client_List_Cursor).Get_Session_ID)
           or (This.Last_Request_Time
               > Client_Vectors.Element (Client_List_Cursor).Get_Last_Request_Time
               and then Ada.Real_Time.To_Duration (This.Last_Request_Time
               - Client_Vectors.Element (Client_List_Cursor).Get_Last_Request_Time) > 120.0) then
            Client_To_Remove := Client_Vectors.Element (Client_List_Cursor);
            This.Client_List.Delete (Client_List_Cursor);

            Put_Line ("Room " & This.Get_Name & ", remove client "
              & AWS.Session.Image (Client_To_Remove.Get_Session_ID) & ", number of clients:"
              & This.Client_List.Length'Img);

            Free_Client (Client_To_Remove);

            Client_List_Cursor := This.Client_List.First;
         end if;

         Client_Vectors.Next (Client_List_Cursor);
      end loop;

      Number_Of_Clients_Sync := This.Count_Number_Of_Clients_Sync;

      if This.Number_Of_Clients_Sync /= Number_Of_Clients_Sync then
         This.Number_Of_Clients_Sync := Number_Of_Clients_Sync;

         -- Send update request for the number of clients sync
         AWS.Net.WebSocket.Registry.Send (Rcp, "update_nb_clients_sync");
      end if;
   end Remove_Disconnected_Client;

   -------------------------------------------------------------------------------------------------
   -- Select_Random_Video
   -------------------------------------------------------------------------------------------------
   function Select_Random_Video (This : in T_Room; Videos : in Video_Vectors.Vector)
     return T_Video is
      Video_Index : Natural := Natural'First;
      Video       : T_Video :=
        (Video_ID        => To_Unbounded_String (""),
         Video_Title     => To_Unbounded_String ("no video played"),
         Video_Thumbnail => To_Unbounded_String (""));
   begin
      if Natural (Videos.Length) > 0 then
         Video_Index :=
           Natural (Ada.Numerics.Float_Random.Random (This.RNG) * (Float (Videos.Length) - 1.0));
      end if;

      if Video_Vectors.Has_Element (Videos.To_Cursor (Video_Index)) then
         Video := Videos.Element (Video_Index);
      end if;

      return Video;
   end Select_Random_Video;

   -------------------------------------------------------------------------------------------------
   -- Set_Video
   -------------------------------------------------------------------------------------------------
   procedure Set_Video (This : in out T_Room; Video : in T_Video) is
   begin
      This.Room_Video_Playlist_Mutex.Seize;
      This.Room_Current_Video := Video;
      This.Room_Video_Playlist_Mutex.Release;
   end Set_Video;

   -------------------------------------------------------------------------------------------------
   -- Playlist_Append
   -------------------------------------------------------------------------------------------------
   procedure Playlist_Append (This : in out T_Room; Video : in T_Video) is
   begin
      This.Room_Video_Playlist_Mutex.Seize;
      This.Room_Playlist.Append (Video);
      This.Room_Video_Playlist_Mutex.Release;
   end Playlist_Append;

   -------------------------------------------------------------------------------------------------
   -- Playlist_Delete_First
   -------------------------------------------------------------------------------------------------
   procedure Playlist_Delete_First (This : in out T_Room) is
   begin
      This.Room_Video_Playlist_Mutex.Seize;
      This.Room_Playlist.Delete_First;
      This.Room_Video_Playlist_Mutex.Release;
   end Playlist_Delete_First;

   -------------------------------------------------------------------------------------------------
   -- Get_Video
   -------------------------------------------------------------------------------------------------
   function Get_Video (This : in out T_Room) return T_Video is
      Video : T_Video;
   begin
      This.Room_Video_Playlist_Mutex.Seize;
      Video := This.Room_Current_Video;
      This.Room_Video_Playlist_Mutex.Release;

      return Video;
   end Get_Video;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (This : in out T_Room) return Video_Vectors.Vector is
      Room_Playlist : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
   begin
      This.Room_Video_Playlist_Mutex.Seize;
      Room_Playlist := This.Room_Playlist;
      This.Room_Video_Playlist_Mutex.Release;

      return Room_Playlist;
   end Get_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist_First
   -------------------------------------------------------------------------------------------------
   function Get_Playlist_First (This : in out T_Room) return T_Video is
      Video : T_Video;
   begin
      This.Room_Video_Playlist_Mutex.Seize;
      Video := Video_Vectors.Element (This.Room_Playlist.First);
      This.Room_Video_Playlist_Mutex.Release;

      return Video;
   end Get_Playlist_First;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist_Is_Empty
   -------------------------------------------------------------------------------------------------
   function Get_Playlist_Is_Empty (This : in out T_Room) return Boolean is
      Is_Empty : Boolean;
   begin
      This.Room_Video_Playlist_Mutex.Seize;
      Is_Empty := This.Room_Playlist.Is_Empty;
      This.Room_Video_Playlist_Mutex.Release;

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
