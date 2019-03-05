with AWS.Net.Websocket.Registry;
with AWS.Session; use AWS.Session;

with Ada.Text_IO; use Ada.Text_IO;

with Client; use Client;

package body Room is

   -------------------------------------------------------------------------------------------------
   -- T_Room_Sync_Task
   -------------------------------------------------------------------------------------------------
   task body T_Room_Sync_Task is
      Playlist_Empty : Boolean := False;

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/socket");
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

            exit when Playlist_Empty;

            -- Wait for the duration of the current video
            delay Duration (YT_API.Get_Video_Duration (This.Room_Current_Video));

            This.Room_Current_Video_Mutex.Seize;

            if not This.Room_Playlist.Is_Empty then
               -- If the playlist is not empty, select the next video
               This.Room_Current_Video := Playlist.Video_Vectors.Element (This.Room_Playlist.First);
               This.Room_Playlist.Delete_First;

               -- Add the current video to the historic
               This.DB.Add_To_Historic (This.Room_Current_Video);
            else
               -- The playlist is empty, go back at waiting for the start of a new playlist
               This.Room_Current_Video_Active := False;
               This.Room_Current_Video.Video_Title := To_Unbounded_String ("no video played");
               This.Room_Current_Video.Video_ID := To_Unbounded_String ("");

               Playlist_Empty := True;
            end if;

            This.Room_Current_Video_Mutex.Release;

            This.Update_No_Player_Clients;
         end loop;
      end loop;
   end T_Room_Sync_Task;

   -------------------------------------------------------------------------------------------------
   -- Client_Compare
   -------------------------------------------------------------------------------------------------
   function Client_Compare (Left, Right : Client.T_Client_Class_Access) return Boolean is (False);

   -------------------------------------------------------------------------------------------------
   -- Set_Database
   -------------------------------------------------------------------------------------------------
   procedure Set_Database
     (This : in out T_Room; DB : in not null Database.T_Database_Class_Access) is
   begin
      This.DB := DB;
   end Set_Database;

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
      -- Set a parameter to this session ID to register it, the parameter is a unique ID
      AWS.Session.Set (Session_ID, "ID", This.Client_ID_Counter);
      This.Client_ID_Counter := This.Client_ID_Counter + 1;

      -- Add the new client to the list and set his session ID
      This.Client_List.Append (new Client.T_Client);
      This.Client_List.Last_Element.Set_Session_ID (Session_ID);

      This.Room_Current_Video_Mutex.Seize;

      This.Client_List.Last_Element.Set_Playlist (This.Room_Playlist);
      This.Client_List.Last_Element.Set_Current_Video (This.Room_Current_Video);

      This.Room_Current_Video_Mutex.Release;

      Put_Line ("New client: " & AWS.Session.Image (Session_ID) & ", internal ID:"
        & Integer'Image (AWS.Session.Get (This.Client_List.Last_Element.Get_Session_ID, "ID")));
   end Add_Client;

   -------------------------------------------------------------------------------------------------
   -- Is_Registered
   -------------------------------------------------------------------------------------------------
   function Is_Registered (This : in out T_Room; Session_ID : in AWS.Session.ID) return Boolean is
     (This.Find_Client_From_Session_ID (Session_ID) /= null);

   -------------------------------------------------------------------------------------------------
   -- Add_Video_To_Playlists
   -------------------------------------------------------------------------------------------------
   procedure Add_Video_To_Playlists (This : in out T_Room; Video : in YT_API.T_Video) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      -- Add the video to the room playlist for room sync
      This.Room_Current_Video_Mutex.Seize;
      if This.Room_Playlist.Is_Empty and not This.Room_Current_Video_Active then
         This.Room_Current_Video_Active := True;
         This.Room_Current_Video := Video;

         This.Room_Sync_Task.Start_Room_Playlist;

         -- Add the current video to the historic
         This.DB.Add_To_Historic (Video);
      else
         This.Room_Playlist.Append (Video);
      end if;
      This.Room_Current_Video_Mutex.Release;

      -- Add the video to all the clients playlist
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if not Client_Vectors.Element (Client_List_Cursor).Get_Display_Player
           or Client_Vectors.Element (Client_List_Cursor).Get_Sync_With_Room then
            -- If the client is sync with the room (sync player or no player) then sync it
            This.Room_Current_Video_Mutex.Seize;
            Client_Vectors.Element (Client_List_Cursor).Set_Current_Video (This.Room_Current_Video);
            Client_Vectors.Element (Client_List_Cursor).Set_Playlist (This.Room_Playlist);
            This.Room_Current_Video_Mutex.Release;
         else
            -- Otherwise only add the video to the playlist client
            Client_Vectors.Element (Client_List_Cursor).Add_Video_To_Playlist (Video);
         end if;

         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;
   end Add_Video_To_Playlists;

   -------------------------------------------------------------------------------------------------
   -- Next_Client_Video
   -------------------------------------------------------------------------------------------------
   procedure Next_Client_Video (This : in out T_Room; Session_ID : in AWS.Session.ID) is
      Current_Client : constant Client.T_Client_Class_Access :=
        This.Find_Client_From_Session_ID (Session_ID);
   begin
      if Current_Client.Get_Sync_With_Room then
         -- Synchronized the client playlist and current video with the room ones
         This.Room_Current_Video_Mutex.Seize;
         Current_Client.Set_Playlist (This.Room_Playlist);
         Current_Client.Set_Current_Video (This.Room_Current_Video);
         This.Room_Current_Video_Mutex.Release;
      else
         -- Set the first client video in the playlist as the current client video and remove
         -- it from the playlist
         Current_Client.Set_Current_Video;
         Current_Client.Remove_First_Playlist_Video;
      end if;
   end Next_Client_Video;

   -------------------------------------------------------------------------------------------------
   -- Set_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   procedure Set_Video_Search_Results
     (This : in out T_Room; Video_Search_Results : in YT_API.T_Video_Search_Results) is
   begin
      This.Video_Search_Results := Video_Search_Results;
   end Set_Video_Search_Results;

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
         This.Room_Current_Video_Mutex.Seize;
         Current_Client.Set_Playlist (This.Room_Playlist);
         Current_Client.Set_Current_Video (This.Room_Current_Video);
         This.Room_Current_Video_Mutex.Release;
      end if;
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
         This.Room_Current_Video_Mutex.Seize;
         Current_Client.Set_Playlist (This.Room_Playlist);
         Current_Client.Set_Current_Video (This.Room_Current_Video);
         This.Room_Current_Video_Mutex.Release;
      end if;
   end Set_Client_Sync_With_Room;

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Video
   -------------------------------------------------------------------------------------------------
   function Get_Current_Video (This : in T_Room) return YT_API.T_Video is (This.Room_Current_Video);

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Video_Search_Results (This : in T_Room) return YT_API.T_Video_Search_Results is
     (This.Video_Search_Results);

   -------------------------------------------------------------------------------------------------
   -- Get_Historic
   -------------------------------------------------------------------------------------------------
   function Get_Historic (This : in T_Room) return Playlist.Video_Vectors.Vector is
     (This.DB.Get_Historic);

   -------------------------------------------------------------------------------------------------
   -- Get_Historic_Item
   -------------------------------------------------------------------------------------------------
   function Get_Historic_Item (This : in T_Room; Item_Number : in Natural) return YT_API.T_Video is
     (This.DB.Get_Historic.Element (Item_Number));

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Client_Video
   -------------------------------------------------------------------------------------------------
   function Get_Current_Client_Video (This : in T_Room; Session_ID : in AWS.Session.ID)
     return YT_API.T_Video is (This.Find_Client_From_Session_ID (Session_ID).Get_Current_Video);

   -------------------------------------------------------------------------------------------------
   -- Get_Client_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Client_Playlist (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Playlist.Video_Vectors.Vector is
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
   function Client_Has_Nothing_To_Play (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Boolean is
      Current_Client : constant Client.T_Client_Class_Access :=
        This.Find_Client_From_Session_ID (Session_ID);
      Nothing_To_Play : Boolean := False;
   begin
      if Current_Client.Get_Sync_With_Room then
         if To_String (This.Room_Current_Video.Video_Title) = "no video played" then
            Nothing_To_Play := True;
         end if;
      else
         Nothing_To_Play := Current_Client.Has_Nothing_To_Play;
      end if;

      return Nothing_To_Play;
   end Client_Has_Nothing_To_Play;

   -------------------------------------------------------------------------------------------------
   -- Update_No_Player_Clients
   -------------------------------------------------------------------------------------------------
   procedure Update_No_Player_Clients (This : in out T_Room) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if not Client_Vectors.Element (Client_List_Cursor).Get_Display_Player then
            This.Room_Current_Video_Mutex.Seize;
            Client_Vectors.Element (Client_List_Cursor).Set_Current_Video (This.Room_Current_Video);
            Client_Vectors.Element (Client_List_Cursor).Set_Playlist (This.Room_Playlist);
            This.Room_Current_Video_Mutex.Release;
         end if;

         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;
   end Update_No_Player_Clients;

   -------------------------------------------------------------------------------------------------
   -- Find_Client_From_Session_ID
   -------------------------------------------------------------------------------------------------
   function Find_Client_From_Session_ID (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Client.T_Client_Class_Access is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
      Client_To_Find     : Client.T_Client_Class_Access := null;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Session_ID = Session_ID then
            Client_To_Find:= Client_Vectors.Element (Client_List_Cursor);
            exit;
         end if;

         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;

      return Client_To_Find;
   end Find_Client_From_Session_ID;

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
