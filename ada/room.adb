with AWS.Net.Websocket.Registry;
with AWS.Session; use AWS.Session;

with Ada.Text_IO; use Ada.Text_IO;

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

            if not This.Room_Playlist.Is_Empty then
               -- If the playlist is not empty, select the next video
               This.Room_Current_Video := Playlist.Video_Vectors.Element (This.Room_Playlist.First);
               This.Room_Playlist.Delete_First;
            else
               -- The playlist is empty, go back at waiting for the start of a new playlist
               This.Room_Current_Video_Active := False;
               This.Room_Current_Video.Video_Title := To_Unbounded_String ("no video played");

               Playlist_Empty := True;
            end if;
         end loop;
      end loop;
   end T_Room_Sync_Task;

   -------------------------------------------------------------------------------------------------
   -- Client_Compare
   -------------------------------------------------------------------------------------------------
   function Client_Compare (Left, Right : Client.T_Client_Class_Access) return Boolean is (False);

   -------------------------------------------------------------------------------------------------
   -- Set_Room_Sync_Task
   -------------------------------------------------------------------------------------------------
   procedure Set_Room_Sync_Task (This : in out T_Room; Sync_Task : in T_Room_Sync_Task_Access) is
   begin
      This.Room_Sync_Task := Sync_Task;
   end Set_Room_Sync_Task;

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

      This.Client_List.Last_Element.Set_Playlist (This.Room_Playlist);
      This.Client_List.Last_Element.Set_Current_Video (This.Room_Current_Video);

      Put_Line ("New client: " & AWS.Session.Image (Session_ID) & ", internal ID:"
        & Integer'Image (AWS.Session.Get (This.Client_List.Last_Element.Get_Session_ID, "ID")));
   end Add_Client;

   -------------------------------------------------------------------------------------------------
   -- Is_Registered
   -------------------------------------------------------------------------------------------------
   function Is_Registered (This : in out T_Room; Session_ID : in AWS.Session.ID) return Boolean is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
      Registered         : Boolean := False;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) and not Registered loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Session_ID = Session_ID then
            Registered := True;
         end if;

         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;

      return Registered;
   end Is_Registered;

   -------------------------------------------------------------------------------------------------
   -- Add_Video_To_Playlists
   -------------------------------------------------------------------------------------------------
   procedure Add_Video_To_Playlists (This : in out T_Room; Video : in YT_API.T_Video) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      -- Add the video to all the clients playlist
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         Client_Vectors.Element (Client_List_Cursor).Add_Video_To_Playlist (Video);
         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;

      -- Also add the video to the room playlist for room sync
      if This.Room_Playlist.Is_Empty and not This.Room_Current_Video_Active then
         This.Room_Current_Video_Active := True;
         This.Room_Current_Video := Video;
         This.Room_Sync_Task.Start_Room_Playlist;
      else
         This.Room_Playlist.Append (Video);
      end if;
   end Add_Video_To_Playlists;

   -------------------------------------------------------------------------------------------------
   -- Remove_First_Client_Playlist_Video
   -------------------------------------------------------------------------------------------------
   procedure Remove_First_Client_Playlist_Video
     (This : in out T_Room; Session_ID : in AWS.Session.ID) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Session_ID = Session_ID then
            Client_Vectors.Element (Client_List_Cursor).Remove_First_Playlist_Video;
            exit;
         end if;

         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;
   end Remove_First_Client_Playlist_Video;

   -------------------------------------------------------------------------------------------------
   -- Set_Current_Client_Video
   -------------------------------------------------------------------------------------------------
   procedure Set_Current_Client_Video (This : in out T_Room; Session_ID : in AWS.Session.ID) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Session_ID = Session_ID then
            Client_Vectors.Element (Client_List_Cursor).Set_Current_Video;
            exit;
         end if;

         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;
   end Set_Current_Client_Video;

   -------------------------------------------------------------------------------------------------
   -- Set_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   procedure Set_Video_Search_Results
     (This : in out T_Room; Video_Search_Results : in YT_API.T_Video_Search_Results) is
   begin
      This.Video_Search_Results := Video_Search_Results;
   end Set_Video_Search_Results;

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
   -- Get_Current_Client_Video
   -------------------------------------------------------------------------------------------------
   function Get_Current_Client_Video (This : in T_Room; Session_ID : in AWS.Session.ID)
     return YT_API.T_Video is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Session_ID = Session_ID then
            exit;
         end if;

         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;

      return Client_Vectors.Element (Client_List_Cursor).Get_Current_Video;
   end Get_Current_Client_Video;

   -------------------------------------------------------------------------------------------------
   -- Get_Client_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Client_Playlist (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Playlist.Video_Vectors.Vector is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Session_ID = Session_ID then
            exit;
         end if;

         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;

      return Client_Vectors.Element (Client_List_Cursor).Get_Playlist;
   end Get_Client_Playlist;

end Room;
