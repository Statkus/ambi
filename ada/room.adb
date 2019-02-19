with AWS.Session; use AWS.Session;

with Ada.Text_IO; use Ada.Text_IO;

package body Room is

   -------------------------------------------------------------------------------------------------
   -- Client_Compare
   -------------------------------------------------------------------------------------------------
   function Client_Compare (Left, Right : Client.T_Client_Class_Access) return Boolean is (False);

   -------------------------------------------------------------------------------------------------
   -- Add_Client
   -------------------------------------------------------------------------------------------------
   procedure Add_Client (This : in out T_Room; Session_ID : in AWS.Session.ID) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
      New_Client         : Boolean := True;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) and New_Client loop
         if Client_Vectors.Element (Client_List_Cursor).Get_Session_ID = Session_ID then
            New_Client := False;
         end if;

         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;

      if New_Client then
         Put_Line ("New client: " & AWS.Session.Image (Session_ID));

         -- Set a parameter to this session ID to register it, the parameter is a unique ID
         AWS.Session.Set (Session_ID, "ID", This.Client_ID_Counter);
         This.Client_ID_Counter := This.Client_ID_Counter + 1;

         -- Add the new client to the list and set his session ID
         This.Client_List.Append (new Client.T_Client);
         This.Client_List.Last_Element.Set_Session_ID (Session_ID);

         Put_Line ("Internal ID:" & Integer'Image (AWS.Session.Get (This.Client_List.Last_Element.Get_Session_ID, "ID")));
      end if;
   end Add_Client;

   -------------------------------------------------------------------------------------------------
   -- Add_Video_To_Clients_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Add_Video_To_Clients_Playlist (This : in out T_Room; Video : in YT_API.T_Video) is
      Client_List_Cursor : Client_Vectors.Cursor := This.Client_List.First;
   begin
      while Client_Vectors.Has_Element (Client_List_Cursor) loop
         Client_Vectors.Element (Client_List_Cursor).Add_Video_To_Playlist (Video);

         Client_List_Cursor := Client_Vectors.Next (Client_List_Cursor);
      end loop;
   end Add_Video_To_Clients_Playlist;

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
   -- Get_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Video_Search_Results (This : in T_Room) return YT_API.T_Video_Search_Results is
     (This.Video_Search_Results);

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
