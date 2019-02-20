with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with AWS.MIME;
with AWS.Net.Websocket.Registry;
with AWS.Parameters;
with AWS.Session;

with Templates_Parser;

package body Callback is

   -------------------------------------------------------------------------------------------------
   -- Set_Server_Address
   -------------------------------------------------------------------------------------------------
   procedure Set_Server_Address (Address : in String) is
   begin
      SERVER_ADDRESS := To_Unbounded_String (Address);
   end Set_Server_Address;

   -------------------------------------------------------------------------------------------------
   -- Create_Room
   -------------------------------------------------------------------------------------------------
   procedure Create_Room is
   begin
      Current_Room := new Room.T_Room;
      Current_Room.Set_Room_Sync_Task (new Room.T_Room_Sync_Task (Current_Room));
   end Create_Room;

   -------------------------------------------------------------------------------------------------
   -- Ambi_Callback
   -------------------------------------------------------------------------------------------------
   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      URI        : constant String := AWS.Status.URI (Request);
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);

      Response : AWS.Response.Data;
   begin
      Current_Room_Mutex.Seize;

      if URI = "/" then
         if not Current_Room.Is_Registered (Session_ID) then
            Current_Room.Add_Client (Session_ID);
         end if;

         Put_Line ("Client ID:" & Integer'Image (AWS.Session.Get (Session_ID, "ID"))
           & ", request: '" & URI & "'");

         Response := Room_Callback (Request);
      elsif Current_Room.Is_Registered (Session_ID) then
         Put_Line ("Client ID:" & Integer'Image (AWS.Session.Get (Session_ID, "ID"))
           & ", request: '" & URI & "'");

         if Index (URI, "/javascripts/") > 0 then
            Response := Javascripts_Callback (Request);
         elsif URI = "/onclick$search_button" then
            Response := Search_Button_Callback (Request);
         elsif URI = "/onclick$search_results_list" then
            Response := Search_Result_Callback (Request);
         elsif URI = "/next_video" then
            Response := Next_Video_Callback (Request);
         elsif URI = "/get_playlist" then
            Response := Get_Playlist_Callback (Request);
         elsif URI = "/get_current_room_video" then
            Response := Get_Current_Room_Video_Callback (Request);
         else
            Put_Line ("Not supported request: '" & URI & "'");
            Response := AWS.Response.Build (AWS.MIME.Text_HTML, "");
         end if;
      else
         Put_Line ("Not registered session ID and not supported request: '" & URI & "'");
         Response := AWS.Response.Build (AWS.MIME.Text_HTML, "");
      end if;

      Current_Room_Mutex.Release;

      return Response;
   end Ambi_Callback;

   -------------------------------------------------------------------------------------------------
   -- Room_Callback
   -------------------------------------------------------------------------------------------------
   function Room_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);

      Translations : Templates_Parser.Translate_Table (1 .. 4);
   begin
      Put_Line ("Video to play: "
        & To_String (Current_Room.Get_Current_Client_Video (Session_ID).Video_Title));

      Translations (1) := Templates_Parser.Assoc
        ("ROOM_VIDEO", To_String (Current_Room.Get_Current_Video.Video_Title));

      Translations (2) := Templates_Parser.Assoc
        ("PLAYLIST", Build_Playlist (Current_Room.Get_Client_Playlist (Session_ID)));

      Translations (3) := Templates_Parser.Assoc
        ("SERVER_ADDRESS", To_String (SERVER_ADDRESS));

      Translations (4) := Templates_Parser.Assoc
        ("VIDEO_ID", To_String (Current_Room.Get_Current_Client_Video (Session_ID).Video_ID));

      return AWS.Response.Build
        (AWS.MIME.Text_HTML, To_String (Templates_Parser.Parse ("html/ambi.thtml", Translations)));
   end Room_Callback;

   -------------------------------------------------------------------------------------------------
   -- Javascripts_Callback
   -------------------------------------------------------------------------------------------------
   function Javascripts_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      return AWS.Response.File (AWS.MIME.Text_Javascript, URI (URI'First + 1 .. URI'Last));
   end Javascripts_Callback;

   -------------------------------------------------------------------------------------------------
   -- Search_Button_Callback
   -------------------------------------------------------------------------------------------------
   function Search_Button_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Parameters   : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Search_Input : constant String := AWS.Parameters.Get (Parameters, "search_input");
   begin
      Current_Room.Set_Video_Search_Results (YT_API.Get_Video_Search_Results (Search_Input));

      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
          ("search_results_list", Build_Search_Result (Current_Room.Get_Video_Search_Results)));
   end Search_Button_Callback;

   -------------------------------------------------------------------------------------------------
   -- Search_Result_Callback
   -------------------------------------------------------------------------------------------------
   function Search_Result_Callback(Request : in AWS.Status.Data) return AWS.Response.Data is
      Parameters  : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Item_Number : constant Integer := Integer'Value (AWS.Parameters.Get (Parameters, "item"));

      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);

      Rcp : AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/socket");
   begin
      Current_Room.Add_Video_To_Playlists
        (Current_Room.Get_Video_Search_Results (Item_Number));

      AWS.Net.WebSocket.Registry.Send (Rcp, "update_client_playlist_request");

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Search_Result_Callback;

   -------------------------------------------------------------------------------------------------
   -- Next_Video_Callback
   -------------------------------------------------------------------------------------------------
   function Next_Video_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);
   begin
      Current_Room.Set_Current_Client_Video (Session_ID);
      Current_Room.Remove_First_Client_Playlist_Video (Session_ID);

      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
          ("playlist", Build_Playlist (Current_Room.Get_Client_Playlist (Session_ID))));
   end Next_Video_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Playlist_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);
   begin
      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
          ("playlist", Build_Playlist (Current_Room.Get_Client_Playlist (Session_ID))));
   end Get_Playlist_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Room_Video_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Current_Room_Video_Callback (Request : in AWS.Status.Data)
     return AWS.Response.Data is
   begin
      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
          ("current_room_video", To_String (Current_Room.Get_Current_Video.Video_Title)));
   end Get_Current_Room_Video_Callback;

   -------------------------------------------------------------------------------------------------
   -- Build_Search_Result
   -------------------------------------------------------------------------------------------------
   function Build_Search_Result (Video_Search_Results : in YT_API.T_Video_Search_Results)
     return String is
      Translations : Templates_Parser.Translate_Table (1 .. 3);

      Response : Unbounded_String := To_Unbounded_String ("<ul>");
   begin
      for Result_Index in Video_Search_Results'Range loop
         Translations (1) := Templates_Parser.Assoc
           ("ITEM_ID", Trim (Result_Index'Img, Ada.Strings.Left));

         Translations (2) := Templates_Parser.Assoc
           ("IMAGE_URL", To_String (Video_Search_Results (Result_Index).Video_Image_URL));

         Translations (3) := Templates_Parser.Assoc
           ("VIDEO_TITLE", To_String (Video_Search_Results (Result_Index).Video_Title));

         Append (Response, To_String
           (Templates_Parser.Parse ("html/search_results_list_item.thtml", Translations)));
      end loop;

      Append (Response, "</ul>");

      return To_String (Response);
   end Build_Search_Result;

   -------------------------------------------------------------------------------------------------
   -- Build_Playlist
   -------------------------------------------------------------------------------------------------
   function Build_Playlist (Current_Playlist : in Playlist.Video_Vectors.Vector) return String is
      Translations : Templates_Parser.Translate_Table (1 .. 3);

      Response : Unbounded_String := To_Unbounded_String ("<ul>");

      Playlist_Cursor : Playlist.Video_Vectors.Cursor := Current_Playlist.First;
   begin
      while Playlist.Video_Vectors.Has_Element (Playlist_Cursor) loop
         Translations (1) := Templates_Parser.Assoc
           ("ITEM_ID", Trim
             (Integer'Image (Playlist.Video_Vectors.To_Index (Playlist_Cursor)), Ada.Strings.Left));

         Translations (2) := Templates_Parser.Assoc
           ("IMAGE_URL", Playlist.Video_Vectors.Element (Playlist_Cursor).Video_Image_URL);

         Translations (3) := Templates_Parser.Assoc
           ("VIDEO_TITLE", Playlist.Video_Vectors.Element (Playlist_Cursor).Video_Title);

         Append (Response, To_String
           (Templates_Parser.Parse ("html/playlist_item.thtml", Translations)));

         Playlist_Cursor := Playlist.Video_Vectors.Next (Playlist_Cursor);
      end loop;

      Append (Response, "</ul>");

      return To_String (Response);
   end Build_Playlist;

   function Pack_AJAX_XML_Response (Placeholder : in String; Value : in String) return String is
      Translations : Templates_Parser.Translate_Table (1 .. 2);

      Replace_Fields : Unbounded_String;
      Response       : Unbounded_String;
   begin
      -- Pack <replace> fields
      -- For now, only one field at a time is supported
      Translations (1) := Templates_Parser.Assoc ("PLACEHOLDER", Placeholder);
      Translations (2) := Templates_Parser.Assoc ("VALUE", Value);

      Replace_Fields := To_Unbounded_String
        (Templates_Parser.Parse ("xml/ajax_xml_replace.txml", Translations));

      -- Pack global response
      Translations (1) := Templates_Parser.Assoc ("ACTION_FIELDS", Replace_Fields);

      return Templates_Parser.Parse ("xml/ajax_xml_response.txml", Translations);
   end Pack_AJAX_XML_Response;

   protected body Mutex is

      entry Seize when not Owned is
      begin
         Owned := True;
      end Seize;

      procedure Release is
      begin
         Owned := False;
      end Release;

   end Mutex;

end Callback;