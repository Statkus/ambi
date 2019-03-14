with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with AWS.MIME;
with AWS.Net.Websocket.Registry;
with AWS.Parameters;

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
   procedure Create_Room (DB : in not null Database.T_Database_Class_Access) is
   begin
      Current_Room := new Room.T_Room;
      Current_Room.Set_Database (DB);
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
      Current_Room.Lock;

      if URI = "/" then
         if not Current_Room.Is_Registered (Session_ID) then
            Current_Room.Add_Client (Session_ID);
         end if;

         Put_Line ("Client: " & AWS.Session.Image (Session_ID)
           & ", request: '" & URI & "'");

         Response := Room_Callback (Request);
      elsif Current_Room.Is_Registered (Session_ID) then
         Put_Line ("Client: " & AWS.Session.Image (Session_ID)
           & ", request: '" & URI & "'");

         -- Update the session life start time and client last request time
         AWS.Session.Touch (Session_ID);
         Current_Room.Set_Client_Last_Request_Time (Session_ID);

         if Index (URI, "/javascripts/") > 0 then
            Response := Javascripts_Callback (Request);
         elsif Index (URI, "/css/") > 0 then
            Response := CSS_Callback (Request);
         elsif URI = "/onclick$search_button" then
            Response := Search_Button_Callback (Request);
         elsif URI = "/onclick$add_to_playlist" then
            Response := Add_To_Playlist_Callback (Request);
         elsif URI = "/onclick$add_remove_like" then
            Response := Add_Remove_Like_Callback (Request);
         elsif URI = "/onclick$player_display_checkbox" then
            Response := Player_Display_Checkbox_Callback (Request);
         elsif URI = "/onclick$player_sync_checkbox" then
            Response := Player_Sync_Checkbox_Callback (Request);
         elsif URI = "/next_video" then
            Response := Next_Video_Callback (Request);
         elsif URI = "/get_video_list" then
            Response := Get_Video_List_Callback (Request);
         elsif URI = "/get_current_room_video" then
            Response := Get_Current_Room_Video_Callback;
         else
            Put_Line ("Not supported request: '" & URI & "'");
            Response := AWS.Response.Build (AWS.MIME.Text_HTML, "");
         end if;
      else
         Put_Line ("Not registered session ID and not supported request: '" & URI & "'");
         Response := AWS.Response.Build (AWS.MIME.Text_HTML, "");
      end if;

      Current_Room.Unlock;

      return Response;
   end Ambi_Callback;

   -------------------------------------------------------------------------------------------------
   -- Room_Callback
   -------------------------------------------------------------------------------------------------
   function Room_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);

      Translations : Templates_Parser.Translate_Table (1 .. 6);
      YT_Player_Translations : Templates_Parser.Translate_Table (1 .. 1);
   begin
      Put_Line ("Video to play: "
        & To_String (Current_Room.Get_Current_Client_Video (Session_ID).Video_Title));

      -- Sync checkbox displaying and player state
      if Current_Room.Get_Client_Display_Player (Session_ID) then
         Translations (1) := Templates_Parser.Assoc ("DISPLAY_SYNC_CHECKBOX", "inline-block");
         Translations (5) := Templates_Parser.Assoc ("PLAYER_STATE", "end");
      else
         Translations (1) := Templates_Parser.Assoc ("DISPLAY_SYNC_CHECKBOX", "none");
         Translations (5) := Templates_Parser.Assoc ("PLAYER_STATE", "no_player");
      end if;

      -- Current room video title
      Translations (2) := Templates_Parser.Assoc
        ("ROOM_VIDEO", To_String (Current_Room.Get_Current_Video.Video_Title));

      -- Client playlist
      Translations (3) := Templates_Parser.Assoc
        ("VIDEO_LIST", Build_Video_List (Session_ID, Playlist));

      -- Server address for WebSocket
      Translations (4) := Templates_Parser.Assoc ("SERVER_ADDRESS", To_String (SERVER_ADDRESS));

      -- Player script
      if Current_Room.Get_Client_Display_Player (Session_ID)
        and not Current_Room.Client_Has_Nothing_To_Play (Session_ID) then
         YT_Player_Translations (1) := Templates_Parser.Assoc
           ("VIDEO_ID", To_String (Current_Room.Get_Current_Client_Video (Session_ID).Video_ID));

         Translations (6) := Templates_Parser.Assoc
           ("YOUTUBE_PLAYER_SCRIPT",
            To_String
              (Templates_Parser.Parse ("javascripts/youtube_player.tjs", YT_Player_Translations)));
      else
         Translations (6) := Templates_Parser.Assoc ("YOUTUBE_PLAYER_SCRIPT", "");
      end if;

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
   -- CSS_Callback
   -------------------------------------------------------------------------------------------------
   function CSS_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      return AWS.Response.File (AWS.MIME.Text_CSS, URI (URI'First + 1 .. URI'Last));
   end CSS_Callback;

   -------------------------------------------------------------------------------------------------
   -- Search_Button_Callback
   -------------------------------------------------------------------------------------------------
   function Search_Button_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Parameters   : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
   begin
      Current_Room.Set_Video_Search_Results (AWS.Parameters.Get (Parameters, "search_input"));

      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
          ("search_results", Build_Search_Results (Current_Room.Get_Video_Search_Results)));
   end Search_Button_Callback;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Playlist_Callback
   -------------------------------------------------------------------------------------------------
   function Add_To_Playlist_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Parameters  : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Source      : constant T_Add_To_Playlist_Source :=
        T_Add_To_Playlist_Source'Value (AWS.Parameters.Get (Parameters, "source"));
      Item_Number : constant Natural := Natural'Value (AWS.Parameters.Get (Parameters, "item"));

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/socket");
   begin
      case Source is
         when Search_Results =>
            Current_Room.Add_Video_To_Playlists
              (Current_Room.Get_Video_Search_Results_Item (Item_Number));

         when Historic =>
            Current_Room.Add_Video_To_Playlists (Current_Room.Get_Historic_Item (Item_Number));

         when Likes =>
            Current_Room.Add_Video_To_Playlists (Current_Room.Get_Likes_Item (Item_Number));
      end case;

      AWS.Net.WebSocket.Registry.Send (Rcp, "update_playlist_request");

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Add_To_Playlist_Callback;

   -------------------------------------------------------------------------------------------------
   -- Add_Remove_Like_Callback
   -------------------------------------------------------------------------------------------------
   function Add_Remove_Like_Callback (Request : in AWS.Status.Data)
     return AWS.Response.Data is
      Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
      Parameters  : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Source      : constant T_Video_List_Source :=
        T_Video_List_Source'Value (AWS.Parameters.Get (Parameters, "source"));
      Like_Value  : constant T_Like := T_Like'Value (AWS.Parameters.Get (Parameters, "like"));
      Item_Number : constant Natural := Natural'Value (AWS.Parameters.Get (Parameters, "item"));

      Video_To_Add_Remove : T_Video;

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/socket");
   begin
      case Source is
         when Playlist =>
           Video_To_Add_Remove := Current_Room.Get_Client_Playlist_Item (Session_ID, Item_Number);
         when Historic => Video_To_Add_Remove := Current_Room.Get_Historic_Item (Item_Number);
         when Likes    => Video_To_Add_Remove := Current_Room.Get_Likes_Item (Item_Number);
      end case;

      case Like_Value is
         when Like   => Current_Room.Add_Like (Video_To_Add_Remove);
         when Unlike => Current_Room.Remove_Like (Video_To_Add_Remove);
      end case;

      -- Send update request only if the video lists are not empty
      if not Current_Room.Get_Client_Playlist (Session_ID).Is_Empty then
         AWS.Net.WebSocket.Registry.Send (Rcp, "update_playlist_request");
      end if;
      if not Current_Room.Get_Historic.Is_Empty then
         AWS.Net.WebSocket.Registry.Send (Rcp, "update_historic_request");
      end if;

      -- Likes list is always updated as a video has been added or removed
      AWS.Net.WebSocket.Registry.Send (Rcp, "update_likes_request");

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Add_Remove_Like_Callback;

   -------------------------------------------------------------------------------------------------
   -- Player_Display_Checkbox_Callback
   -------------------------------------------------------------------------------------------------
   function Player_Display_Checkbox_Callback (Request : in AWS.Status.Data)
     return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);
      Parameters : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Checked    : constant Boolean := Boolean'Value (AWS.Parameters.Get (Parameters, "checked"));
   begin
      Current_Room.Set_Client_Display_Player (Session_ID, Checked);

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Player_Display_Checkbox_Callback;

   -------------------------------------------------------------------------------------------------
   -- Player_Sync_Checkbox_Callback
   -------------------------------------------------------------------------------------------------
   function Player_Sync_Checkbox_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);
      Parameters : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Sync       : constant Boolean := Boolean'Value (AWS.Parameters.Get (Parameters, "checked"));
   begin
      Current_Room.Set_Client_Sync_With_Room (Session_ID, Sync);

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Player_Sync_Checkbox_Callback;

   -------------------------------------------------------------------------------------------------
   -- Next_Video_Callback
   -------------------------------------------------------------------------------------------------
   function Next_Video_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);
   begin
      Current_Room.Next_Client_Video (Session_ID);

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Next_Video_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_List_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Video_List_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);
      Parameters : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Source     : constant T_Video_List_Source :=
        T_Video_List_Source'Value (AWS.Parameters.Get (Parameters, "source"));
   begin
      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
        ("video_list", Build_Video_List (Session_ID, Source)));
   end Get_Video_List_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Room_Video_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Current_Room_Video_Callback return AWS.Response.Data is
   begin
      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
          ("current_room_video", To_String (Current_Room.Get_Current_Video.Video_Title)));
   end Get_Current_Room_Video_Callback;

   -------------------------------------------------------------------------------------------------
   -- Build_Search_Results
   -------------------------------------------------------------------------------------------------
   function Build_Search_Results (Video_Search_Results : in Video_Vectors.Vector) return String is
      Translations : Templates_Parser.Translate_Table (1 .. 3);

      Response : Unbounded_String;

      List_Cursor : Video_Vectors.Cursor := Video_Search_Results.First;
   begin
      while Video_Vectors.Has_Element (List_Cursor) loop
         Translations (1) := Templates_Parser.Assoc
           ("ITEM_ID", Trim
             (Integer'Image (Video_Vectors.To_Index (List_Cursor)), Ada.Strings.Left));

         Translations (2) := Templates_Parser.Assoc
           ("IMAGE_URL", Video_Vectors.Element (List_Cursor).Video_Thumbnail);

         Translations (3) := Templates_Parser.Assoc
           ("VIDEO_TITLE", Video_Vectors.Element (List_Cursor).Video_Title);

         Append (Response, To_String
           (Templates_Parser.Parse ("html/search_results_item.thtml", Translations)));

         List_Cursor := Video_Vectors.Next (List_Cursor);
      end loop;

      return To_String (Response);
   end Build_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Build_Video_List
   -------------------------------------------------------------------------------------------------
   function Build_Video_List (Session_ID : in AWS.Session.ID; Source : in T_Video_List_Source)
     return String is
      Translations : Templates_Parser.Translate_Table (1 .. 4);

      Response : Unbounded_String;

      Videos      : Video_Vectors.Vector;
      List_Cursor : Video_Vectors.Cursor;
   begin
      case Source is
         when Playlist =>
            Videos := Current_Room.Get_Client_Playlist (Session_ID);
            List_Cursor := Videos.First;
         when Historic =>
            Videos := Current_Room.Get_Historic;
            List_Cursor := Videos.Last;
         when Likes =>
            Videos := Current_Room.Get_Likes;
            List_Cursor := Videos.Last;
      end case;

      while Video_Vectors.Has_Element (List_Cursor) loop
         Translations (1) := Templates_Parser.Assoc
           ("ITEM_ID", Trim
             (Integer'Image (Video_Vectors.To_Index (List_Cursor)), Ada.Strings.Left));

         Translations (2) := Templates_Parser.Assoc
           ("IMAGE_URL", Video_Vectors.Element (List_Cursor).Video_Thumbnail);

         Translations (3) := Templates_Parser.Assoc
           ("VIDEO_TITLE", Video_Vectors.Element (List_Cursor).Video_Title);

         if Source = Likes then
            Translations (4) := Templates_Parser.Assoc ("LIKE", "Unlike");
         elsif Current_Room.Is_Video_Liked (Video_Vectors.Element (List_Cursor)) then
            Translations (4) := Templates_Parser.Assoc ("LIKE", "Unlike");
         else
            Translations (4) := Templates_Parser.Assoc ("LIKE", "Like");
         end if;

         case Source is
            when Playlist =>
               Append (Response, To_String
                 (Templates_Parser.Parse ("html/playlist_item.thtml", Translations)));

               List_Cursor := Video_Vectors.Next (List_Cursor);

            when Historic =>
               Append (Response, To_String
                 (Templates_Parser.Parse ("html/historic_item.thtml", Translations)));

               List_Cursor := Video_Vectors.Previous (List_Cursor);

            when Likes =>
               Append (Response, To_String
                 (Templates_Parser.Parse ("html/likes_item.thtml", Translations)));

               List_Cursor := Video_Vectors.Previous (List_Cursor);
         end case;

      end loop;

      return To_String (Response);
   end Build_Video_List;

   -------------------------------------------------------------------------------------------------
   -- Pack_AJAX_XML_Response
   -------------------------------------------------------------------------------------------------
   function Pack_AJAX_XML_Response (Placeholder : in String; Value : in String) return String is
      Translations : Templates_Parser.Translate_Table (1 .. 2);

      Replace_Fields : Unbounded_String;
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

end Callback;
