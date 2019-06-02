with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with AWS.MIME;
with AWS.Net.Websocket.Registry;
with AWS.Session; use AWS.Session;

with Templates_Parser;

package body Callback_Room is

   -------------------------------------------------------------------------------------------------
   -- Set_Server_Address
   -------------------------------------------------------------------------------------------------
   procedure Set_Server_Address (Address : in String) is
   begin
      SERVER_ADDRESS := To_Unbounded_String (Address);
   end Set_Server_Address;

   -------------------------------------------------------------------------------------------------
   -- Callback
   -------------------------------------------------------------------------------------------------
   function Callback (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Full_URI : constant String := AWS.Status.URI (Request);
      URI      : constant String := Full_URI
        (Full_URI'First + 1 + Current_Room.Get_Name'Length .. Full_URI'Last);

      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);

      Response : AWS.Response.Data;
   begin
      Current_Room.Lock;

      if URI = "/" or URI = "" then
         if not Current_Room.Is_Registered (Session_ID) then
            Current_Room.Add_Client (Session_ID);
         end if;

         Put_Line ("Room " & Current_Room.Get_Name & ", client " & AWS.Session.Image (Session_ID)
           & ", request: '" & URI & "'");

         Response := Room_Callback (Request, Current_Room);
      elsif Current_Room.Is_Registered (Session_ID) then
         Put_Line ("Room " & Current_Room.Get_Name & ", client " & AWS.Session.Image (Session_ID)
           & ", request: '" & URI & "'");

         -- Update the session life start time and client last request time
         AWS.Session.Touch (Session_ID);
         Current_Room.Set_Client_Last_Request_Time (Session_ID);

         if URI = "/onclick$search_button" then
            Response := Search_Button_Callback (Request, Current_Room);
         elsif URI = "/onclick$add_to_playlist" then
            Response := Add_To_Playlist_Callback (Request, Current_Room);
         elsif URI = "/onclick$remove_from_playlist" then
            Response := Remove_From_Playlist_Callback (Request, Current_Room);
         elsif URI = "/onclick$add_remove_like" then
            Response := Add_Remove_Like_Callback (Request, Current_Room);
         elsif URI = "/onclick$player_display_checkbox" then
            Response := Player_Display_Checkbox_Callback (Request, Current_Room);
         elsif URI = "/onclick$player_sync_checkbox" then
            Response := Player_Sync_Checkbox_Callback (Request, Current_Room);
         elsif URI = "/onclick$next_room_video" then
            Response := Next_Room_Video_Callback (Current_Room);
         elsif URI = "/onclick$up_vote" then
            Response := Up_Vote_Callback (Request, Current_Room);
         elsif URI = "/next_video" then
            Response := Next_Video_Callback (Request, Current_Room);
         elsif URI = "/get_video_list" then
            Response := Get_Video_List_Callback (Request, Current_Room);
         elsif URI = "/get_current_room_video" then
            Response := Get_Current_Room_Video_Callback (Current_Room);
         elsif URI = "/get_nb_clients" then
            Response := Get_Number_Clients_Callback (Current_Room);
         else
            Put_Line ("Room " & Current_Room.Get_Name & ", not supported request: '" & URI & "'");
            Response := AWS.Response.Build (AWS.MIME.Text_HTML, "");
         end if;
      else
         Put_Line ("Room " & Current_Room.Get_Name
           & ", not registered session ID and not supported request: '" & URI & "'");
         Response := AWS.Response.Build (AWS.MIME.Text_HTML, "");
      end if;

      Current_Room.Unlock;

      return Response;
   end Callback;

   -------------------------------------------------------------------------------------------------
   -- Room_Callback
   -------------------------------------------------------------------------------------------------
   function Room_Callback (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);

      Translations : Templates_Parser.Translate_Table (1 .. 9);
      YT_Player_Translations : Templates_Parser.Translate_Table (1 .. 2);
   begin
      Put_Line ("Video to play: "
        & To_String (Current_Room.Get_Current_Client_Video (Session_ID).Video_Title));

      Translations (9) := Templates_Parser.Assoc ("ROOM_NAME", Current_Room.Get_Name);

      -- Sync checkbox displaying and player state
      if Current_Room.Get_Client_Display_Player (Session_ID) then
         Translations (1) := Templates_Parser.Assoc ("DISPLAY_SYNC_CHECKBOX", "inline-block");
         Translations (7) := Templates_Parser.Assoc ("PLAYER_STATE", "end");
      else
         Translations (1) := Templates_Parser.Assoc ("DISPLAY_SYNC_CHECKBOX", "none");
         Translations (7) := Templates_Parser.Assoc ("PLAYER_STATE", "no_player");
      end if;

      -- Current room video title
      Translations (2) := Templates_Parser.Assoc
        ("ROOM_VIDEO", To_String (Current_Room.Get_Current_Video.Video_Title));

      -- Number of client sync
      Translations (3) := Templates_Parser.Assoc ("NB_CLIENTS",
        Trim (Current_Room.Get_Number_Clients_Sync'Img, Ada.Strings.Left) & "/" &
        Trim (Current_Room.Get_Number_Clients'Img, Ada.Strings.Left));

      -- Client playlist
      Translations (4) := Templates_Parser.Assoc
        ("VIDEO_LIST", Build_Playlist (Current_Room, Session_ID));

      -- Sync checkboxe value
      Translations (5) :=
        Templates_Parser.Assoc ("CLIENT_SYNC", Current_Room.Get_Client_Sync_With_Room (Session_ID));

      -- Server address for WebSocket
      Translations (6) := Templates_Parser.Assoc ("SERVER_ADDRESS", To_String (SERVER_ADDRESS));

      -- Player script
      if Current_Room.Get_Client_Display_Player (Session_ID)
        and not Current_Room.Client_Has_Nothing_To_Play (Session_ID) then
         YT_Player_Translations (1) := Templates_Parser.Assoc
           ("VIDEO_ID", To_String (Current_Room.Get_Current_Client_Video (Session_ID).Video_ID));

         -- Current room video title
         YT_Player_Translations (2) := Templates_Parser.Assoc ("ROOM_NAME", Current_Room.Get_Name);

         Translations (8) := Templates_Parser.Assoc
           ("YOUTUBE_PLAYER_SCRIPT",
            To_String
              (Templates_Parser.Parse ("javascripts/youtube_player.tjs", YT_Player_Translations)));
      else
         Translations (8) := Templates_Parser.Assoc ("YOUTUBE_PLAYER_SCRIPT", "");
      end if;

      return AWS.Response.Build
        (AWS.MIME.Text_HTML, To_String (Templates_Parser.Parse ("html/room.thtml", Translations)));
   end Room_Callback;

   -------------------------------------------------------------------------------------------------
   -- Search_Button_Callback
   -------------------------------------------------------------------------------------------------
   function Search_Button_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);

      Direct_Link    : Boolean;
      Search_Results : constant Video_Vectors.Vector := Current_Room.Get_Video_Search_Results
        (Session_ID, AWS.Status.Parameter (Request, "search_input"), Direct_Link);

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/" & Current_Room.Get_Name & "Socket");
   begin
      if Direct_Link then
         AWS.Net.WebSocket.Registry.Send (Rcp, "update_playlist_request");
      end if;

      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
          ("search_results", Build_Search_Results (Search_Results)));
   end Search_Button_Callback;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Playlist_Callback
   -------------------------------------------------------------------------------------------------
   function Add_To_Playlist_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);

      Video : constant T_Video :=
        (Video_ID        => To_Unbounded_String (AWS.Status.Parameter (Request, "videoId")),
         Video_Title     => To_Unbounded_String (AWS.Status.Parameter (Request, "videoTitle")),
         Video_Thumbnail => To_Unbounded_String (AWS.Status.Parameter (Request, "videoThumbnail")));

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Add_Video_To_Playlists (Session_ID, Video);

      AWS.Net.WebSocket.Registry.Send (Rcp, "update_playlist_request");

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Add_To_Playlist_Callback;

   -------------------------------------------------------------------------------------------------
   -- Remove_From_Playlist_Callback
   -------------------------------------------------------------------------------------------------
   function Remove_From_Playlist_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Item_ID : constant T_Playlist_Item_ID
        := T_Playlist_Item_ID'Value (AWS.Status.Parameter (Request, "itemId"));

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Remove_From_Playlists (Item_ID);

      AWS.Net.WebSocket.Registry.Send (Rcp, "update_playlist_request");

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Remove_From_Playlist_Callback;

   -------------------------------------------------------------------------------------------------
   -- Add_Remove_Like_Callback
   -------------------------------------------------------------------------------------------------
   function Add_Remove_Like_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);

      Video       : constant T_Video :=
        (Video_ID        => To_Unbounded_String (AWS.Status.Parameter (Request, "videoId")),
         Video_Title     => To_Unbounded_String (AWS.Status.Parameter (Request, "videoTitle")),
         Video_Thumbnail => To_Unbounded_String (AWS.Status.Parameter (Request, "videoThumbnail")));
      Liked       : constant Boolean := Boolean'Value (AWS.Status.Parameter (Request, "liked"));

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/" & Current_Room.Get_Name & "Socket");
   begin
      if Liked then
         Current_Room.Remove_Like (Video);
      else
         Current_Room.Add_Like (Video);
      end if;

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
   function Player_Display_Checkbox_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);
      Checked    : constant Boolean := Boolean'Value (AWS.Status.Parameter (Request, "checked"));

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Set_Client_Display_Player (Session_ID, Checked);

      -- Send update request for the number of clients
      AWS.Net.WebSocket.Registry.Send (Rcp, "update_nb_clients");

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Player_Display_Checkbox_Callback;

   -------------------------------------------------------------------------------------------------
   -- Player_Sync_Checkbox_Callback
   -------------------------------------------------------------------------------------------------
   function Player_Sync_Checkbox_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);
      Sync       : constant Boolean := Boolean'Value (AWS.Status.Parameter (Request, "checked"));

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Set_Client_Sync_With_Room (Session_ID, Sync);

      -- Send update request for the number of clients
      AWS.Net.WebSocket.Registry.Send (Rcp, "update_nb_clients");

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Player_Sync_Checkbox_Callback;

   -------------------------------------------------------------------------------------------------
   -- Next_Room_Video_Callback
   -------------------------------------------------------------------------------------------------
   function Next_Room_Video_Callback (Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Next_Room_Video;

      while not Current_Room.Get_Room_Next_Video_Ready loop
         null;
      end loop;

      -- Send next video request for all the clients sync
      AWS.Net.WebSocket.Registry.Send (Rcp, "force_next_video");

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Next_Room_Video_Callback;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Callback
   -------------------------------------------------------------------------------------------------
   function Up_Vote_Callback (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Item_ID : constant T_Playlist_Item_ID
        := T_Playlist_Item_ID'Value (AWS.Status.Parameter (Request, "itemId"));

      Rcp : constant AWS.Net.WebSocket.Registry.Recipient :=
        AWS.Net.WebSocket.Registry.Create (URI => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Up_Vote_Playlist_Item (Item_ID);

      AWS.Net.WebSocket.Registry.Send (Rcp, "update_playlist_request");

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Up_Vote_Callback;

   -------------------------------------------------------------------------------------------------
   -- Next_Video_Callback
   -------------------------------------------------------------------------------------------------
   function Next_Video_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);
   begin
      Current_Room.Next_Client_Video (Session_ID);

      return AWS.Response.Build (AWS.MIME.Text_HTML, "");
   end Next_Video_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_List_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Video_List_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
      Session_ID : constant AWS.Session.ID := AWS.Status.Session (Request);
      Source     : constant T_Video_List_Source :=
        T_Video_List_Source'Value (AWS.Status.Parameter (Request, "source"));

      Response : Unbounded_String := Null_Unbounded_String;
   begin
      case Source is
         when Playlist =>
            Response := To_Unbounded_String (Build_Playlist (Current_Room, Session_ID));

         when others =>
            Response := To_Unbounded_String (Build_Video_List (Current_Room, Source));
      end case;

      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
        ("video_list", To_String (Response)));
   end Get_Video_List_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Room_Video_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Current_Room_Video_Callback (Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
   begin
      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
          ("current_room_video", To_String (Current_Room.Get_Current_Video.Video_Title)));
   end Get_Current_Room_Video_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Clients_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Number_Clients_Callback (Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data is
   begin
      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response ("nb_clients",
        Trim (Current_Room.Get_Number_Clients_Sync'Img, Ada.Strings.Left) & "/" &
        Trim (Current_Room.Get_Number_Clients'Img, Ada.Strings.Left)));
   end Get_Number_Clients_Callback;

   -------------------------------------------------------------------------------------------------
   -- Build_Search_Results
   -------------------------------------------------------------------------------------------------
   function Build_Search_Results (Video_Search_Results : in Video_Vectors.Vector) return String is
      Translations : Templates_Parser.Translate_Table (1 .. 3);

      Response : Unbounded_String := Null_Unbounded_String;

      List_Cursor : Video_Vectors.Cursor := Video_Search_Results.First;
   begin
      while Video_Vectors.Has_Element (List_Cursor) loop
         Translations (1) := Templates_Parser.Assoc
           ("VIDEO_ID", Video_Vectors.Element (List_Cursor).Video_ID);

         Translations (2) := Templates_Parser.Assoc
           ("VIDEO_TITLE", Video_Vectors.Element (List_Cursor).Video_Title);

         Translations (3) := Templates_Parser.Assoc
           ("VIDEO_THUMBNAIL", Video_Vectors.Element (List_Cursor).Video_Thumbnail);

         Append (Response, To_String
           (Templates_Parser.Parse ("html/search_results_item.thtml", Translations)));

         List_Cursor := Video_Vectors.Next (List_Cursor);
      end loop;

      return To_String (Response);
   end Build_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Build_Playlist
   -------------------------------------------------------------------------------------------------
   function Build_Playlist
     (Current_Room : in T_Room_Class_Access;
      Session_ID   : in AWS.Session.ID)
     return String is
      Translations : Templates_Parser.Translate_Table (1 .. 7);

      Response : Unbounded_String := Null_Unbounded_String;

      Playlist_Items : constant Playlist_Vectors.Vector :=
        Current_Room.Get_Client_Playlist (Session_ID);

      List_Cursor : Playlist_Vectors.Cursor := Playlist_Items.First;
   begin
      while Playlist_Vectors.Has_Element (List_Cursor) loop
         Translations (1) := Templates_Parser.Assoc
           ("SOURCE_CLIENT", Session_ID = Playlist_Vectors.Element (List_Cursor).Client_ID);

         Translations (2) := Templates_Parser.Assoc
           ("ITEM_ID", Trim (Playlist_Vectors.Element (List_Cursor).ID'Img, Ada.Strings.Left));

         Translations (3) := Templates_Parser.Assoc
           ("VIDEO_ID", Playlist_Vectors.Element (List_Cursor).Video.Video_ID);

         Translations (4) := Templates_Parser.Assoc
           ("VIDEO_TITLE", Playlist_Vectors.Element (List_Cursor).Video.Video_Title);

         Translations (5) := Templates_Parser.Assoc
           ("VIDEO_THUMBNAIL", Playlist_Vectors.Element (List_Cursor).Video.Video_Thumbnail);

         if Current_Room.Is_Video_Liked (Playlist_Vectors.Element (List_Cursor).Video) then
            Translations (6) := Templates_Parser.Assoc ("LIKE", "s");
         else
            Translations (6) := Templates_Parser.Assoc ("LIKE", "r");
         end if;

         Translations (7) := Templates_Parser.Assoc
           ("UP_VOTES",
            Trim (Playlist_Vectors.Element (List_Cursor).Up_Votes'Img, Ada.Strings.Left));

         Append (Response, To_String
           (Templates_Parser.Parse ("html/playlist_item.thtml", Translations)));

         Playlist_Vectors.Next (List_Cursor);
      end loop;

      return To_String (Response);
   end Build_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Build_Video_List
   -------------------------------------------------------------------------------------------------
   function Build_Video_List
     (Current_Room : in T_Room_Class_Access;
      Source       : in T_Video_List_Source)
     return String is
      Translations : Templates_Parser.Translate_Table (1 .. 4);

      Response : Unbounded_String := Null_Unbounded_String;

      Videos      : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
      List_Cursor : Video_Vectors.Cursor;
   begin
      if Source = Historic then
         Videos := Current_Room.Get_Historic;
         List_Cursor := Videos.Last;
      elsif Source = Likes then
         Videos := Current_Room.Get_Likes;
         List_Cursor := Videos.Last;
      end if;

      while Video_Vectors.Has_Element (List_Cursor) loop
         Translations (1) := Templates_Parser.Assoc
           ("VIDEO_ID", Video_Vectors.Element (List_Cursor).Video_ID);

         Translations (2) := Templates_Parser.Assoc
           ("VIDEO_TITLE", Video_Vectors.Element (List_Cursor).Video_Title);

         Translations (3) := Templates_Parser.Assoc
           ("VIDEO_THUMBNAIL", Video_Vectors.Element (List_Cursor).Video_Thumbnail);

         if Source = Likes then
            Translations (4) := Templates_Parser.Assoc ("LIKE", "s");
         elsif Current_Room.Is_Video_Liked (Video_Vectors.Element (List_Cursor)) then
            Translations (4) := Templates_Parser.Assoc ("LIKE", "s");
         else
            Translations (4) := Templates_Parser.Assoc ("LIKE", "r");
         end if;

         Append (Response, To_String
           (Templates_Parser.Parse ("html/video_list_item.thtml", Translations)));

         Video_Vectors.Previous (List_Cursor);
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

end Callback_Room;
