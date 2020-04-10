with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Aws.Mime;
with Aws.Net.Websocket.Registry;
with Aws.Session; use Aws.Session;

with Templates_Parser;

with Api;
with Playlist;      use Playlist;
with Playlist_Item; use Playlist_Item;
with Song;          use Song;

package body Callback_Room is

   -------------------------------------------------------------------------------------------------
   -- Set_Server_Address
   -------------------------------------------------------------------------------------------------
   procedure Set_Server_Address (Address : in String) is
   begin
      Server_Address := To_Unbounded_String (Address);
   end Set_Server_Address;

   -------------------------------------------------------------------------------------------------
   -- Callback
   -------------------------------------------------------------------------------------------------
   function Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Full_Uri : constant String := Aws.Status.Uri (Request);
      Uri      : constant String :=
        Full_Uri (Full_Uri'First + 1 + Current_Room.Get_Name'Length .. Full_Uri'Last);

      Session_Id : constant Aws.Session.Id := Aws.Status.Session (Request);

      Response : Aws.Response.Data;
   begin
      Current_Room.Lock;

      if Uri = "/" or Uri = "" then
         if not Current_Room.Is_Registered (Session_Id) then
            Current_Room.Add_Client (Session_Id);
         end if;

         Put_Line
           ("Room " &
            Current_Room.Get_Name &
            ", client " &
            Aws.Session.Image (Session_Id) &
            ", request: '" &
            Uri &
            "'");

         Response := Room_Callback (Request, Current_Room);
      elsif Current_Room.Is_Registered (Session_Id) then
         Put_Line
           ("Room " &
            Current_Room.Get_Name &
            ", client " &
            Aws.Session.Image (Session_Id) &
            ", request: '" &
            Uri &
            "'");

         -- Update the session life start time and client last request time
         Aws.Session.Touch (Session_Id);
         Current_Room.Set_Client_Last_Request_Time (Session_Id);

         if Uri = "/onclick$search_button" then
            Response := Search_Button_Callback (Request, Current_Room);
         elsif Uri = "/onclick$add_to_playlist" then
            Response := Add_To_Playlist_Callback (Request, Current_Room);
         elsif Uri = "/onclick$remove_from_playlist" then
            Response := Remove_From_Playlist_Callback (Request, Current_Room);
         elsif Uri = "/onclick$add_remove_like" then
            Response := Add_Remove_Like_Callback (Request, Current_Room);
         elsif Uri = "/onclick$player_display_checkbox" then
            Response := Player_Display_Checkbox_Callback (Request, Current_Room);
         elsif Uri = "/onclick$player_sync_checkbox" then
            Response := Player_Sync_Checkbox_Callback (Request, Current_Room);
         elsif Uri = "/onclick$next_room_video" then
            Response := Next_Room_Song_Callback (Current_Room);
         elsif Uri = "/onclick$up_vote" then
            Response := Up_Vote_Callback (Request, Current_Room);
         elsif Uri = "/next_video" then
            Response := Next_Song_Callback (Request, Current_Room);
         elsif Uri = "/get_video_list" then
            Response := Get_Song_List_Callback (Request, Current_Room);
         elsif Uri = "/get_current_room_video" then
            Response := Get_Current_Room_Song_Callback (Current_Room);
         elsif Uri = "/get_nb_clients" then
            Response := Get_Number_Clients_Callback (Current_Room);
         else
            Put_Line ("Room " & Current_Room.Get_Name & ", not supported request: '" & Uri & "'");
            Response := Aws.Response.Build (Aws.Mime.Text_Html, "");
         end if;
      else
         Put_Line
           ("Room " &
            Current_Room.Get_Name &
            ", not registered session ID and not supported request: '" &
            Uri &
            "'");
         Response := Aws.Response.Build (Aws.Mime.Text_Html, "");
      end if;

      Current_Room.Unlock;

      return Response;
   end Callback;

   -------------------------------------------------------------------------------------------------
   -- Room_Callback
   -------------------------------------------------------------------------------------------------
   function Room_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Session_Id : constant Aws.Session.Id := Aws.Status.Session (Request);

      Translations           : Templates_Parser.Translate_Table (1 .. 9);
      Yt_Player_Translations : Templates_Parser.Translate_Table (1 .. 2);
   begin
      Put_Line ("Song to play: " & Current_Room.Get_Current_Client_Song (Session_Id).Get_Title);

      Translations (9) := Templates_Parser.Assoc ("ROOM_NAME", Current_Room.Get_Name);

      -- Sync checkbox displaying and player state
      if Current_Room.Get_Client_Display_Player (Session_Id) then
         Translations (1) := Templates_Parser.Assoc ("DISPLAY_SYNC_CHECKBOX", "inline-block");
         Translations (7) := Templates_Parser.Assoc ("PLAYER_STATE", "end");
      else
         Translations (1) := Templates_Parser.Assoc ("DISPLAY_SYNC_CHECKBOX", "none");
         Translations (7) := Templates_Parser.Assoc ("PLAYER_STATE", "no_player");
      end if;

      -- Current room song title
      Translations (2) :=
        Templates_Parser.Assoc ("ROOM_VIDEO", Current_Room.Get_Current_Song.Get_Title);

      -- Number of client sync
      Translations (3) :=
        Templates_Parser.Assoc
          ("NB_CLIENTS",
           Trim (Current_Room.Get_Number_Clients_Sync'Img, Ada.Strings.Left) &
           "/" &
           Trim (Current_Room.Get_Number_Clients'Img, Ada.Strings.Left));

      -- Client playlist
      Translations (4) :=
        Templates_Parser.Assoc ("VIDEO_LIST", Build_Playlist (Current_Room, Session_Id));

      -- Sync checkboxe value
      Translations (5) :=
        Templates_Parser.Assoc ("CLIENT_SYNC", Current_Room.Get_Client_Sync_With_Room (Session_Id));

      -- Server address for WebSocket
      Translations (6) := Templates_Parser.Assoc ("SERVER_ADDRESS", To_String (Server_Address));

      -- Player script
      if Current_Room.Get_Client_Display_Player (Session_Id) and
        not Current_Room.Client_Has_Nothing_To_Play (Session_Id)
      then
         Yt_Player_Translations (1) :=
           Templates_Parser.Assoc
             ("VIDEO_ID",
              Current_Room.Get_Current_Client_Song (Session_Id).Get_Id);

         -- Current room song title
         Yt_Player_Translations (2) := Templates_Parser.Assoc ("ROOM_NAME", Current_Room.Get_Name);

         Translations (8) :=
           Templates_Parser.Assoc
             ("YOUTUBE_PLAYER_SCRIPT",
              To_String
                (Templates_Parser.Parse
                   ("javascripts/youtube_player.tjs",
                    Yt_Player_Translations)));
      else
         Translations (8) := Templates_Parser.Assoc ("YOUTUBE_PLAYER_SCRIPT", "");
      end if;

      return Aws.Response.Build
          (Aws.Mime.Text_Html,
           To_String (Templates_Parser.Parse ("html/room.thtml", Translations)));
   end Room_Callback;

   -------------------------------------------------------------------------------------------------
   -- Search_Button_Callback
   -------------------------------------------------------------------------------------------------
   function Search_Button_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Session_Id : constant Aws.Session.Id := Aws.Status.Session (Request);

      Direct_Link    : Boolean;
      Search_Results : constant T_Song_Vector :=
        Current_Room.Get_Song_Search_Results
        (Session_Id, Aws.Status.Parameter (Request, "search_input"), Direct_Link);

      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & Current_Room.Get_Name & "Socket");
   begin
      if Direct_Link then
         Aws.Net.Websocket.Registry.Send (Rcp, "update_playlist_request");
         Aws.Net.Websocket.Registry.Send (Rcp, "clear_search_input");
      end if;

      return Aws.Response.Build
          (Aws.Mime.Text_Xml,
           Pack_Ajax_Xml_Response ("search_results", Build_Search_Results (Search_Results)));
   end Search_Button_Callback;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Playlist_Callback
   -------------------------------------------------------------------------------------------------
   function Add_To_Playlist_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Session_Id : constant Aws.Session.Id := Aws.Status.Session (Request);

      New_Song : constant T_Song :=
        Song.Constructors.Initialize
          (Id             => Aws.Status.Parameter (Request, "VideoId"),
           Title          => Aws.Status.Parameter (Request, "videoTitle"),
           Thumbnail_Link => Aws.Status.Parameter (Request, "videoThumbnail"),
           Provider       => Api.Youtube_Api);

      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Add_Song_To_Playlists (Session_Id, New_Song);

      Aws.Net.Websocket.Registry.Send (Rcp, "update_playlist_request");

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Add_To_Playlist_Callback;

   -------------------------------------------------------------------------------------------------
   -- Remove_From_Playlist_Callback
   -------------------------------------------------------------------------------------------------
   function Remove_From_Playlist_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Item_Id : constant T_Playlist_Item_Id :=
        T_Playlist_Item_Id'Value (Aws.Status.Parameter (Request, "itemId"));

      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Remove_From_Playlists (Item_Id);

      Aws.Net.Websocket.Registry.Send (Rcp, "update_playlist_request");

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Remove_From_Playlist_Callback;

   -------------------------------------------------------------------------------------------------
   -- Add_Remove_Like_Callback
   -------------------------------------------------------------------------------------------------
   function Add_Remove_Like_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Session_Id : constant Aws.Session.Id := Aws.Status.Session (Request);

      Modified_Song : constant T_Song :=
        Song.Constructors.Initialize
          (Id             => Aws.Status.Parameter (Request, "videoId"),
           Title          => Aws.Status.Parameter (Request, "videoTitle"),
           Thumbnail_Link => Aws.Status.Parameter (Request, "videoThumbnail"),
           Provider       => Api.Youtube_Api);
      Liked : constant Boolean := Boolean'Value (Aws.Status.Parameter (Request, "liked"));

      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & Current_Room.Get_Name & "Socket");
   begin
      if Liked then
         Current_Room.Remove_Like (Modified_Song);
      else
         Current_Room.Add_Like (Modified_Song);
      end if;

      -- Send update request only if the song lists are not empty
      if not Current_Room.Get_Client_Playlist (Session_Id).Is_Empty then
         Aws.Net.Websocket.Registry.Send (Rcp, "update_playlist_request");
      end if;
      if not Current_Room.Get_Historic.Is_Empty then
         Aws.Net.Websocket.Registry.Send (Rcp, "update_historic_request");
      end if;

      -- Likes list is always updated as a song has been added or removed
      Aws.Net.Websocket.Registry.Send (Rcp, "update_likes_request");

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Add_Remove_Like_Callback;

   -------------------------------------------------------------------------------------------------
   -- Player_Display_Checkbox_Callback
   -------------------------------------------------------------------------------------------------
   function Player_Display_Checkbox_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Session_Id : constant Aws.Session.Id := Aws.Status.Session (Request);
      Checked    : constant Boolean := Boolean'Value (Aws.Status.Parameter (Request, "checked"));

      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Set_Client_Display_Player (Session_Id, Checked);

      -- Send update request for the number of clients
      Aws.Net.Websocket.Registry.Send (Rcp, "update_nb_clients");

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Player_Display_Checkbox_Callback;

   -------------------------------------------------------------------------------------------------
   -- Player_Sync_Checkbox_Callback
   -------------------------------------------------------------------------------------------------
   function Player_Sync_Checkbox_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Session_Id : constant Aws.Session.Id := Aws.Status.Session (Request);
      Sync       : constant Boolean := Boolean'Value (Aws.Status.Parameter (Request, "checked"));

      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Set_Client_Sync_With_Room (Session_Id, Sync);

      -- Send update request for the number of clients
      Aws.Net.Websocket.Registry.Send (Rcp, "update_nb_clients");

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Player_Sync_Checkbox_Callback;

   -------------------------------------------------------------------------------------------------
   -- Next_Room_Song_Callback
   -------------------------------------------------------------------------------------------------
   function Next_Room_Song_Callback
     (Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Next_Room_Song;

      while not Current_Room.Get_Room_Next_Song_Ready loop
         null;
      end loop;

      -- Send next song request for all the clients sync
      Aws.Net.Websocket.Registry.Send (Rcp, "force_next_video");

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Next_Room_Song_Callback;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Callback
   -------------------------------------------------------------------------------------------------
   function Up_Vote_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Item_Id : constant T_Playlist_Item_Id :=
        T_Playlist_Item_Id'Value (Aws.Status.Parameter (Request, "itemId"));

      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & Current_Room.Get_Name & "Socket");
   begin
      Current_Room.Up_Vote_Playlist_Item (Item_Id);

      Aws.Net.Websocket.Registry.Send (Rcp, "update_playlist_request");

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Up_Vote_Callback;

   -------------------------------------------------------------------------------------------------
   -- Next_Song_Callback
   -------------------------------------------------------------------------------------------------
   function Next_Song_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Session_Id : constant Aws.Session.Id := Aws.Status.Session (Request);
   begin
      Current_Room.Next_Client_Song (Session_Id);

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Next_Song_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_List_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Song_List_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
      Session_Id : constant Aws.Session.Id     := Aws.Status.Session (Request);
      Source     : constant T_Song_List_Source :=
        T_Song_List_Source'Value (Aws.Status.Parameter (Request, "source"));

      Response : Unbounded_String := Null_Unbounded_String;
   begin
      case Source is
         when Playlist =>
            Response := To_Unbounded_String (Build_Playlist (Current_Room, Session_Id));

         when others =>
            Response := To_Unbounded_String (Build_Song_List (Current_Room, Source));
      end case;

      return Aws.Response.Build
          (Aws.Mime.Text_Xml,
           Pack_Ajax_Xml_Response ("video_list", To_String (Response)));
   end Get_Song_List_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Room_Song_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Current_Room_Song_Callback
     (Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
   begin
      return Aws.Response.Build
          (Aws.Mime.Text_Xml,
           Pack_Ajax_Xml_Response ("current_room_video", Current_Room.Get_Current_Song.Get_Title));
   end Get_Current_Room_Song_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Clients_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Number_Clients_Callback
     (Current_Room : in T_Room_Class_Access) return Aws.Response.Data
   is
   begin
      return Aws.Response.Build
          (Aws.Mime.Text_Xml,
           Pack_Ajax_Xml_Response
             ("nb_clients",
              Trim (Current_Room.Get_Number_Clients_Sync'Img, Ada.Strings.Left) &
              "/" &
              Trim (Current_Room.Get_Number_Clients'Img, Ada.Strings.Left)));
   end Get_Number_Clients_Callback;

   -------------------------------------------------------------------------------------------------
   -- Build_Search_Results
   -------------------------------------------------------------------------------------------------
   function Build_Search_Results (Song_Search_Results : in T_Song_Vector) return String is
      Translations : Templates_Parser.Translate_Table (1 .. 3);

      Response : Unbounded_String := Null_Unbounded_String;

      List_Cursor : Song_Vectors.Cursor := Song_Search_Results.First;
   begin
      while Song_Vectors.Has_Element (List_Cursor) loop
         Translations (1) :=
           Templates_Parser.Assoc ("VIDEO_ID", Song_Vectors.Element (List_Cursor).Get_Id);

         Translations (2) :=
           Templates_Parser.Assoc ("VIDEO_TITLE", Song_Vectors.Element (List_Cursor).Get_Title);

         Translations (3) :=
           Templates_Parser.Assoc
             ("VIDEO_THUMBNAIL",
              Song_Vectors.Element (List_Cursor).Get_Thumbnail_Link);

         Append
           (Response,
            To_String (Templates_Parser.Parse ("html/search_results_item.thtml", Translations)));

         List_Cursor := Song_Vectors.Next (List_Cursor);
      end loop;

      return To_String (Response);
   end Build_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Build_Playlist
   -------------------------------------------------------------------------------------------------
   function Build_Playlist
     (Current_Room : in T_Room_Class_Access;
      Session_Id   : in Aws.Session.Id) return String
   is
      Translations : Templates_Parser.Translate_Table (1 .. 7);

      Response : Unbounded_String := Null_Unbounded_String;

      Playlist_Items : constant T_Playlist := Current_Room.Get_Client_Playlist (Session_Id);

      List_Cursor : Playlist_Item_Vectors.Cursor := Playlist_Items.First;
   begin
      while Playlist_Item_Vectors.Has_Element (List_Cursor) loop
         Translations (1) :=
           Templates_Parser.Assoc
             ("SOURCE_CLIENT",
              Session_Id = Playlist_Item_Vectors.Element (List_Cursor).Get_Client_Id);

         Translations (2) :=
           Templates_Parser.Assoc
             ("ITEM_ID",
              Trim (Playlist_Item_Vectors.Element (List_Cursor).Get_Id'Img, Ada.Strings.Left));

         Translations (3) :=
           Templates_Parser.Assoc
             ("VIDEO_ID",
              Playlist_Item_Vectors.Element (List_Cursor).Get_Song.Get_Id);

         Translations (4) :=
           Templates_Parser.Assoc
             ("VIDEO_TITLE",
              Playlist_Item_Vectors.Element (List_Cursor).Get_Song.Get_Title);

         Translations (5) :=
           Templates_Parser.Assoc
             ("VIDEO_THUMBNAIL",
              Playlist_Item_Vectors.Element (List_Cursor).Get_Song.Get_Thumbnail_Link);

         if Current_Room.Is_Song_Liked (Playlist_Item_Vectors.Element (List_Cursor).Get_Song) then
            Translations (6) := Templates_Parser.Assoc ("LIKE", "s");
         else
            Translations (6) := Templates_Parser.Assoc ("LIKE", "r");
         end if;

         Translations (7) :=
           Templates_Parser.Assoc
             ("UP_VOTES",
              Trim
                (Playlist_Item_Vectors.Element (List_Cursor).Get_Up_Votes'Img,
                 Ada.Strings.Left));

         Append
           (Response,
            To_String (Templates_Parser.Parse ("html/playlist_item.thtml", Translations)));

         Playlist_Item_Vectors.Next (List_Cursor);
      end loop;

      return To_String (Response);
   end Build_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Build_Song_List
   -------------------------------------------------------------------------------------------------
   function Build_Song_List
     (Current_Room : in T_Room_Class_Access;
      Source       : in T_Song_List_Source) return String
   is
      Translations : Templates_Parser.Translate_Table (1 .. 4);

      Response : Unbounded_String := Null_Unbounded_String;

      Songs       : T_Song_Vector := Song_Vector.Constructors.Initialize;
      List_Cursor : Song_Vectors.Cursor;
   begin
      if Source = Historic then
         Songs       := Current_Room.Get_Historic;
         List_Cursor := Songs.Last;
      elsif Source = Likes then
         Songs       := Current_Room.Get_Likes;
         List_Cursor := Songs.Last;
      end if;

      while Song_Vectors.Has_Element (List_Cursor) loop
         Translations (1) :=
           Templates_Parser.Assoc ("VIDEO_ID", Song_Vectors.Element (List_Cursor).Get_Id);

         Translations (2) :=
           Templates_Parser.Assoc ("VIDEO_TITLE", Song_Vectors.Element (List_Cursor).Get_Title);

         Translations (3) :=
           Templates_Parser.Assoc
             ("VIDEO_THUMBNAIL",
              Song_Vectors.Element (List_Cursor).Get_Thumbnail_Link);

         if Source = Likes then
            Translations (4) := Templates_Parser.Assoc ("LIKE", "s");
         elsif Current_Room.Is_Song_Liked (Song_Vectors.Element (List_Cursor)) then
            Translations (4) := Templates_Parser.Assoc ("LIKE", "s");
         else
            Translations (4) := Templates_Parser.Assoc ("LIKE", "r");
         end if;

         Append
           (Response,
            To_String (Templates_Parser.Parse ("html/video_list_item.thtml", Translations)));

         Song_Vectors.Previous (List_Cursor);
      end loop;

      return To_String (Response);
   end Build_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Pack_AJAX_XML_Response
   -------------------------------------------------------------------------------------------------
   function Pack_Ajax_Xml_Response (Placeholder : in String; Value : in String) return String is
      Translations : Templates_Parser.Translate_Table (1 .. 2);

      Replace_Fields : Unbounded_String;
   begin
      -- Pack <replace> fields
      -- For now, only one field at a time is supported
      Translations (1) := Templates_Parser.Assoc ("PLACEHOLDER", Placeholder);
      Translations (2) := Templates_Parser.Assoc ("VALUE", Value);

      Replace_Fields :=
        To_Unbounded_String (Templates_Parser.Parse ("xml/ajax_xml_replace.txml", Translations));

      -- Pack global response
      Translations (1) := Templates_Parser.Assoc ("ACTION_FIELDS", Replace_Fields);

      return Templates_Parser.Parse ("xml/ajax_xml_response.txml", Translations);
   end Pack_Ajax_Xml_Response;

end Callback_Room;
