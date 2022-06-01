with Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Aws.Mime;
with Aws.Session;

with Templates_Parser;

with Api;
with Sanitizer;
with Song.Item.List;

package body Callback_Room is

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   procedure Initialize is
      Config_File : File_Type;
   begin
      -- Read server address
      Open (File => Config_File, Mode => In_File, Name => "server_address.txt");
      Server_Ip := To_Unbounded_String (Get_Line (Config_File));
      Close (Config_File);
   end Initialize;

   -------------------------------------------------------------------------------------------------
   -- Callback
   -------------------------------------------------------------------------------------------------
   function Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
      Full_Uri : constant String := Aws.Status.Uri (Request);
      Uri      : constant String :=
        Full_Uri (Full_Uri'First + 1 + Current_Room.Get_Name'Length .. Full_Uri'Last);

      Session_Id : constant Aws.Session.Id := Aws.Status.Session (Request);

      Response : Aws.Response.Data;
   begin
      Current_Room.Lock;

      if Uri = "/" or Uri = "" then
         if not Current_Room.Is_Client_Registered (Session_Id) then
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

         Response := Room_Page_Callback (Request, Current_Room);
      elsif Current_Room.Is_Client_Registered (Session_Id) then
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
         Current_Room.Get_Client (Session_Id).Update_Last_Request_Time;

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
         elsif Uri = "/onclick$next_room_song" then
            Response := Next_Room_Song_Callback (Current_Room);
         elsif Uri = "/onclick$up_vote" then
            Response := Up_Vote_Callback (Request, Current_Room);
         elsif Uri = "/next_client_song" then
            Response := Next_Client_Song_Callback (Request, Current_Room);
         elsif Uri = "/get_song_list" then
            Response := Get_Song_List_Callback (Request, Current_Room);
         elsif Uri = "/get_current_room_song" then
            Response := Get_Current_Room_Song_Callback (Current_Room);
         elsif Uri = "/get_next_song_votes" then
            Response := Get_Next_Song_Votes_Callback (Current_Room);
         elsif Uri = "/get_nb_clients" then
            Response := Get_Number_Of_Clients_Callback (Current_Room);
         elsif Uri = "/get_chat_log" then
            Response := Get_Chat_Log_Callback (Current_Room);
         elsif Uri = "/add_chat_message" then
            Response := Add_Chat_Message_Callback (Request, Current_Room);
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
   -- Room_Page_Callback
   -------------------------------------------------------------------------------------------------
   function Room_Page_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
      use Templates_Parser;

      use type Api.T_Api_Provider;

      Current_Client : constant Client.T_Client_Access :=
        Current_Room.Get_Client (Aws.Status.Session (Request));
      Current_Song : Song.T_Song;

      Translations        : Translate_Set;
      Player_Translations : Translate_Set;
   begin
      if Current_Client.Is_Sync_With_Room then
         Current_Song := Current_Room.Get_Current_Song;
      else
         Current_Song := Current_Client.Get_Current_Song;
      end if;

      Put_Line ("Song to play: " & Current_Song.Get_Title);

      Insert (Translations, Assoc (Room_Name'Img, Current_Room.Get_Name));

      if Current_Client.Is_Player_Displayed then
         Insert (Translations, Assoc (Player_State'Img, "end"));
      else
         Insert (Translations, Assoc (Player_State'Img, "no_player"));
      end if;

      if
        (Current_Client.Is_Sync_With_Room
         and then Current_Room.Get_Current_Song.Get_Provider /= Api.No_Provider_Api)
        or else
        (not Current_Client.Is_Sync_With_Room
         and then Current_Client.Get_Current_Song.Get_Provider /= Api.No_Provider_Api)
      then
         Insert (Translations, Assoc (Player_Checkbox'Img, "block"));
      else
         Insert (Translations, Assoc (Player_Checkbox'Img, "none"));
      end if;

      if Current_Room.Get_Current_Song.Get_Provider /= Api.No_Provider_Api and
        Current_Client.Is_Sync_With_Room
      then
         Insert (Translations, Assoc (Next_Room_Song'Img, "inline"));
      else
         Insert (Translations, Assoc (Next_Room_Song'Img, "none"));
      end if;

      Insert (Translations, Assoc (Room_Song'Img, Current_Room.Get_Current_Song.Get_Title));
      Insert
        (Translations,
         Assoc
           (Next_Song_Votes'Img,
            Current_Room.Get_Next_Song_Votes'Img & " /" & Current_Room.Get_Number_Of_Clients'Img));
      Insert (Translations, Assoc (Nb_Clients'Img, Current_Room.Get_Number_Of_Clients'Img));
      Insert (Translations, Assoc (Song_List'Img, Build_Playlist (Current_Room, Current_Client)));
      Insert
        (Translations,
         Assoc (Suggestions_List'Img, Build_Song_List (Current_Room, Suggestions)));
      Insert (Translations, Assoc (Client_Sync'Img, Current_Client.Is_Sync_With_Room));
      Insert (Translations, Assoc (Server_Address'Img, To_String (Server_Ip)));
      Insert (Translations, Assoc (Chat_Log'Img, Current_Room.Get_Chat_Log));

      -- Room script
      Insert
        (Translations,
         Assoc (Room_Script'Img, To_String (Parse ("js/room.tjs", Translations))));

      -- Player script
      if Current_Client.Is_Player_Displayed and
        Current_Song.Get_Provider /= Api.No_Provider_Api
      then
         Insert (Player_Translations, Assoc (Song_Id'Img, Current_Song.Get_Id));
         Insert (Player_Translations, Assoc (Room_Name'Img, Current_Room.Get_Name));
         Insert
           (Translations,
            Assoc
              (Player_Script'Img,
               To_String (Parse ("js/youtube_player.tjs", Player_Translations))));
      else
         Insert (Translations, Assoc (Player_Script'Img, ""));
      end if;

      return Aws.Response.Build
          (Aws.Mime.Text_Html,
           To_String (Parse ("html/room.thtml", Translations)));
   end Room_Page_Callback;

   -------------------------------------------------------------------------------------------------
   -- Search_Button_Callback
   -------------------------------------------------------------------------------------------------
   function Search_Button_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
      Search_Results : constant Song.List.T_Song_List :=
        Current_Room.Get_Song_Search_Results
        (Session_Id                                   =>
           Aws.Status.Session (Request), Search_Input =>
           Aws.Status.Parameter (Request, To_Parameter_String (Param_Search_Input)));
   begin
      return Aws.Response.Build
          (Aws.Mime.Text_Xml,
           Pack_Ajax_Xml_Response
             (To_Placeholder_String (Ph_Search_Results),
              Build_Search_Results (Search_Results)));
   end Search_Button_Callback;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Playlist_Callback
   -------------------------------------------------------------------------------------------------
   function Add_To_Playlist_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      Current_Room.Add_Song_To_Playlist
      (Session_Id                               =>
         Aws.Status.Session (Request), New_Song =>
         Song.Initialize
           (Id             => Aws.Status.Parameter (Request, To_Parameter_String (Param_Song_Id)),
            Title => Aws.Status.Parameter (Request, To_Parameter_String (Param_Song_Title)),
            Thumbnail_Link =>
              Aws.Status.Parameter (Request, To_Parameter_String (Param_Song_Thumbnail_Link)),
            Provider => Api.Youtube_Api));

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Add_To_Playlist_Callback;

   -------------------------------------------------------------------------------------------------
   -- Remove_From_Playlist_Callback
   -------------------------------------------------------------------------------------------------
   function Remove_From_Playlist_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      Current_Room.Remove_Item_From_Playlist
      (Song.Item.T_Item_Id'Value
         (Aws.Status.Parameter (Request, To_Parameter_String (Param_Item_Id))));

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Remove_From_Playlist_Callback;

   -------------------------------------------------------------------------------------------------
   -- Add_Remove_Like_Callback
   -------------------------------------------------------------------------------------------------
   function Add_Remove_Like_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
      Modified_Song : constant Song.T_Song :=
        Song.Initialize
          (Id             => Aws.Status.Parameter (Request, To_Parameter_String (Param_Song_Id)),
           Title          => Aws.Status.Parameter (Request, To_Parameter_String (Param_Song_Title)),
           Thumbnail_Link =>
             Aws.Status.Parameter (Request, To_Parameter_String (Param_Song_Thumbnail_Link)),
           Provider => Api.Youtube_Api);

      Song_Liked : constant Boolean :=
        Boolean'Value (Aws.Status.Parameter (Request, To_Parameter_String (Param_Liked)));
   begin
      if Song_Liked then
         Current_Room.Remove_Like (Modified_Song);
      else
         Current_Room.Add_Like (Modified_Song);
      end if;

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Add_Remove_Like_Callback;

   -------------------------------------------------------------------------------------------------
   -- Player_Display_Checkbox_Callback
   -------------------------------------------------------------------------------------------------
   function Player_Display_Checkbox_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      Current_Room.Display_Client_Player
      (Session_Id                              =>
         Aws.Status.Session (Request), Display =>
         Boolean'Value (Aws.Status.Parameter (Request, To_Parameter_String (Param_Checked))));

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Player_Display_Checkbox_Callback;

   -------------------------------------------------------------------------------------------------
   -- Player_Sync_Checkbox_Callback
   -------------------------------------------------------------------------------------------------
   function Player_Sync_Checkbox_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      Current_Room.Sync_Client_With_Room
      (Session_Id                           =>
         Aws.Status.Session (Request), Sync =>
         Boolean'Value (Aws.Status.Parameter (Request, To_Parameter_String (Param_Checked))));

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Player_Sync_Checkbox_Callback;

   -------------------------------------------------------------------------------------------------
   -- Next_Room_Song_Callback
   -------------------------------------------------------------------------------------------------
   function Next_Room_Song_Callback
     (Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      Current_Room.Next_Song;

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Next_Room_Song_Callback;

   -------------------------------------------------------------------------------------------------
   -- Next_Client_Song_Callback
   -------------------------------------------------------------------------------------------------
   function Next_Client_Song_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      Current_Room.Get_Client (Aws.Status.Session (Request)).Next_Song;

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Next_Client_Song_Callback;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Callback
   -------------------------------------------------------------------------------------------------
   function Up_Vote_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      Current_Room.Up_Vote_Playlist_Item
      (Song.Item.T_Item_Id'Value
         (Aws.Status.Parameter (Request, To_Parameter_String (Param_Item_Id))));

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Up_Vote_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_List_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Song_List_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
      Source_To_Placeholder : constant array (T_Song_List_Source) of T_Placeholder :=
        (Suggestions => Ph_Suggestions_List, others => Ph_Song_List);

      Source : constant T_Song_List_Source :=
        T_Song_List_Source'Value
          (Aws.Status.Parameter (Request, To_Parameter_String (Param_Source)));

      Response : Unbounded_String := Null_Unbounded_String;
   begin
      case Source is
         when Playlist =>
            Response :=
              To_Unbounded_String
                (Build_Playlist
                   (Current_Room,
                    Current_Room.Get_Client (Aws.Status.Session (Request))));

         when others =>
            Response := To_Unbounded_String (Build_Song_List (Current_Room, Source));
      end case;

      return Aws.Response.Build
          (Aws.Mime.Text_Xml,
           Pack_Ajax_Xml_Response
             (To_Placeholder_String (Source_To_Placeholder (Source)),
              To_String (Response)));
   end Get_Song_List_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Room_Song_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Current_Room_Song_Callback
     (Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      return Aws.Response.Build
          (Aws.Mime.Text_Xml,
           Pack_Ajax_Xml_Response
             (To_Placeholder_String (Ph_Current_Room_Song),
              Current_Room.Get_Current_Song.Get_Title));
   end Get_Current_Room_Song_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Next_Song_Votes_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Next_Song_Votes_Callback
     (Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      return Aws.Response.Build
          (Aws.Mime.Text_Xml,
           Pack_Ajax_Xml_Response
             (To_Placeholder_String (Ph_Next_Room_Song_Votes),
              Current_Room.Get_Next_Song_Votes'Img &
              " /" &
              Current_Room.Get_Number_Of_Clients'Img));
   end Get_Next_Song_Votes_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Of_Clients_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Number_Of_Clients_Callback
     (Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      return Aws.Response.Build
          (Aws.Mime.Text_Xml,
           Pack_Ajax_Xml_Response
             (To_Placeholder_String (Ph_Nb_Clients),
              Current_Room.Get_Number_Of_Clients'Img));
   end Get_Number_Of_Clients_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Chat_Log_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Chat_Log_Callback
     (Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      return Aws.Response.Build
          (Aws.Mime.Text_Xml,
           Pack_Ajax_Xml_Response (To_Placeholder_String (Ph_Chat_Log), Current_Room.Get_Chat_Log));
   end Get_Chat_Log_Callback;

   -------------------------------------------------------------------------------------------------
   -- Add_Chat_Message_Callback
   -------------------------------------------------------------------------------------------------
   function Add_Chat_Message_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data
   is
   begin
      Current_Room.Add_Chat_Message
      (Sanitizer.Sanitize_Chat_Message
         (Aws.Status.Parameter (Request, To_Parameter_String (Param_Message))));

      return Aws.Response.Build (Aws.Mime.Text_Html, "");
   end Add_Chat_Message_Callback;

   -------------------------------------------------------------------------------------------------
   -- Build_Search_Results
   -------------------------------------------------------------------------------------------------
   function Build_Search_Results (Song_Search_Results : in Song.List.T_Song_List) return String is
      use Templates_Parser;

      Translations : Translate_Set;
      Response     : Unbounded_String := Null_Unbounded_String;

      ----------------------------------------------------------------------------------------------
      -- Build_Search_Results_Item
      ----------------------------------------------------------------------------------------------
      procedure Build_Search_Results_Item (Element : in Song.T_Song) is
      begin
         Insert (Translations, Assoc (Song_Id'Img, Element.Get_Id));
         Insert (Translations, Assoc (Song_Title'Img, Element.Get_Title));
         Insert (Translations, Assoc (Song_Thumbnail_Link'Img, Element.Get_Thumbnail_Link));

         Append (Response, To_String (Parse ("html/search_results_item.thtml", Translations)));
      end Build_Search_Results_Item;
   begin
      Song_Search_Results.Iterate (Build_Search_Results_Item'Access);

      return To_String (Response);
   end Build_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Build_Playlist
   -------------------------------------------------------------------------------------------------
   function Build_Playlist
     (Current_Room   : in not null Room.T_Room_Access;
      Current_Client : in not null Client.T_Client_Access) return String
   is
      use Templates_Parser;

      use type Aws.Session.Id;

      Translations : Translate_Set;
      Response     : Unbounded_String := Null_Unbounded_String;

      ----------------------------------------------------------------------------------------------
      -- Build_Playlist_Item
      ----------------------------------------------------------------------------------------------
      procedure Build_Playlist_Item (Element : in Song.Item.T_Item) is
      begin
         Insert
           (Translations,
            Assoc (Source_Client'Img, Current_Client.Get_Id = Element.Get_Client_Id));
         Insert (Translations, Assoc (Item_Id'Img, Trim (Element.Get_Id'Img, Ada.Strings.Left)));
         Insert (Translations, Assoc (Song_Id'Img, Element.Get_Song.Get_Id));
         Insert (Translations, Assoc (Song_Title'Img, Element.Get_Song.Get_Title));
         Insert
           (Translations,
            Assoc (Song_Thumbnail_Link'Img, Element.Get_Song.Get_Thumbnail_Link));

         if Current_Room.Is_Song_Liked (Element.Get_Song) then
            Insert (Translations, Assoc (Liked'Img, "s"));
         else
            Insert (Translations, Assoc (Liked'Img, "r"));
         end if;

         Insert
           (Translations,
            Assoc (Up_Votes'Img, Trim (Element.Get_Up_Votes'Img, Ada.Strings.Left)));

         Append (Response, To_String (Parse ("html/playlist_item.thtml", Translations)));
      end Build_Playlist_Item;

      Room_Playlist   : Song.Item.List.T_Item_List := Current_Room.Get_Playlist;
      Client_Playlist : Song.Item.List.T_Item_List := Current_Client.Get_Playlist;
   begin
      if Current_Client.Is_Sync_With_Room then
         if Room_Playlist.Is_Empty then
            Response := To_Unbounded_String (Build_Empty_Playlist (Current_Room, Current_Client));
         else
            Room_Playlist.Iterate (Build_Playlist_Item'Access);
         end if;
      else
         if Client_Playlist.Is_Empty then
            Response := To_Unbounded_String (Build_Empty_Playlist (Current_Room, Current_Client));
         else
            Client_Playlist.Iterate (Build_Playlist_Item'Access);
         end if;
      end if;

      return To_String (Response);
   end Build_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Build_Empty_Playlist
   -------------------------------------------------------------------------------------------------
   function Build_Empty_Playlist
     (Current_Room   : in not null Room.T_Room_Access;
      Current_Client : in not null Client.T_Client_Access) return String
   is
      use Templates_Parser;

      use type Api.T_Api_Provider;

      Translations : Translate_Set;
   begin
      if Current_Client.Is_Sync_With_Room
        and then Current_Room.Is_Auto_Playback_Requested
        and then not Current_Room.Get_Suggestions.Is_Empty
        and then Current_Room.Get_Current_Song.Get_Provider /= Api.No_Provider_Api
      then
         Insert (Translations, Assoc (Display_Next_Suggested_Song'Img, True));
         Insert
           (Translations,
            Assoc (Song_Id'Img, Current_Room.Get_Suggestions.First_Element.Get_Id));
         Insert
           (Translations,
            Assoc (Song_Title'Img, Current_Room.Get_Suggestions.First_Element.Get_Title));
         Insert
           (Translations,
            Assoc
              (Song_Thumbnail_Link'Img,
               Current_Room.Get_Suggestions.First_Element.Get_Thumbnail_Link));

         if Current_Room.Is_Song_Liked (Current_Room.Get_Suggestions.First_Element) then
            Insert (Translations, Assoc (Liked'Img, "s"));
         else
            Insert (Translations, Assoc (Liked'Img, "r"));
         end if;

         Insert
           (Translations,
            Assoc (Song_List'Img, To_String (Parse ("html/song_list_item.thtml", Translations))));
      else
         Insert (Translations, Assoc (Display_Next_Suggested_Song'Img, False));
      end if;

      return Parse ("html/playlist_empty.thtml", Translations);
   end Build_Empty_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Build_Song_List
   -------------------------------------------------------------------------------------------------
   function Build_Song_List
     (Current_Room : in not null Room.T_Room_Access;
      Source       : in T_Song_List_Source) return String
   is
      use Templates_Parser;

      Translations : Translate_Set;
      Response     : Unbounded_String := Null_Unbounded_String;

      ----------------------------------------------------------------------------------------------
      -- Build_Song_List_Item
      ----------------------------------------------------------------------------------------------
      procedure Build_Song_List_Item (Element : in Song.T_Song) is
      begin
         Insert (Translations, Assoc (Song_Id'Img, Element.Get_Id));
         Insert (Translations, Assoc (Song_Title'Img, Element.Get_Title));
         Insert (Translations, Assoc (Song_Thumbnail_Link'Img, Element.Get_Thumbnail_Link));

         if Source = Likes then
            Insert (Translations, Assoc (Liked'Img, "s"));
         elsif Current_Room.Is_Song_Liked (Element) then
            Insert (Translations, Assoc (Liked'Img, "s"));
         else
            Insert (Translations, Assoc (Liked'Img, "r"));
         end if;

         Append (Response, To_String (Parse ("html/song_list_item.thtml", Translations)));
      end Build_Song_List_Item;

      Songs : Song.List.T_Song_List := Song.List.Initialize;
   begin
      case Source is
         when History =>
            Songs := Current_Room.Get_History;
            Songs.Reverse_Iterate (Build_Song_List_Item'Access);

         when Likes =>
            Songs := Current_Room.Get_Likes;

            if not Songs.Is_Empty then
               Songs.Reverse_Iterate (Build_Song_List_Item'Access);
            else
               Response :=
                 To_Unbounded_String
                   ("<span>No song liked for the moment, add one by clicking on <i class=""far fa-heart""></i></span> of a song");
            end if;

         when Suggestions =>
            Songs := Current_Room.Get_Suggestions;

            if not Songs.Is_Empty then
               Songs.Iterate (Build_Song_List_Item'Access);
            else
               Response := To_Unbounded_String ("<span>No suggestions available</span>");
            end if;

         when others =>
            null;
      end case;

      return To_String (Response);
   end Build_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Pack_AJAX_XML_Response
   -------------------------------------------------------------------------------------------------
   function Pack_Ajax_Xml_Response
     (Xml_Placeholder : in String;
      Xml_Value       : in String) return String
   is
      use Templates_Parser;

      Translations : Translate_Set;

      Replace_Fields : Unbounded_String;
   begin
      -- Pack <replace> fields
      -- For now, only one field at a time is supported
      Insert (Translations, Templates_Parser.Assoc (Placeholder'Img, Xml_Placeholder));
      Insert (Translations, Templates_Parser.Assoc (Value'Img, Xml_Value));

      Replace_Fields :=
        To_Unbounded_String (Templates_Parser.Parse ("xml/ajax_xml_replace.txml", Translations));

      -- Pack global response
      Insert (Translations, Templates_Parser.Assoc (Action_Fields'Img, Replace_Fields));

      return Templates_Parser.Parse ("xml/ajax_xml_response.txml", Translations);
   end Pack_Ajax_Xml_Response;

   -------------------------------------------------------------------------------------------------
   -- To_Parameter_String
   -------------------------------------------------------------------------------------------------
   function To_Parameter_String (Parameter : in T_Parameter) return String is
      Parameter_String : constant String := Ada.Characters.Handling.To_Lower (Parameter'Img);
   begin
      return Parameter_String (Parameter_String'First + 6 .. Parameter_String'Last);
   end To_Parameter_String;

   -------------------------------------------------------------------------------------------------
   -- To_Placeholder_String
   -------------------------------------------------------------------------------------------------
   function To_Placeholder_String (Xml_Placeholder : in T_Placeholder) return String is
      Placeholder_String : constant String :=
        Ada.Characters.Handling.To_Lower (Xml_Placeholder'Img);
   begin
      return Placeholder_String (Placeholder_String'First + 3 .. Placeholder_String'Last);
   end To_Placeholder_String;

end Callback_Room;
