with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Parameters;

with Templates_Parser;

with Room; use Room;

package body Callback is

   Current_Room : Room.T_Room;

   -------------------------------------------------------------------------------------------------
   -- Ambi_Callback
   -------------------------------------------------------------------------------------------------
   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);

      Translations : Templates_Parser.Translate_Table (1 .. 2);

      Web_Page : Unbounded_String;
   begin
      Put_Line (URI);

      if Index (URI, "/javascripts/") > 0 then
         return Javascripts_Callback (Request);
      elsif URI = "/onclick$search_button" then
         return Search_Button_Callback (Request);
      elsif Index (URI, "/onclick$search_result_item_") > 0 then
         return Search_Result_Item_Callback (Request);
      elsif URI = "/play_first_video" then
         Current_Room.Set_Current_Video;
         return AWS.Response.Build (AWS.MIME.Text_HTML, "");
      elsif URI = "/remove_first_video" then
         Current_Room.Remove_First_Playlist_Video;
         return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
             ("playlist", Build_Playlist (Current_Room.Get_Playlist)));
      end if;

      Put_Line ("Video to play: " & To_String (Current_Room.Get_Current_Video.Video_Title));

      Translations (1) := Templates_Parser.Assoc
        ("PLAYLIST", Build_Playlist (Current_Room.Get_Playlist));

      Translations (2) := Templates_Parser.Assoc
        ("VIDEO_ID", To_String (Current_Room.Get_Current_Video.Video_ID));

      Web_Page := To_Unbounded_String (Templates_Parser.Parse ("html/ambi.thtml", Translations));
      --Put_Line (To_String (Web_Page));

      return AWS.Response.Build (AWS.MIME.Text_HTML, To_String (Web_Page));
   end Ambi_Callback;

   -------------------------------------------------------------------------------------------------
   -- Javascripts_Callback
   -------------------------------------------------------------------------------------------------
   function Javascripts_Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      return AWS.Response.File (AWS.MIME.Text_Javascript, URI (URI'First + 1 .. URI'Last));
   end Javascripts_Callback;

   -------------------------------------------------------------------------------------------------
   -- Search_Button_Callback
   -------------------------------------------------------------------------------------------------
   function Search_Button_Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      Parameters   : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Search_Input : constant String := AWS.Parameters.Get (Parameters, "search_input");

      API_Request  : String := Get_Search_Request (Search_Input);
      API_Response : AWS.Response.DATA;
   begin
      Put_Line ("API query: " & API_Request);
      API_Response := AWS.Client.Get (URL => API_Request);

      Current_Room.Set_Video_Search_Results
        (Parse_Video_Search_Results (AWS.Response.Message_Body (API_Response)));

      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
          ("search_results_list", Build_Search_Result (Current_Room.Get_Video_Search_Results)));
   end Search_Button_Callback;

   -------------------------------------------------------------------------------------------------
   -- Search_Result_Item_Callback
   -------------------------------------------------------------------------------------------------
   function Search_Result_Item_Callback(Request : AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      Current_Room.Add_Video_To_Playlist
        (Current_Room.Get_Video_Search_Results (Integer'Value (URI (URI'Last .. URI'Last))));

      return AWS.Response.Build (AWS.MIME.Text_XML, Pack_AJAX_XML_Response
          ("playlist", Build_Playlist (Current_Room.Get_Playlist)));
   end Search_Result_Item_Callback;

   -------------------------------------------------------------------------------------------------
   -- Build_Search_Result
   -------------------------------------------------------------------------------------------------
   function Build_Search_Result (Video_Search_Results : in T_Video_Search_Results) return String is
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
   function Build_Playlist (Playlist : in Room.Video_Vectors.Vector) return String is
      Translations : Templates_Parser.Translate_Table (1 .. 3);
      Response : Unbounded_String := To_Unbounded_String ("<ul>");
      Playlist_Cursor : Room.Video_Vectors.Cursor := Playlist.First;
   begin
      while Video_Vectors.Has_Element (Playlist_Cursor) loop
         Translations (1) := Templates_Parser.Assoc
           ("ITEM_ID", Trim (Integer'Image (Video_Vectors.To_Index (Playlist_Cursor)), Ada.Strings.Left));

         Translations (2) := Templates_Parser.Assoc
           ("IMAGE_URL", Video_Vectors.Element (Playlist_Cursor).Video_Image_URL);

         Translations (3) := Templates_Parser.Assoc
           ("VIDEO_TITLE", Video_Vectors.Element (Playlist_Cursor).Video_Title);

         Append (Response, To_String
           (Templates_Parser.Parse ("html/playlist_item.thtml", Translations)));

         Playlist_Cursor := Video_Vectors.Next (Playlist_Cursor);
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

end Callback;
