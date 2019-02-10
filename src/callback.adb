with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

with AWS.Client;
with AWS.MIME;
with AWS.Parameters;

with Templates_Parser;

with Room;

package body Callback is

   Current_Room : Room.T_Room;

   -------------------------------------------------------------------------------------------------
   -- Ambi_Callback
   -------------------------------------------------------------------------------------------------
   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      URI        : constant String := AWS.Status.URI (Request);

      Translations : Templates_Parser.Translate_Table (1 .. 1);

      Web_Page : Unbounded_String;
   begin
      Put_Line (URI);

      if Index (URI, "we_js") > 0 then
         return Javascripts_Callback (Request);
      elsif URI = "/onclick$search_button" then
         return Search_Button_Callback (Request);
      elsif Index (URI, "/onclick$search_result_item_") > 0 then
         Current_Room.Set_Current_Video (Integer'Value (URI (URI'Last .. URI'Last)));
         return AWS.Response.Build (AWS.MIME.Text_HTML, "");
      end if;

      Put_Line ("Video to play: " & To_String (Current_Room.Get_Current_Video.Video_Title));

      Translations (1) := Templates_Parser.Assoc
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

      Dummy_Translations : Templates_Parser.Translate_Table (1 .. 1);

      JS_Match_Pattern : GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile ("/we_js/([a-zA-Z0-9_-]+\.js)");
      TJS_Match_Pattern : GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile ("/we_js/([a-zA-Z0-9_-]+\.tjs)");

      Match_Result : GNAT.Regpat.Match_Array (0 .. 1);
   begin
      GNAT.Regpat.Match (JS_Match_Pattern, URI, Match_Result);

      if Match_Result (1) /= GNAT.Regpat.No_Match then
         return AWS.Response.File (AWS.MIME.Text_Javascript,
             "javascripts/" & URI (Match_Result (1).First .. Match_Result (1).Last));
      end if;

      GNAT.Regpat.Match (TJS_Match_Pattern, URI, Match_Result);

      if Match_Result (1) /= GNAT.Regpat.No_Match then
         return AWS.Response.Build (AWS.MIME.Text_Javascript,
             To_String (Templates_Parser.Parse
               ("javascripts/" & URI (Match_Result (1).First .. Match_Result (1).Last),
                Dummy_Translations)));
      end if;

      return AWS.Response.Build (AWS.MIME.Text_Javascript, "");
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

      return AWS.Response.Build (AWS.MIME.Text_HTML,
                                 Build_Search_Result (Current_Room.Get_Video_Search_Results));
   end Search_Button_Callback;

   -------------------------------------------------------------------------------------------------
   -- Build_Search_Result
   -------------------------------------------------------------------------------------------------
   function Build_Search_Result
     (Video_Search_Results : in T_Video_Search_Results) return String is
      Translations : Templates_Parser.Translate_Table
        (1 .. 3);
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

end Callback;
