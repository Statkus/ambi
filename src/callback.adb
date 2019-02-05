with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Parameters;

with Templates_Parser;

package body Callback is

   -------------------------------------------------------------------------------------------------
   -- Ambi_Callback
   -------------------------------------------------------------------------------------------------
   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      URI        : constant String := AWS.Status.URI (Request);
      Parameters : constant AWS.Parameters.List := AWS.Status.Parameters (Request);

      Translations : Templates_Parser.Translate_Table (1 .. 1);

      Web_Page : Unbounded_String;
   begin
      Put_Line ("New Ambi callback --------------------------------------------------------------");

      if URI = "/search_result" then
         return Search_Result_Callback (Request);
      elsif URI = "/1" then
         Video_ID := Video_Search_List_Response.Video_Search_Results (1).Video_ID;
      elsif URI = "/2" then
         Video_ID := Video_Search_List_Response.Video_Search_Results (2).Video_ID;
      elsif URI = "/3" then
         Video_ID := Video_Search_List_Response.Video_Search_Results (3).Video_ID;
      elsif URI = "/4" then
         Video_ID := Video_Search_List_Response.Video_Search_Results (4).Video_ID;
      elsif URI = "/5" then
         Video_ID := Video_Search_List_Response.Video_Search_Results (5).Video_ID;
      end if;

      Put_Line ("Video ID to play: " & Video_ID);

      Translations (1) := Templates_Parser.Assoc ("VIDEO_ID", Video_ID);

      Web_Page := To_Unbounded_String (Templates_Parser.Parse ("html/ambi.thtml", Translations));
      --Put_Line (To_String (Web_Page));

      return AWS.Response.Build (AWS.MIME.Text_HTML, To_String (Web_Page));
   end Ambi_Callback;

   -------------------------------------------------------------------------------------------------
   -- Search_Result_Callback
   -------------------------------------------------------------------------------------------------
   function Search_Result_Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      Parameters  : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      Search_Input : constant String := AWS.Parameters.Get (Parameters, "search_input");

      API_Request : String := Get_Search_Request (Search_Input);
      API_Result  : AWS.Response.DATA;

   begin
      Put_Line ("New Search_Result callback -----------------------------------------------------");
      Put_Line ("API query: " & API_Request);
      API_Result := AWS.Client.Get (URL => API_Request);
      --Put_Line ("API result:");
      --Put_Line (AWS.Response.Message_Body (API_Result));

      Video_Search_List_Response :=
        Parse_Video_Search_List_Request (AWS.Response.Message_Body (API_Result));

      for Index in 1 .. YT_VIDEO_SEARCH_RESULT loop
         Put_Line (To_String (Video_Search_List_Response.Video_Search_Results (Index).Video_Title));
      end loop;

      return AWS.Response.Build (AWS.MIME.Text_HTML,
                                    Build_Search_Result (Video_Search_List_Response));
   end Search_Result_Callback;

   -------------------------------------------------------------------------------------------------
   -- Build_Search_Result
   -------------------------------------------------------------------------------------------------
   function Build_Search_Result
     (Video_Search_List_Response : in T_Video_Search_List_Response) return String is
      Translations : Templates_Parser.Translate_Table
        (1 .. Video_Search_List_Response.Results_Per_Page * 2);
   begin
      for Index in 1 .. Video_Search_List_Response.Results_Per_Page loop
         Translations (Index) := Templates_Parser.Assoc
           ("IMAGE_URL_" & Trim (Index'Img, Ada.Strings.Left),
            Video_Search_List_Response.Video_Search_Results (Index).Video_Image_URL);
         Translations (Video_Search_List_Response.Results_Per_Page + Index) := Templates_Parser.Assoc
           ("VIDEO_TITLE_" & Trim (Index'Img, Ada.Strings.Left),
            To_String (Video_Search_List_Response.Video_Search_Results (Index).Video_Title));
      end loop;

      return Templates_Parser.Parse ("html/search_result.thtml", Translations);
   end Build_Search_Result;

end Callback;
