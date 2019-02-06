with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

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
      Parameters : constant AWS.Parameters.List := AWS.Status.Parameters (Request);

      Translations : Templates_Parser.Translate_Table (1 .. 1);

      Web_Page : Unbounded_String;
   begin
      Put_Line ("New Ambi callback --------------------------------------------------------------");

      if URI = "/search_result" then
         return Search_Result_Callback (Request);
      elsif URI = "/1" then
         Current_Room.Set_Current_Video (1);
      elsif URI = "/2" then
         Current_Room.Set_Current_Video (2);
      elsif URI = "/3" then
         Current_Room.Set_Current_Video (3);
      elsif URI = "/4" then
         Current_Room.Set_Current_Video (4);
      elsif URI = "/5" then
         Current_Room.Set_Current_Video (5);
      end if;

      Put_Line ("Video to play: " & To_String (Current_Room.Get_Current_Video.Video_Title));

      Translations (1) := Templates_Parser.Assoc
        ("VIDEO_ID", To_String (Current_Room.Get_Current_Video.Video_ID));

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

      API_Request  : String := Get_Search_Request (Search_Input);
      API_Response : AWS.Response.DATA;
   begin
      Put_Line ("New Search_Result callback -----------------------------------------------------");
      Put_Line ("API query: " & API_Request);
      API_Response := AWS.Client.Get (URL => API_Request);

      Current_Room.Set_Video_Search_Results
        (Parse_Video_Search_Results (AWS.Response.Message_Body (API_Response)));

      for Index in Current_Room.Get_Video_Search_Results'Range loop
         Put_Line (To_String (Current_Room.Get_Video_Search_Results (Index).Video_Title));
      end loop;

      return AWS.Response.Build (AWS.MIME.Text_HTML,
                                 Build_Search_Result (Current_Room.Get_Video_Search_Results));
   end Search_Result_Callback;

   -------------------------------------------------------------------------------------------------
   -- Build_Search_Result
   -------------------------------------------------------------------------------------------------
   function Build_Search_Result
     (Video_Search_Results : in T_Video_Search_Results) return String is
      Translations : Templates_Parser.Translate_Table
        (1 .. Current_Room.Get_Video_Search_Results'Length * 2);
   begin
      for Index in Current_Room.Get_Video_Search_Results'Range loop
         Translations (Index) := Templates_Parser.Assoc
           ("IMAGE_URL_" & Trim (Index'Img, Ada.Strings.Left),
            To_String (Current_Room.Get_Video_Search_Results (Index).Video_Image_URL));
         Translations (Current_Room.Get_Video_Search_Results'Length + Index) :=
           Templates_Parser.Assoc
             ("VIDEO_TITLE_" & Trim (Index'Img, Ada.Strings.Left),
              To_String (Current_Room.Get_Video_Search_Results (Index).Video_Title));
      end loop;

      return Templates_Parser.Parse ("html/search_result.thtml", Translations);
   end Build_Search_Result;

end Callback;
