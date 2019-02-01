with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;

with AWS.Client;
with AWS.MIME;
with AWS.Parameters;

with Templates_Parser;

with GNAT.Regpat; use GNAT.Regpat;

package body Callback is

   -------------------------------------------------------------------------------------------------
   -- Ambi_Callback
   -------------------------------------------------------------------------------------------------
   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      URI        : constant String := AWS.Status.URI (Request);
      Parameters : constant AWS.Parameters.List := AWS.Status.Parameters (Request);

      Search_Input : constant String := AWS.Parameters.Get (Parameters, "search_input");
      Video_ID : String (1 .. YT_VIDEO_ID_LENGTH) := (others => '0');

      Translations : Templates_Parser.Translate_Table (1 .. 1);

      Web_Page : Unbounded_String;

      API_Request : String := YT_API_URL & "search?key=" & YT_API_KEY
        & "&part=snippet&q=" & Search_Input & "&videoDefinition=high&type=video&order=viewCount";
      API_Result  : AWS.Response.DATA;

      Video_ID_Search_Result : T_Video_ID_Search_Result := (others => (others => '0'));
   begin
      Put_Line ("NEW REQUEST ----------------------------------------------------------------");
      Put_Line ("API query: " & API_Request);
      API_Result := AWS.Client.Get (URL => API_Request);
      Put_Line ("API result:");
      Put_Line (AWS.Response.Message_Body (API_Result));


      Video_ID_Search_Result := Parse_Search_Request (AWS.Response.Message_Body (API_Result));

      for Index in 1 .. YT_VIDEO_SEARCH_RESULT loop
         Put_Line (Video_ID_Search_Result (Index));
      end loop;

      if URI = "/search_result" then
         Put_Line ("coucou");
         return AWS.Response.Build (AWS.MIME.Text_HTML,
                                    Build_Search_Result (Video_ID_Search_Result));
      end if;

      Video_ID := Video_ID_Search_Result (Video_ID_Search_Result'First);

      Put_Line ("Search input: " & Search_Input);
      Put_Line ("Video ID: " & Video_ID);

      Translations (1) := Templates_Parser.Assoc ("VIDEO_ID", Video_ID);

      Web_Page := To_Unbounded_String (Templates_Parser.Parse ("html/ambi.thtml", Translations));
      --Put_Line (To_String (Web_Page));

      return AWS.Response.Build (AWS.MIME.Text_HTML, To_String (Web_Page));
   end Ambi_Callback;

   -------------------------------------------------------------------------------------------------
   -- Set_YT_API_Key
   -------------------------------------------------------------------------------------------------
   procedure Set_YT_API_Key (Key : String) is
   begin
      YT_API_KEY := Key;
   end Set_YT_API_Key;

   -------------------------------------------------------------------------------------------------
   -- Parse_Search_Request
   -------------------------------------------------------------------------------------------------
   function Parse_Search_Request (Search_Result : in String) return T_Video_ID_Search_Result is
      Video_ID_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile ("""videoId"": ""([a-zA-Z0-9_-]+)""");

      REGEX_Search_Offset : Natural := 0;

      Result : GNAT.Regpat.Match_Array (0 .. 1);

      Video_ID_Search_Result : T_Video_ID_Search_Result := (others => (others => '0'));
   begin
      for Index in Video_ID_Search_Result'Range loop
         GNAT.Regpat.Match (Video_ID_Pattern, Search_Result
           (Search_Result'First + REGEX_Search_Offset .. Search_Result'Last), Result);

         if Result (1) /= GNAT.Regpat.No_Match then
            Put_Line ("Match :" & Search_Result (Result (1).First .. Result (1).Last));
            Video_ID_Search_Result (Index)
              (Video_ID_Search_Result (Index)'First .. Video_ID_Search_Result (Index)'Last) :=
              Search_Result (Result (1).First .. Result (1).Last);

            REGEX_Search_Offset := Result (1).Last;
         end if;
      end loop;

      return Video_ID_Search_Result;
   end Parse_Search_Request;

   function Build_Search_Result
     (Video_ID_Search_Result : in T_Video_ID_Search_Result) return String is
      Translations : Templates_Parser.Translate_Table (Video_ID_Search_Result'Range);
   begin
      for Index in Video_ID_Search_Result'Range loop
         Translations (Index) := Templates_Parser.Assoc
           ("VIDEO_ID_" & Trim (Index'Img, Ada.Strings.Left), Video_ID_Search_Result (Index));
      end loop;

      Put_Line (Templates_Parser.Parse ("html/search_result.thtml", Translations));

      return Templates_Parser.Parse ("html/search_result.thtml", Translations);
   end Build_Search_Result;

end Callback;
