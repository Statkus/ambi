with GNAT.Regpat; use GNAT.Regpat;

package body YT_API is

   -------------------------------------------------------------------------------------------------
   -- Set_YT_API_Key
   -------------------------------------------------------------------------------------------------
   procedure Set_YT_API_Key (Key : String) is
   begin
      YT_API_KEY := Key;
   end Set_YT_API_Key;

   -------------------------------------------------------------------------------------------------
   -- Get_Search_Request
   -------------------------------------------------------------------------------------------------
   function Get_Search_Request (Search_Input : in String) return String is
   begin
      return YT_API_URL & "search?key=" & YT_API_KEY
        & "&part=snippet&q=" & Search_Input & "&videoDefinition=high&type=video&order=viewCount";
   End Get_Search_Request;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Search_List_Request
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Search_List_Request (Search_Result : in String)
     return T_Video_Search_List_Response is

      Next_Page_Token  : String (1 .. YT_PAGE_TOKEN_LENGTH) := (others => '0');
      Total_Results    : Integer := 0;
      Results_Per_Page : Integer := 0;

      Video_Search_Results : T_Video_Search_Results;
      Video_Search_Offset : Natural := 0;

      Field_Not_Found : Exception;
   begin
      -- Parse next page token
      Next_Page_Token := REGEX_Match_To_String
        ("""nextPageToken"": ""([A-Z]+)""", Search_Result, Video_Search_Offset);

      if Next_Page_Token = "" then
         raise Field_Not_Found with "Next page token not found";
      end if;

      -- Parse total results
      Total_Results := REGEX_Match_To_Integer
        ("""totalResults"": ([0-9]+)", Search_Result, Video_Search_Offset);

      if Total_Results = Integer'First then
         raise Field_Not_Found with "Total results not found";
      end if;

      -- Parse result per page
      Results_Per_Page := REGEX_Match_To_Integer
        ("""resultsPerPage"": ([0-9]+)", Search_Result, Video_Search_Offset);

      if Results_Per_Page = Integer'First then
         raise Field_Not_Found with "Results per page not found";
      end if;

      -- Parse video search results
      for Index in 1 .. Results_Per_Page loop
         Video_Search_Results (Index).Video_ID :=
           REGEX_Match_To_String ("""videoId"": ""([a-zA-Z0-9_-]+)""", Search_Result
               (Search_Result'First + Video_Search_Offset .. Search_Result'Last),
             Video_Search_Offset);

         Video_Search_Results (Index).Video_Title :=
           To_Unbounded_String (
             REGEX_Match_To_String ("""title"": ""(.+)"",", Search_Result
                 (Search_Result'First + Video_Search_Offset .. Search_Result'Last),
               Video_Search_Offset));

         if Video_Search_Results (Index).Video_Title = "" then
           raise Field_Not_Found with "Video title not found";
         end if;

         Video_Search_Results (Index).Video_Image_URL :=
           To_Unbounded_String (
             REGEX_Match_To_String
               ("""url"": ""(https://i.ytimg.com/vi/[a-zA-Z0-9_-]+/default.jpg)"",",
                Search_Result
                  (Search_Result'First + Video_Search_Offset .. Search_Result'Last),
                Video_Search_Offset));

         if Video_Search_Results (Index).Video_Image_URL = "" then
           raise Field_Not_Found with "Video image URL not found";
         end if;
      end loop;

      return (Next_Page_Token, Total_Results, Results_Per_Page, Video_Search_Results);
   end Parse_Video_Search_List_Request;

   -------------------------------------------------------------------------------------------------
   -- REGEX_Match_To_String
   -------------------------------------------------------------------------------------------------
   function REGEX_Match_To_String
     (Pattern : in String;
      Source  : in String;
      Offset  : out Integer)
     return String is
      Match_Pattern : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile (Pattern);
      Match_Result  : GNAT.Regpat.Match_Array (0 .. 1);
   begin
      Offset := Source'First;

      GNAT.Regpat.Match (Match_Pattern, Source, Match_Result);

      if Match_Result (1) /= GNAT.Regpat.No_Match then
         Offset := Match_Result (1).Last;
         return Source (Match_Result (1).First .. Match_Result (1).Last);
      else
         return "";
      end if;
   end REGEX_Match_To_String;

   -------------------------------------------------------------------------------------------------
   -- REGEX_Match_To_Integer
   -------------------------------------------------------------------------------------------------
   function REGEX_Match_To_Integer
     (Pattern : in String;
      Source  : in String;
      Offset  : out Integer)
     return Integer is
      Match_Pattern : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile (Pattern);
      Match_Result  : GNAT.Regpat.Match_Array (0 .. 1);
   begin
      Offset := Source'First;

      GNAT.Regpat.Match (Match_Pattern, Source, Match_Result);

      if Match_Result (1) /= GNAT.Regpat.No_Match then
         Offset := Match_Result (1).Last;
         return Integer'Value (Source (Match_Result (1).First .. Match_Result (1).Last));
      else
         return Integer'First;
      end if;
   end REGEX_Match_To_Integer;

end YT_API;
