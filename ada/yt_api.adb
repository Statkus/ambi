with GNAT.Regpat; use GNAT.Regpat;

with AWS.Client;
with AWS.Response;

with JSON.Streams;

with Ada.Text_IO; use Ada.Text_IO;

package body YT_API is

   -------------------------------------------------------------------------------------------------
   -- Set_YT_API_Key
   -------------------------------------------------------------------------------------------------
   procedure Set_YT_API_Key (Key : in String) is
   begin
      YT_API_KEY := To_Unbounded_String (Key);
   end Set_YT_API_Key;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Video_Search_Results (Search_Input : in String) return T_Video_Search_Results is
   begin
      Put_Line ("YT API query: " & Get_Search_Request (Search_Input));

      return Parse_Video_Search_Results (AWS.Response.Message_Body
        (AWS.Client.Get (URL => Get_Search_Request (Search_Input))));
   end Get_Video_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Video_Duration (Video : in T_Video) return Natural is
   begin
      Put_Line ("YT API query: " & Get_Video_Request (To_String (Video.Video_ID)));

      return Parse_Video_Duration_Result (AWS.Response.Message_Body
        (AWS.Client.Get (URL => Get_Video_Request (To_String (Video.Video_ID)))));
   end Get_Video_Duration;

   -------------------------------------------------------------------------------------------------
   -- Get_Search_Request
   -------------------------------------------------------------------------------------------------
   function Get_Search_Request (Search_Input : in String) return String is
   begin
      return YT_API_URL & "search?key=" & To_String (YT_API_KEY)
        & "&q=" & Search_Input
        & "&maxResult=" & MAX_VIDEO_SEARCH_RESULTS'Img
        & "&part=snippet&videoDefinition=any&type=video&order=relevance&safeSearch=none"
        & "&videoEmbeddable=true&videoSyndicated=true";
   end Get_Search_Request;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Request
   -------------------------------------------------------------------------------------------------
   function Get_Video_Request (Video_ID : in String) return String is
   begin
      return YT_API_URL & "videos?key=" & To_String (YT_API_KEY)
        & "&id=" & Video_ID
        & "&part=contentDetails";
   end Get_Video_Request;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Search_Results (Search_Results : in String) return T_Video_Search_Results is
      JSON_String_Response : aliased String := Search_Results;

      JSON_Stream    : JSON.Streams.Stream'Class :=
        JSON.Streams.Create_Stream (JSON_String_Response'Access);
      JSON_Allocator : Types.Memory_Allocator;
      JSON_Result    : constant Types.JSON_Value := Parsers.Parse (JSON_Stream, JSON_Allocator);

      Video_Search_Results       : T_Video_Search_Results;
      Video_Search_Results_Index : Integer := Video_Search_Results'First;
   begin
      for Item of JSON_Result.Get ("items") loop
         Video_Search_Results (Video_Search_Results_Index).Video_ID :=
           To_Unbounded_String (Item.Get ("id").Get ("videoId").Value);

         Video_Search_Results (Video_Search_Results_Index).Video_Title :=
           To_Unbounded_String (Item.Get ("snippet").Get ("title").Value);

         Video_Search_Results (Video_Search_Results_Index).Video_Image_URL :=
           To_Unbounded_String
             (Item.Get ("snippet").Get ("thumbnails").Get ("default").Get ("url").Value);

         Video_Search_Results_Index := Video_Search_Results_Index + 1;
      end loop;

      return Video_Search_Results;
   end Parse_Video_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Duration_Result
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Duration_Result (Search_Result : in String) return Natural is
      JSON_String_Response : aliased String := Search_Result;

      JSON_Stream    : JSON.Streams.Stream'Class :=
        JSON.Streams.Create_Stream (JSON_String_Response'Access);
      JSON_Allocator : Types.Memory_Allocator;
      JSON_Result    : constant Types.JSON_Value := Parsers.Parse (JSON_Stream, JSON_Allocator);

      Duration : Natural := 0;
   begin
      for Item of JSON_Result.Get ("items") loop
         -- Normally there is only one item
         Duration := Parse_Duration (Item.Get ("contentDetails").Get ("duration").Value);
      end loop;

      return Duration;
   end Parse_Video_Duration_Result;

   -------------------------------------------------------------------------------------------------
   -- Parse_Duration
   -------------------------------------------------------------------------------------------------
   function Parse_Duration (Duration_String : in String) return Natural is
      Minutes_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("([0-9]+)M");
      Seconds_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("([0-9]+)S");
      Result  : GNAT.Regpat.Match_Array (0 .. 1);

      Minutes : Natural := 0;
      Seconds : Natural := 0;
   begin
      GNAT.Regpat.Match (Minutes_Pattern, Duration_String, Result);

      if Result (1) /= GNAT.Regpat.No_Match then
         Minutes := Natural'Value (Duration_String (Result (1).First .. Result (1).Last));
      end if;

      GNAT.Regpat.Match (Seconds_Pattern, Duration_String, Result);

      if Result (1) /= GNAT.Regpat.No_Match then
         Seconds := Natural'Value (Duration_String (Result (1).First .. Result (1).Last));
      end if;

      return Minutes * 60 + Seconds;
   end Parse_Duration;

end YT_API;
