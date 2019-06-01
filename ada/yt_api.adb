with Ada.Strings.Fixed; use Ada.Strings.Fixed;

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
   function Get_Video_Search_Results (Search_Input : in String) return Video_Vectors.Vector is
   begin
      return Parse_Video_Search_Results (Get_Request_Response (Get_Search_Request (Search_Input)));

   exception
      when others => return Video_Vectors.Empty_Vector;
   end Get_Video_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Video_Duration (Video : in T_Video) return Natural is
   begin
      return Parse_Video_Duration_Result
        (Get_Request_Response (Get_Video_Request (To_String (Video.Video_ID))));

   exception
      when others => return Natural'First;
   end Get_Video_Duration;

   -------------------------------------------------------------------------------------------------
   -- Get_Videos_Related
   -------------------------------------------------------------------------------------------------
   function Get_Videos_Related (Video : in T_Video) return Video_Vectors.Vector is
   begin
      return Parse_Video_Search_Results
       (Get_Request_Response (Get_Videos_Related_Request (To_String (Video.Video_ID))));

   exception
      when others => return Video_Vectors.Empty_Vector;
   end Get_Videos_Related;

   -------------------------------------------------------------------------------------------------
   -- Get_Request_Response
   -------------------------------------------------------------------------------------------------
   function Get_Request_Response (URL_Request : in String) return String is
      Response      : Unbounded_String := To_Unbounded_String ("GET request error.");
      Number_Of_Try : Natural := Natural'First;
   begin
      Put_Line ("YT API request: " & URL_Request);

      while Index (To_String (Response), "GET request error.") = 1
        and Number_Of_Try < MAX_NUMBER_OF_REQUEST_RETRY loop
         Response :=
           To_Unbounded_String (AWS.Response.Message_Body (AWS.Client.Get (URL => URL_Request)));

         Number_Of_Try := Number_Of_Try + 1;
      end loop;

      if Index (To_String (Response), "GET request error.") = 1 then
         Put_Line ("YT API request not answered after" & Number_Of_Try'Img & " retry");
      end if;

      return To_String (Response);
   end Get_Request_Response;

   -------------------------------------------------------------------------------------------------
   -- Get_Search_Request
   -------------------------------------------------------------------------------------------------
   function Get_Search_Request (Search_Input : in String) return String is
   begin
      return YT_API_URL & "search?key=" & To_String (YT_API_KEY)
        & "&q=" & Search_Input
        & "&maxResults=" & MAX_VIDEO_SEARCH_RESULTS
        & "&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true";
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
   -- Get_Videos_Related_Request
   -------------------------------------------------------------------------------------------------
   function Get_Videos_Related_Request (Video_ID : in String) return String is
   begin
      return YT_API_URL & "search?key=" & To_String (YT_API_KEY)
        & "&relatedToVideoId=" & Video_ID
        & "&maxResults=" & MAX_VIDEO_SEARCH_RESULTS
        & "&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true";
   end Get_Videos_Related_Request;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Search_Results (Search_Results : in String) return Video_Vectors.Vector is
      JSON_String_Response : aliased String := Search_Results;

      JSON_Stream    : JSON.Streams.Stream'Class :=
        JSON.Streams.Create_Stream (JSON_String_Response'Access);
      JSON_Allocator : Types.Memory_Allocator;
      JSON_Result    : constant Types.JSON_Value := Parsers.Parse (JSON_Stream, JSON_Allocator);

      Video         : T_Video;
      Videos_Result : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
   begin
      for Item of JSON_Result.Get ("items") loop
         Video.Video_ID := To_Unbounded_String (Item.Get ("id").Get ("videoId").Value);

         Video.Video_Title := To_Unbounded_String (Item.Get ("snippet").Get ("title").Value);

         Video.Video_Thumbnail :=
           To_Unbounded_String
             (Item.Get ("snippet").Get ("thumbnails").Get ("default").Get ("url").Value);

         Videos_Result.Append (Video);
      end loop;

      return Videos_Result;
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
      Hours_Pattern   : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("([0-9]+)H");
      Minutes_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("([0-9]+)M");
      Seconds_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("([0-9]+)S");
      Result  : GNAT.Regpat.Match_Array (0 .. 1);

      Hours   : Natural := 0;
      Minutes : Natural := 0;
      Seconds : Natural := 0;
   begin
      GNAT.Regpat.Match (Hours_Pattern, Duration_String, Result);

      if Result (1) /= GNAT.Regpat.No_Match then
         Hours := Natural'Value (Duration_String (Result (1).First .. Result (1).Last));
      end if;

      GNAT.Regpat.Match (Minutes_Pattern, Duration_String, Result);

      if Result (1) /= GNAT.Regpat.No_Match then
         Minutes := Natural'Value (Duration_String (Result (1).First .. Result (1).Last));
      end if;

      GNAT.Regpat.Match (Seconds_Pattern, Duration_String, Result);

      if Result (1) /= GNAT.Regpat.No_Match then
         Seconds := Natural'Value (Duration_String (Result (1).First .. Result (1).Last));
      end if;

      return (Hours * 3600) + (Minutes * 60) + Seconds;
   end Parse_Duration;

end YT_API;
