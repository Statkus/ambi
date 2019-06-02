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
   function Get_Video_Search_Results (Search_Input : in String; Search_Type : out T_Search_Type)
     return Video_Vectors.Vector is
      Video_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile
        ("www\.youtube\.com/watch\?v=([\w-]+)");
      Playlist_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile
        ("www\.youtube\.com/playlist\?list=([\w-]+)");

      Video_Match_Result    : GNAT.Regpat.Match_Array (0 .. 1);
      Playlist_Match_Result : GNAT.Regpat.Match_Array (0 .. 1);

      Video_Search_Results : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
   begin
      GNAT.Regpat.Match (Video_Pattern, Search_Input, Video_Match_Result);
      GNAT.Regpat.Match (Playlist_Pattern, Search_Input, Playlist_Match_Result);

      if Video_Match_Result (1) /= GNAT.Regpat.No_Match then
         Search_Type := Video_Link;

         Video_Search_Results.Append (
           Parse_Video_Search_Results (Get_Request_Response (Get_Search_Request (Search_Input
             (Video_Match_Result (1).First .. Video_Match_Result (1).Last)))).First_Element);
      elsif Playlist_Match_Result (1) /= GNAT.Regpat.No_Match then
         Search_Type := Playlist_Link;

         Video_Search_Results := Get_Playlist
           (Search_Input (Playlist_Match_Result (1).First .. Playlist_Match_Result (1).Last));
      else
         Search_Type := Words;

         Video_Search_Results :=
           Parse_Video_Search_Results (Get_Request_Response (Get_Search_Request (Search_Input)));
      end if;

      return Video_Search_Results;

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
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (Playlist_ID : in String) return Video_Vectors.Vector is
      Videos : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
      Total_Results      : Natural := 0;
      Current_Page_Token : Unbounded_String := To_Unbounded_String ("");
      Next_Page_Token    : Unbounded_String := To_Unbounded_String ("");
      Unavailable_Videos : Natural := 0;
   begin
      Videos.Append (Parse_Playlist_Item_Results (Get_Request_Response
        (Get_Playlist_Items_Request (Playlist_ID, To_String (Current_Page_Token))),
         Total_Results, Next_Page_Token, Unavailable_Videos));

      while Natural (Videos.Length) < Total_Results - Unavailable_Videos loop
         Current_Page_Token := Next_Page_Token;

         Videos.Append (Parse_Playlist_Item_Results (Get_Request_Response
           (Get_Playlist_Items_Request (Playlist_ID, To_String (Current_Page_Token))),
            Total_Results, Next_Page_Token, Unavailable_Videos));
      end loop;

      return Videos;
   end Get_Playlist;

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
   -- Get_Playlist_Items_Request
   -------------------------------------------------------------------------------------------------
   function Get_Playlist_Items_Request (Playlist_ID : in String; Page_Token : in String)
     return String is
   begin
      return YT_API_URL & "playlistItems?key=" & To_String (YT_API_KEY)
        & "&playlistId=" & Playlist_ID
        & "&pageToken=" & Page_Token
        & "&maxResults=50&part=snippet";
   end Get_Playlist_Items_Request;

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

         Video.Video_Thumbnail := To_Unbounded_String
           (Item.Get ("snippet").Get ("thumbnails").Get ("default").Get ("url").Value);

         Videos_Result.Append (Video);
      end loop;

      return Videos_Result;
   end Parse_Video_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Parse_Playlist_Item_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Playlist_Item_Results
     (Search_Results     : in String;
      Total_Results      : out Natural;
      Next_Page_Token    : out Unbounded_String;
      Unavailable_Videos : in out Natural)
     return Video_Vectors.Vector is
      JSON_String_Response : aliased String := Search_Results;

      JSON_Stream    : JSON.Streams.Stream'Class :=
        JSON.Streams.Create_Stream (JSON_String_Response'Access);
      JSON_Allocator : Types.Memory_Allocator;
      JSON_Result    : constant Types.JSON_Value := Parsers.Parse (JSON_Stream, JSON_Allocator);

      Video         : T_Video;
      Videos_Result : Video_Vectors.Vector := Video_Vectors.Empty_Vector;

      Results_Per_Page : Natural;
   begin
      Total_Results :=
        Natural'Value (JSON_Result.Get ("pageInfo").Get ("totalResults").Image);
      Results_Per_Page :=
        Natural'Value (JSON_Result.Get ("pageInfo").Get ("resultsPerPage").Image);

      if JSON_Result.Contains ("nextPageToken") then
         Next_Page_Token := To_Unbounded_String (JSON_Result.Get ("nextPageToken").Value);
      else
         Next_Page_Token := To_Unbounded_String ("");
      end if;

      for Item of JSON_Result.Get ("items") loop
         Video.Video_Title := To_Unbounded_String (Item.Get ("snippet").Get ("title").Value);

         if Video.Video_Title /= To_Unbounded_String ("Private video") then
            Video.Video_ID :=
              To_Unbounded_String (Item.Get ("snippet").Get ("resourceId").Get ("videoId").Value);

            Video.Video_Thumbnail := To_Unbounded_String
              (Item.Get ("snippet").Get ("thumbnails").Get ("default").Get ("url").Value);

            Videos_Result.Append (Video);
         end if;
      end loop;

      Unavailable_Videos := Unavailable_Videos + Results_Per_Page - Natural (Videos_Result.Length);

      return Videos_Result;
   end Parse_Playlist_Item_Results;

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
