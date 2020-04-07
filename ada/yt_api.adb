with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

with Aws.Client;
with Aws.Response;

with Json.Parsers;
with Json.Streams;
with Json.Types;

package body Yt_Api is

   package Types is new Json.Types (Long_Integer, Long_Float);
   use Types;
   package Parsers is new Json.Parsers (Types);

   -------------------------------------------------------------------------------------------------
   -- Set_Api_Key
   -------------------------------------------------------------------------------------------------
   procedure Set_Api_Key (Key : in String) is
   begin
      Api_Key := To_Unbounded_String (Key);
   end Set_Api_Key;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Video_Search_Results
     (Search_Input : in     String;
      Search_Type  :    out Api_Provider.T_Search_Type) return T_Song_Vector
   is
      Video_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile ("www\.youtube\.com/watch\?v=([\w-]+)");
      Playlist_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile ("www\.youtube\.com/playlist\?list=([\w-]+)");

      Video_Match_Result    : GNAT.Regpat.Match_Array (0 .. 1);
      Playlist_Match_Result : GNAT.Regpat.Match_Array (0 .. 1);

      Video_Search_Results : T_Song_Vector := Song_Vector.Constructors.Initialize;
   begin
      GNAT.Regpat.Match (Video_Pattern, Search_Input, Video_Match_Result);
      GNAT.Regpat.Match (Playlist_Pattern, Search_Input, Playlist_Match_Result);

      if Video_Match_Result (1) /= GNAT.Regpat.No_Match then
         Search_Type := Api_Provider.Video_Link;

         Video_Search_Results.Append
         (Parse_Video_Search_Results
            (Get_Request_Response
               (Get_Search_Request
                  (Search_Input
                     (Video_Match_Result (1).First ..
                          Video_Match_Result (1).Last)))).First_Element);
      elsif Playlist_Match_Result (1) /= GNAT.Regpat.No_Match then
         Search_Type := Api_Provider.Playlist_Link;

         Video_Search_Results :=
           Get_Playlist
             (Search_Input (Playlist_Match_Result (1).First .. Playlist_Match_Result (1).Last));
      else
         Search_Type := Api_Provider.Words;

         Video_Search_Results :=
           Parse_Video_Search_Results (Get_Request_Response (Get_Search_Request (Search_Input)));
      end if;

      return Video_Search_Results;

   exception
      when others =>
         return Song_Vector.Constructors.Initialize;
   end Get_Video_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Video_Duration (Video : in T_Song) return Natural is
   begin
      return Parse_Video_Duration_Result (Get_Request_Response (Get_Video_Request (Video.Get_Id)));

   exception
      when others =>
         return Natural'First;
   end Get_Video_Duration;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Videos
   -------------------------------------------------------------------------------------------------
   function Get_Related_Videos (Video : in T_Song) return T_Song_Vector is
   begin
      return Parse_Video_Search_Results
          (Get_Request_Response (Get_Videos_Related_Request (Video.Get_Id)));

   exception
      when others =>
         return Song_Vector.Constructors.Initialize;
   end Get_Related_Videos;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (Playlist_Id : in String) return T_Song_Vector is
      Videos             : T_Song_Vector    := Song_Vector.Constructors.Initialize;
      Total_Results      : Natural          := 0;
      Current_Page_Token : Unbounded_String := To_Unbounded_String ("");
      Next_Page_Token    : Unbounded_String := To_Unbounded_String ("");
      Unavailable_Videos : Natural          := 0;
   begin
      Videos.Append
      (Parse_Playlist_Item_Results
         (Get_Request_Response
            (Get_Playlist_Items_Request (Playlist_Id, To_String (Current_Page_Token))),
          Total_Results,
          Next_Page_Token,
          Unavailable_Videos));

      while Natural (Videos.Length) < Total_Results - Unavailable_Videos loop
         Current_Page_Token := Next_Page_Token;

         Videos.Append
         (Parse_Playlist_Item_Results
            (Get_Request_Response
               (Get_Playlist_Items_Request (Playlist_Id, To_String (Current_Page_Token))),
             Total_Results,
             Next_Page_Token,
             Unavailable_Videos));
      end loop;

      return Videos;
   end Get_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Get_Request_Response
   -------------------------------------------------------------------------------------------------
   function Get_Request_Response (Url_Request : in String) return String is
      Response      : Unbounded_String := To_Unbounded_String ("GET request error.");
      Number_Of_Try : Natural          := Natural'First;
   begin
      Put_Line ("YT API request: " & Url_Request);

      while Index (To_String (Response), "GET request error.") = 1 and
        Number_Of_Try < Max_Number_Of_Request_Retry
      loop
         Response :=
           To_Unbounded_String (Aws.Response.Message_Body (Aws.Client.Get (Url => Url_Request)));

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
      return Api_Url &
        "search?key=" &
        To_String (Api_Key) &
        "&q=" &
        Search_Input &
        "&maxResults=" &
        Max_Video_Search_Results &
        "&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true";
   end Get_Search_Request;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Request
   -------------------------------------------------------------------------------------------------
   function Get_Video_Request (Video_Id : in String) return String is
   begin
      return Api_Url &
        "videos?key=" &
        To_String (Api_Key) &
        "&id=" &
        Video_Id &
        "&part=contentDetails";
   end Get_Video_Request;

   -------------------------------------------------------------------------------------------------
   -- Get_Videos_Related_Request
   -------------------------------------------------------------------------------------------------
   function Get_Videos_Related_Request (Video_Id : in String) return String is
   begin
      return Api_Url &
        "search?key=" &
        To_String (Api_Key) &
        "&relatedToVideoId=" &
        Video_Id &
        "&maxResults=" &
        Max_Video_Search_Results &
        "&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true";
   end Get_Videos_Related_Request;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist_Items_Request
   -------------------------------------------------------------------------------------------------
   function Get_Playlist_Items_Request
     (Playlist_Id : in String;
      Page_Token  : in String) return String
   is
   begin
      return Api_Url &
        "playlistItems?key=" &
        To_String (Api_Key) &
        "&playlistId=" &
        Playlist_Id &
        "&pageToken=" &
        Page_Token &
        "&maxResults=50&part=snippet";
   end Get_Playlist_Items_Request;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Search_Results (Search_Results : in String) return T_Song_Vector is
      Json_String_Response : aliased String := Search_Results;

      Json_Stream : Json.Streams.Stream'Class :=
        Json.Streams.Create_Stream (Json_String_Response'Access);
      Json_Allocator : Types.Memory_Allocator;
      Json_Result    : constant Types.Json_Value := Parsers.Parse (Json_Stream, Json_Allocator);

      Video         : T_Song;
      Videos_Result : T_Song_Vector := Song_Vector.Constructors.Initialize;
   begin
      for Item of Json_Result.Get ("items") loop
         Video :=
           Song.Constructors.Initialize
             (Id             => Item.Get ("id").Get ("videoId").Value,
              Title          => Item.Get ("snippet").Get ("title").Value,
              Thumbnail_Link =>
                Item.Get ("snippet").Get ("thumbnails").Get ("default").Get ("url").Value,
              Provider => Api_Provider.Youtube);

         Videos_Result.Append (Video);
      end loop;

      return Videos_Result;
   end Parse_Video_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Parse_Playlist_Item_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Playlist_Item_Results
     (Search_Results     : in     String;
      Total_Results      :    out Natural;
      Next_Page_Token    :    out Unbounded_String;
      Unavailable_Videos : in out Natural) return T_Song_Vector
   is
      Json_String_Response : aliased String := Search_Results;

      Json_Stream : Json.Streams.Stream'Class :=
        Json.Streams.Create_Stream (Json_String_Response'Access);
      Json_Allocator : Types.Memory_Allocator;
      Json_Result    : constant Types.Json_Value := Parsers.Parse (Json_Stream, Json_Allocator);

      Video         : T_Song;
      Videos_Result : T_Song_Vector := Song_Vector.Constructors.Initialize;

      Results_Per_Page : Natural;
   begin
      Total_Results    := Natural'Value (Json_Result.Get ("pageInfo").Get ("totalResults").Image);
      Results_Per_Page := Natural'Value (Json_Result.Get ("pageInfo").Get ("resultsPerPage").Image);

      if Json_Result.Contains ("nextPageToken") then
         Next_Page_Token := To_Unbounded_String (Json_Result.Get ("nextPageToken").Value);
      else
         Next_Page_Token := To_Unbounded_String ("");
      end if;

      for Item of Json_Result.Get ("items") loop
         if Item.Get ("snippet").Get ("title").Value /= "Private video" then
            Video :=
              Song.Constructors.Initialize
                (Id             => Item.Get ("snippet").Get ("resourceId").Get ("videoId").Value,
                 Title          => Item.Get ("snippet").Get ("title").Value,
                 Thumbnail_Link =>
                   Item.Get ("snippet").Get ("thumbnails").Get ("default").Get ("url").Value,
                 Provider => Api_Provider.Youtube);

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
      Json_String_Response : aliased String := Search_Result;

      Json_Stream : Json.Streams.Stream'Class :=
        Json.Streams.Create_Stream (Json_String_Response'Access);
      Json_Allocator : Types.Memory_Allocator;
      Json_Result    : constant Types.Json_Value := Parsers.Parse (Json_Stream, Json_Allocator);

      Duration : Natural := 0;
   begin
      for Item of Json_Result.Get ("items") loop
         -- Normally there is only one item
         Duration :=
           Convert_Iso_8601_Duration_To_Seconds
             (Item.Get ("contentDetails").Get ("duration").Value);
      end loop;

      return Duration;
   end Parse_Video_Duration_Result;

   -------------------------------------------------------------------------------------------------
   -- Convert_Iso_8601_Duration_To_Seconds
   -------------------------------------------------------------------------------------------------
   function Convert_Iso_8601_Duration_To_Seconds
     (Iso_8601_Duration_String : in String) return Natural
   is
      Hours_Pattern   : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("([0-9]+)H");
      Minutes_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("([0-9]+)M");
      Seconds_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("([0-9]+)S");
      Result          : GNAT.Regpat.Match_Array (0 .. 1);

      Hours   : Natural := 0;
      Minutes : Natural := 0;
      Seconds : Natural := 0;
   begin
      GNAT.Regpat.Match (Hours_Pattern, Iso_8601_Duration_String, Result);

      if Result (1) /= GNAT.Regpat.No_Match then
         Hours := Natural'Value (Iso_8601_Duration_String (Result (1).First .. Result (1).Last));
      end if;

      GNAT.Regpat.Match (Minutes_Pattern, Iso_8601_Duration_String, Result);

      if Result (1) /= GNAT.Regpat.No_Match then
         Minutes := Natural'Value (Iso_8601_Duration_String (Result (1).First .. Result (1).Last));
      end if;

      GNAT.Regpat.Match (Seconds_Pattern, Iso_8601_Duration_String, Result);

      if Result (1) /= GNAT.Regpat.No_Match then
         Seconds := Natural'Value (Iso_8601_Duration_String (Result (1).First .. Result (1).Last));
      end if;

      return (Hours * 3600) + (Minutes * 60) + Seconds;
   end Convert_Iso_8601_Duration_To_Seconds;

end Yt_Api;
