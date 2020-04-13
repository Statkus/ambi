with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

with Json.Parsers;
with Json.Streams;
with Json.Types;

package body Api.Provider.Youtube is

   package Types is new Json.Types (Long_Integer, Long_Float);
   use Types;
   package Parsers is new Json.Parsers (Types);

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize
     (Api_Key       : in String;
      Http_Accessor : in not null Web_Methods.Http.T_Http_Class_Access) return T_Youtube_Access is
     (new T_Youtube'
        (Api_Key_Length => Api_Key'Length, Api_Key => Api_Key, Http_Accessor => Http_Accessor));

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in out T_Youtube;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song.List.T_Song_List
   is
      Video_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile ("www\.youtube\.com/watch\?v=([\w-]+)");
      Playlist_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile ("www\.youtube\.com/playlist\?list=([\w-]+)");

      Video_Match_Result    : GNAT.Regpat.Match_Array (0 .. 1);
      Playlist_Match_Result : GNAT.Regpat.Match_Array (0 .. 1);

      Video_Search_Results : Song.List.T_Song_List := Song.List.Initialize;
   begin
      GNAT.Regpat.Match (Video_Pattern, Search_Input, Video_Match_Result);
      GNAT.Regpat.Match (Playlist_Pattern, Search_Input, Playlist_Match_Result);

      if Video_Match_Result (1) /= GNAT.Regpat.No_Match then
         Search_Type := Video_Link;

         Video_Search_Results :=
           Parse_Video_Search_Results
             (This.Get_Request_Response
              (This.Format_Search_Request
               (Search_Input (Video_Match_Result (1).First .. Video_Match_Result (1).Last))));

         while Natural (Video_Search_Results.Length) > 1 loop
            Video_Search_Results.Delete_Last;
         end loop;
      elsif Playlist_Match_Result (1) /= GNAT.Regpat.No_Match then
         Search_Type := Playlist_Link;

         Video_Search_Results :=
           This.Get_Playlist
           (Search_Input (Playlist_Match_Result (1).First .. Playlist_Match_Result (1).Last));
      else
         Search_Type := Words;

         Video_Search_Results :=
           Parse_Video_Search_Results
             (This.Get_Request_Response (This.Format_Search_Request (Search_Input)));
      end if;

      return Video_Search_Results;
   end Get_Song_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration
     (This        : in out T_Youtube;
      Source_Song : in     Song.T_Song) return Natural
   is
   begin
      return Parse_Video_Duration_Result
          (This.Get_Request_Response (This.Format_Video_Request (Source_Song.Get_Id)));
   end Get_Song_Duration;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in out T_Youtube;
      Source_Song : in     Song.T_Song) return Song.List.T_Song_List
   is
   begin
      return Parse_Video_Search_Results
          (This.Get_Request_Response (This.Format_Videos_Related_Request (Source_Song.Get_Id)));
   end Get_Related_Songs;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist
     (This        : in T_Youtube;
      Playlist_Id : in String) return Song.List.T_Song_List
   is
      Videos             : Song.List.T_Song_List := Song.List.Initialize;
      Total_Results      : Natural               := Natural'Last;
      Current_Page_Token : Unbounded_String      := To_Unbounded_String ("");
      Next_Page_Token    : Unbounded_String      := To_Unbounded_String ("");
      Unavailable_Videos : Natural               := 0;
   begin
      while Natural (Videos.Length) < Total_Results - Unavailable_Videos loop
         Current_Page_Token := Next_Page_Token;

         Videos.Append
         (Parse_Playlist_Item_Results
            (This.Get_Request_Response
             (This.Format_Playlist_Items_Request (Playlist_Id, To_String (Current_Page_Token))),
             Total_Results,
             Next_Page_Token,
             Unavailable_Videos));
      end loop;

      return Videos;
   end Get_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Format_Search_Request
   -------------------------------------------------------------------------------------------------
   function Format_Search_Request (This : in T_Youtube; Search_Input : in String) return String is
   begin
      return Api_Url &
        "search?key=" &
        This.Api_Key &
        "&q=" &
        Search_Input &
        "&maxResults=" &
        Max_Video_Search_Results &
        "&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true";
   end Format_Search_Request;

   -------------------------------------------------------------------------------------------------
   -- Format_Video_Request
   -------------------------------------------------------------------------------------------------
   function Format_Video_Request (This : in T_Youtube; Video_Id : in String) return String is
   begin
      return Api_Url & "videos?key=" & This.Api_Key & "&id=" & Video_Id & "&part=contentDetails";
   end Format_Video_Request;

   -------------------------------------------------------------------------------------------------
   -- Format_Videos_Related_Request
   -------------------------------------------------------------------------------------------------
   function Format_Videos_Related_Request
     (This     : in T_Youtube;
      Video_Id : in String) return String
   is
   begin
      return Api_Url &
        "search?key=" &
        This.Api_Key &
        "&relatedToVideoId=" &
        Video_Id &
        "&maxResults=" &
        Max_Video_Search_Results &
        "&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true";
   end Format_Videos_Related_Request;

   -------------------------------------------------------------------------------------------------
   -- Format_Playlist_Items_Request
   -------------------------------------------------------------------------------------------------
   function Format_Playlist_Items_Request
     (This        : in T_Youtube;
      Playlist_Id : in String;
      Page_Token  : in String) return String
   is
   begin
      return Api_Url &
        "playlistItems?key=" &
        This.Api_Key &
        "&playlistId=" &
        Playlist_Id &
        "&pageToken=" &
        Page_Token &
        "&maxResults=50&part=snippet";
   end Format_Playlist_Items_Request;

   -------------------------------------------------------------------------------------------------
   -- Get_Request_Response
   -------------------------------------------------------------------------------------------------
   function Get_Request_Response (This : in T_Youtube; Url_Request : in String) return String is
      Response          : Unbounded_String;
      Response_Received : Boolean := False;
      Number_Of_Try     : Natural := Natural'First;
   begin
      Put_Line ("YT API request: " & Url_Request);

      while not Response_Received and Number_Of_Try < Max_Number_Of_Request_Retry loop
         Response := To_Unbounded_String (This.Http_Accessor.Get (Url_Request));

         Number_Of_Try := Number_Of_Try + 1;

         if not (Index (To_String (Response), "GET request error.") = 1) then
            Response_Received := True;
         end if;
      end loop;

      if not Response_Received then
         Put_Line ("YT API request not answered after" & Number_Of_Try'Img & " retry");
      end if;

      return To_String (Response);
   end Get_Request_Response;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Search_Results (Search_Results : in String) return Song.List.T_Song_List is
      Json_String_Response : aliased String := Search_Results;

      Json_Stream : Json.Streams.Stream'Class :=
        Json.Streams.Create_Stream (Json_String_Response'Access);
      Json_Allocator : Types.Memory_Allocator;

      Video         : Song.T_Song;
      Videos_Result : Song.List.T_Song_List := Song.List.Initialize;
   begin
      declare
         -- This declaration might raise an exception if the JSON is not well formatted
         Json_Result : constant Types.Json_Value := Parsers.Parse (Json_Stream, Json_Allocator);
      begin
         for Item of Json_Result.Get ("items") loop
            Video :=
              Song.Initialize
                (Id             => Item.Get ("id").Get ("videoId").Value,
                 Title          => Item.Get ("snippet").Get ("title").Value,
                 Thumbnail_Link =>
                   Item.Get ("snippet").Get ("thumbnails").Get ("default").Get ("url").Value,
                 Provider => Api.Youtube_Api);

            Videos_Result.Append (Video);
         end loop;
      end;

      return Videos_Result;

   exception
      when others =>
         return Song.List.Initialize;
   end Parse_Video_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Parse_Playlist_Item_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Playlist_Item_Results
     (Search_Results     : in     String;
      Total_Results      :    out Natural;
      Next_Page_Token    :    out Unbounded_String;
      Unavailable_Videos : in out Natural) return Song.List.T_Song_List
   is
      Json_String_Response : aliased String := Search_Results;

      Json_Stream : Json.Streams.Stream'Class :=
        Json.Streams.Create_Stream (Json_String_Response'Access);
      Json_Allocator : Types.Memory_Allocator;

      Video         : Song.T_Song;
      Videos_Result : Song.List.T_Song_List := Song.List.Initialize;

      Results_Per_Page : Natural;
   begin
      Total_Results   := Natural'First;
      Next_Page_Token := To_Unbounded_String ("");

      declare
         -- This declaration might raise an exception if the JSON is not well formatted
         Json_Result : constant Types.Json_Value := Parsers.Parse (Json_Stream, Json_Allocator);
      begin
         Total_Results := Natural'Value (Json_Result.Get ("pageInfo").Get ("totalResults").Image);
         Results_Per_Page :=
           Natural'Value (Json_Result.Get ("pageInfo").Get ("resultsPerPage").Image);

         if Json_Result.Contains ("nextPageToken") then
            Next_Page_Token := To_Unbounded_String (Json_Result.Get ("nextPageToken").Value);
         end if;

         for Item of Json_Result.Get ("items") loop
            -- Filter private video
            if Item.Get ("snippet").Get ("title").Value /= "Private video" and
              Item.Get ("snippet").Get ("title").Value /= "Deleted video"
            then
               Video :=
                 Song.Initialize
                   (Id             => Item.Get ("snippet").Get ("resourceId").Get ("videoId").Value,
                    Title          => Item.Get ("snippet").Get ("title").Value,
                    Thumbnail_Link =>
                      Item.Get ("snippet").Get ("thumbnails").Get ("default").Get ("url").Value,
                    Provider => Api.Youtube_Api);

               Videos_Result.Append (Video);
            end if;
         end loop;
      end;

      if Results_Per_Page > Total_Results then
         Unavailable_Videos := Unavailable_Videos + Total_Results - Natural (Videos_Result.Length);
      else
         Unavailable_Videos :=
           Unavailable_Videos + Results_Per_Page - Natural (Videos_Result.Length);
      end if;

      return Videos_Result;

   exception
      when others =>
         return Song.List.Initialize;
   end Parse_Playlist_Item_Results;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Duration_Result
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Duration_Result (Search_Result : in String) return Natural is
      Json_String_Response : aliased String := Search_Result;

      Json_Stream : Json.Streams.Stream'Class :=
        Json.Streams.Create_Stream (Json_String_Response'Access);
      Json_Allocator : Types.Memory_Allocator;

      Duration : Natural := Natural'First;
   begin
      declare
         -- This declaration might raise an exception if the JSON is not well formatted
         Json_Result : constant Types.Json_Value := Parsers.Parse (Json_Stream, Json_Allocator);
      begin
         for Item of Json_Result.Get ("items") loop
            -- Normally there is only one item
            Duration :=
              Convert_Iso_8601_Duration_To_Seconds
                (Item.Get ("contentDetails").Get ("duration").Value);
         end loop;
      end;

      return Duration;

   exception
      when others =>
         return Natural'First;
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

end Api.Provider.Youtube;
