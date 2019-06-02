with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with JSON.Parsers;
with JSON.Types;

with List; use List;

package YT_API is

   type T_Search_Type is (Words, Video_Link, Playlist_Link);

   procedure Set_YT_API_Key (Key : in String);

   function Get_Video_Search_Results (Search_Input : in String; Search_Type : out T_Search_Type)
     return Video_Vectors.Vector;

   function Get_Video_Duration (Video : in T_Video) return Natural;

   function Get_Videos_Related (Video : in T_Video) return Video_Vectors.Vector;

private

   package Types is new JSON.Types (Long_Integer, Long_Float); use Types;
   package Parsers is new JSON.Parsers (Types);

   function Get_Playlist (Playlist_ID : in String) return Video_Vectors.Vector;

   function Get_Request_Response (URL_Request : in String) return String;

   function Get_Search_Request (Search_Input : in String) return String;
   function Get_Video_Request (Video_ID : in String) return String;
   function Get_Videos_Related_Request (Video_ID : in String) return String;
   function Get_Playlist_Items_Request (Playlist_ID : in String; Page_Token : in String)
     return String;

   function Parse_Video_Search_Results (Search_Results : in String) return Video_Vectors.Vector;
   function Parse_Playlist_Item_Results
     (Search_Results     : in String;
      Total_Results      : out Natural;
      Next_Page_Token    : out Unbounded_String;
      Unavailable_Videos : in out Natural)
     return Video_Vectors.Vector;
   function Parse_Video_Duration_Result (Search_Result : in String) return Natural;

   function Parse_Duration (Duration_String : in String) return Natural;

   YT_API_KEY : Unbounded_String;
   YT_API_URL : constant String := "https://www.googleapis.com/youtube/v3/";

   MAX_VIDEO_SEARCH_RESULTS    : constant String := "10";
   MAX_NUMBER_OF_REQUEST_RETRY : constant := 10;

end YT_API;
