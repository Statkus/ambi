with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with JSON.Parsers;
with JSON.Types;

package YT_API is

   MAX_VIDEO_SEARCH_RESULTS : constant := 5;

   type T_Video is record
      Video_ID           : Unbounded_String;
      Video_Title        : Unbounded_String;
      Video_Image_URL    : Unbounded_String;
   end record;

   type T_Video_Search_Results is array (1 .. MAX_VIDEO_SEARCH_RESULTS) of T_Video;

   type T_Video_Search_List_Response is record
      Next_Page_Token      : Unbounded_String;
      Total_Results        : Integer;
      Results_Per_Page     : Integer;
      Video_Search_Results : T_Video_Search_Results;
   end record;

   procedure Set_YT_API_Key (Key : in String);

   function Get_Video_Search_Results (Search_Input : in String) return T_Video_Search_Results;

   function Get_Video_Duration (Video : in T_Video) return Natural;

private

   package Types is new JSON.Types (Long_Integer, Long_Float); use Types;
   package Parsers is new JSON.Parsers (Types);

   function Get_Search_Request (Search_Input : in String) return String;
   function Get_Video_Request (Video_ID : in String) return String;

   function Parse_Video_Search_Results (Search_Results : in String) return T_Video_Search_Results;
   function Parse_Video_Duration_Result (Search_Result : in String) return Natural;
   function Parse_Duration (Duration_String : in String) return Natural;

   YT_API_KEY : Unbounded_String;
   YT_API_URL : constant String := "https://www.googleapis.com/youtube/v3/";

end YT_API;
