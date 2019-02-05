with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Status;

package Callback is

   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data;

   procedure Set_YT_API_Key (Key : String);

private

   YT_API_KEY : String := "000000000000000000000000000000000000000";
   YT_API_URL : constant String := "https://www.googleapis.com/youtube/v3/";

   YT_VIDEO_ID_LENGTH     : constant := 11;
   YT_VIDEO_SEARCH_RESULT : constant := 5;
   YT_PAGE_TOKEN_LENGTH   : constant := 6;

   type T_Video_Search_Result is record
      Video_ID           : String (1 .. YT_VIDEO_ID_LENGTH) := (others => '0');
      Video_Title        : Unbounded_String;
      Video_Image_URL    : Unbounded_String;
      Video_Image_Width  : String (1 .. 3) := "320";
      VIdeo_Image_Height : String (1 .. 3) := "180";
   end record;

   type T_Video_Search_Results is array (1 .. YT_VIDEO_SEARCH_RESULT) of T_Video_Search_Result;

   type T_Video_Search_List_Response is record
      Next_Page_Token      : String (1 .. YT_PAGE_TOKEN_LENGTH) := (others => '0');
      Total_Results        : Natural := 0;
      Results_Per_Page     : Natural := 0;
      Video_Search_Results : T_Video_Search_Results;
   end record;

   Video_ID : String (1 .. YT_VIDEO_ID_LENGTH) := (others => '0');
   Video_Search_List_Response : T_Video_Search_List_Response;

   function Search_Result_Callback (Request : AWS.Status.Data) return AWS.Response.Data;

   function Parse_Video_Search_List_Request (Search_Result : in String)
     return T_Video_Search_List_Response;

   function REGEX_Match_To_String
     (Pattern : in String;
      Source  : in String;
      Offset  : out Integer)
     return String;

   function REGEX_Match_To_Integer
     (Pattern : in String;
      Source  : in String;
      Offset  : out Integer)
     return Integer;

   function Build_Search_Result
     (Video_Search_List_Response : in T_Video_Search_List_Response) return String;

end Callback;
