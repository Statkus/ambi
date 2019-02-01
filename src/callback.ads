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

   type T_Video_ID_Search_Result is array (1 .. YT_VIDEO_SEARCH_RESULT) of
     String (1 .. YT_VIDEO_ID_LENGTH);

   function Parse_Search_Request (Search_Result : in String) return T_Video_ID_Search_Result;

   function Build_Search_Result
     (Video_ID_Search_Result : in T_Video_ID_Search_Result) return String;

end Callback;
