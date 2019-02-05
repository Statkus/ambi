with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Status;

with YT_API; use YT_API;

package Callback is

   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data;

private

   Video_ID : String (1 .. YT_VIDEO_ID_LENGTH) := (others => '0');
   Video_Search_List_Response : T_Video_Search_List_Response;

   function Search_Result_Callback (Request : AWS.Status.Data) return AWS.Response.Data;

   function Build_Search_Result
     (Video_Search_List_Response : in T_Video_Search_List_Response) return String;

end Callback;
