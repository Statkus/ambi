with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Status;

with YT_API; use YT_API;

package Callback is

   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data;

private

   function Search_Result_Callback (Request : AWS.Status.Data) return AWS.Response.Data;

   function Build_Search_Result
     (Video_Search_Results : in T_Video_Search_Results) return String;

end Callback;
