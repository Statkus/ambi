with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Status;

with Room;
with YT_API; use YT_API;

package Callback is

   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data;

private

   function Javascripts_Callback (Request : AWS.Status.Data) return AWS.Response.Data;
   function Search_Button_Callback (Request : AWS.Status.Data) return AWS.Response.Data;
   function Search_Result_Item_Callback (Request : AWS.Status.Data) return AWS.Response.Data;

   function Build_Search_Result (Video_Search_Results : in T_Video_Search_Results) return String;
   function Build_Playlist (Playlist : in Room.Video_Vectors.Vector) return String;

   function Pack_AJAX_XML_Response (Placeholder : in String; Value : in String) return String;

end Callback;
