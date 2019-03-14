with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Session;
with AWS.Status;

with Database;
with Room;
with Video_List; use Video_List;

package Callback is

   procedure Set_Server_Address (Address : in String);

   procedure Create_Room (DB : in not null Database.T_Database_Class_Access);

   function Ambi_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;

private

   type T_Add_To_Playlist_Source is (Search_Results, Historic, Likes);
   type T_Video_List_Source is (Playlist, Historic, Likes);
   type T_Like is (Like, Unlike);

   function Room_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Javascripts_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function CSS_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Search_Button_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Add_To_Playlist_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Add_Remove_Like_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Player_Display_Checkbox_Callback (Request : in AWS.Status.Data)
     return AWS.Response.Data;
   function Player_Sync_Checkbox_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Next_Video_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Get_Video_List_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Get_Current_Room_Video_Callback return AWS.Response.Data;

   function Build_Search_Results (Video_Search_Results : in Video_Vectors.Vector) return String;
   function Build_Video_List (Session_ID : in AWS.Session.ID; Source : in T_Video_List_Source)
     return String;

   function Pack_AJAX_XML_Response (Placeholder : in String; Value : in String) return String;

   SERVER_ADDRESS : Unbounded_String;

   Current_Room : Room.T_Room_Class_Access;

end Callback;
