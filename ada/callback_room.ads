with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Session;
with AWS.Status;

with Room; use Room;
with List; use List;

package Callback_Room is

   procedure Set_Server_Address (Address : in String);

   function Callback (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

private

   type T_Video_List_Source is (Playlist, Historic, Likes);

   function Room_Callback (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Search_Button_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Add_To_Playlist_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Add_Remove_Like_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Player_Display_Checkbox_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Player_Sync_Checkbox_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Next_Room_Video_Callback (Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Next_Video_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Get_Video_List_Callback
     (Request : in AWS.Status.Data; Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Get_Current_Room_Video_Callback (Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Get_Number_Clients_Sync_Callback (Current_Room : in T_Room_Class_Access)
     return AWS.Response.Data;

   function Build_Search_Results (Video_Search_Results : in Video_Vectors.Vector) return String;

   function Build_Video_List
     (Current_Room : in T_Room_Class_Access;
      Session_ID   : in AWS.Session.ID;
      Source       : in T_Video_List_Source)
     return String;

   function Pack_AJAX_XML_Response (Placeholder : in String; Value : in String) return String;

   SERVER_ADDRESS : Unbounded_String;

end Callback_Room;
