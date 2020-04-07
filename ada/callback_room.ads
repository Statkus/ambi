with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aws.Response;
with Aws.Session;
with Aws.Status;

with Room;        use Room;
with Song_Vector; use Song_Vector;

package Callback_Room is

   procedure Set_Server_Address (Address : in String);

   function Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

private

   type T_Song_List_Source is (Playlist, Historic, Likes);

   function Room_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Search_Button_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Add_To_Playlist_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Remove_From_Playlist_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Add_Remove_Like_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Player_Display_Checkbox_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Player_Sync_Checkbox_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Next_Room_Song_Callback
     (Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Up_Vote_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Next_Song_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Get_Song_List_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Get_Current_Room_Song_Callback
     (Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Get_Number_Clients_Callback
     (Current_Room : in T_Room_Class_Access) return Aws.Response.Data;

   function Build_Search_Results (Song_Search_Results : in T_Song_Vector) return String;

   function Build_Playlist
     (Current_Room : in T_Room_Class_Access;
      Session_Id   : in Aws.Session.Id) return String;

   function Build_Song_List
     (Current_Room : in T_Room_Class_Access;
      Source       : in T_Song_List_Source) return String;

   function Pack_Ajax_Xml_Response (Placeholder : in String; Value : in String) return String;

   Server_Address : Unbounded_String;

end Callback_Room;
