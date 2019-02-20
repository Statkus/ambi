with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Status;

with Playlist;
with YT_API;

with Room;

package Callback is

   procedure Set_Server_Address (Address : in String);

   procedure Create_Room;

   function Ambi_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;

private

   function Room_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Javascripts_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Search_Button_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Search_Result_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Next_Video_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Get_Playlist_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Get_Current_Room_Video_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;

   function Build_Search_Result (Video_Search_Results : in YT_API.T_Video_Search_Results)
     return String;

   function Build_Playlist (Current_Playlist : in Playlist.Video_Vectors.Vector) return String;

   function Pack_AJAX_XML_Response (Placeholder : in String; Value : in String) return String;

   protected type Mutex is
      entry Seize;
      procedure Release;
   private
      Owned : Boolean := False;
   end Mutex;

   SERVER_ADDRESS : Unbounded_String;

   Current_Room       : Room.T_Room_Class_Access;
   Current_Room_Mutex : Mutex;

end Callback;
