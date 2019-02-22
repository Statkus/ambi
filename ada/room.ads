with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Session;

with Client;
with Playlist;
with YT_API;

package Room is

   type T_Room is tagged limited private;

   type T_Room_Class_Access is access all T_Room'Class;

   task type T_Room_Sync_Task (This : T_Room_Class_Access := null) is
      entry Start_Room_Playlist;
   end T_Room_Sync_Task;

   type T_Room_Sync_Task_Access is access T_Room_Sync_Task;

   -- Dummy function to instantiate a vector, for now comparing Client.T_Client type is useless
   function Client_Compare (Left, Right : Client.T_Client_Class_Access) return Boolean;

   package Client_Vectors is new Ada.Containers.Vectors
     (Natural, Client.T_Client_Class_Access, Client_Compare);

   procedure Set_Room_Sync_Task (This : in out T_Room; Sync_Task : in T_Room_Sync_Task_Access);

   procedure Lock (This : in out T_Room);
   procedure Unlock (This : in out T_Room);

   procedure Add_Client (This : in out T_Room; Session_ID : in AWS.Session.ID);

   function Is_Registered (This : in out T_Room; Session_ID : in AWS.Session.ID) return Boolean;

   procedure Add_Video_To_Playlists (This : in out T_Room; Video : in YT_API.T_Video);

   procedure Remove_First_Client_Playlist_Video
     (This : in out T_Room; Session_ID : in AWS.Session.ID);

   procedure Set_Current_Client_Video (This : in out T_Room; Session_ID : in AWS.Session.ID);

   procedure Set_Video_Search_Results
     (This : in out T_Room; Video_Search_Results : in YT_API.T_Video_Search_Results);

   procedure Set_Client_Display_Player
     (This : in out T_Room; Session_ID : in AWS.Session.ID; Display : in Boolean);

   function Get_Current_Video (This : in T_Room) return YT_API.T_Video;

   function Get_Video_Search_Results (This : in T_Room) return YT_API.T_Video_Search_Results;

   function Get_Current_Client_Video (This : in T_Room; Session_ID : in AWS.Session.ID)
     return YT_API.T_Video;

   function Get_Client_Playlist (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Playlist.Video_Vectors.Vector;

   function Get_Client_Display_Player (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Boolean;

private

   procedure Update_No_Player_Clients (This : in out T_Room);

   protected type T_Mutex is
      entry Seize;
      procedure Release;
   private
      Owned : Boolean := False;
   end T_Mutex;

   type T_Room is tagged limited record
      Video_Search_Results : YT_API.T_Video_Search_Results;
      Room_Current_Video   : YT_API.T_Video :=
        (Video_Title => To_Unbounded_String ("no video played"), others => <>);
      Room_Playlist : Playlist.Video_Vectors.Vector := Playlist.Video_Vectors.Empty_Vector;
      Room_Sync_Task            : T_Room_Sync_Task_Access;
      Room_Current_Video_Active : Boolean := False;
      Room_Mutex                : T_Mutex;
      Client_List               : Client_Vectors.Vector := Client_Vectors.Empty_Vector;
      Client_ID_Counter         : Integer := 0;
   end record;

end Room;
