with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Session;

with Client;
with Database;
with Video_List; use Video_List;
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

   procedure Set_Database
     (This : in out T_Room; DB : in not null Database.T_Database_Class_Access);

   procedure Set_Room_Sync_Task
     (This : in out T_Room; Sync_Task : in not null T_Room_Sync_Task_Access);

   procedure Lock (This : in out T_Room);
   procedure Unlock (This : in out T_Room);

   procedure Add_Client (This : in out T_Room; Session_ID : in AWS.Session.ID);

   function Is_Registered (This : in out T_Room; Session_ID : in AWS.Session.ID) return Boolean;

   procedure Add_Video_To_Playlists (This : in out T_Room; Video : in YT_API.T_Video);

   procedure Add_Like (This : in out T_Room; Video : in YT_API.T_Video);

   procedure Remove_Like (This : in out T_Room; Video : in YT_API.T_Video);

   procedure Next_Client_Video (This : in out T_Room; Session_ID : in AWS.Session.ID);

   procedure Set_Video_Search_Results
     (This : in out T_Room; Video_Search_Results : in YT_API.T_Video_Search_Results);

   procedure Set_Client_Display_Player
     (This : in out T_Room; Session_ID : in AWS.Session.ID; Display : in Boolean);

   procedure Set_Client_Sync_With_Room
     (This : in out T_Room; Session_ID : in AWS.Session.ID; Sync : in Boolean);

   function Get_Current_Video (This : in out T_Room) return YT_API.T_Video;

   function Get_Video_Search_Results (This : in T_Room) return YT_API.T_Video_Search_Results;

   function Get_Historic (This : in T_Room) return Video_Vectors.Vector;

   function Get_Historic_Item (This : in T_Room; Item_Number : in Natural) return YT_API.T_Video;

   function Get_Likes (This : in T_Room) return Video_Vectors.Vector;

   function Get_Likes_Item (This : in T_Room; Item_Number : in Natural) return YT_API.T_Video;

   function Is_Video_Liked (This : in T_Room; Video : in YT_API.T_Video) return Boolean;

   function Get_Current_Client_Video (This : in T_Room; Session_ID : in AWS.Session.ID)
     return YT_API.T_Video;

   function Get_Client_Playlist (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Video_Vectors.Vector;

   function Get_Client_Playlist_Item
     (This : in T_Room; Session_ID : in AWS.Session.ID; Item_Number : in Natural)
     return YT_API.T_Video;

   function Get_Client_Display_Player (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Boolean;

   function Get_Client_Sync_With_Room (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Boolean;

   function Client_Has_Nothing_To_Play (This : in out T_Room; Session_ID : in AWS.Session.ID)
     return Boolean;

private

   procedure Update_No_Player_Clients (This : in out T_Room);

   function Find_Client_From_Session_ID (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Client.T_Client_Class_Access;

   -- Accessors protected by mutex
   procedure Set_Video (This : in out T_Room; Video : in YT_API.T_Video);
   procedure Playlist_Append (This : in out T_Room; Video : in YT_API.T_Video);
   procedure Playlist_Delete_First (This : in out T_Room);
   function Get_Video (This : in out T_Room) return YT_API.T_Video;
   function Get_Playlist (This : in out T_Room) return Video_Vectors.Vector;
   function Get_Playlist_First (This : in out T_Room) return YT_API.T_Video;
   function Get_Playlist_Is_Empty (This : in out T_Room) return Boolean;

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
      Room_Playlist : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
      Room_Sync_Task            : T_Room_Sync_Task_Access;
      Room_Current_Video_Active : Boolean := False;
      Room_Video_Playlist_Mutex : T_Mutex;
      Room_Callback_Mutex       : T_Mutex;
      Client_List               : Client_Vectors.Vector := Client_Vectors.Empty_Vector;
      DB                        : Database.T_Database_Class_Access;
   end record;

end Room;
