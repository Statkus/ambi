with Ada.Containers.Vectors;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Session;

with Client;
with Database;
with List; use List; use List.Video_Vectors;


package Room is

   type T_Room is tagged limited private;

   type T_Room_Class_Access is access all T_Room'Class;

   task type T_Room_Sync_Task (This : T_Room_Class_Access := null) is
      entry Start_Room_Playlist;
      entry Next_Room_Video;
   end T_Room_Sync_Task;

   type T_Room_Sync_Task_Access is access T_Room_Sync_Task;

   function New_And_Initialize
     (Name : in String; DB : in not null Database.T_Database_Class_Access)
     return T_Room_Class_Access;

   procedure Set_Room_Sync_Task
     (This : in out T_Room; Sync_Task : in not null T_Room_Sync_Task_Access);

   procedure Lock (This : in out T_Room);
   procedure Unlock (This : in out T_Room);

   procedure Add_Client (This : in out T_Room; Session_ID : in AWS.Session.ID);

   procedure Set_Client_Last_Request_Time (This : in out T_Room; Session_ID : in AWS.Session.ID);

   function Is_Registered (This : in out T_Room; Session_ID : in AWS.Session.ID) return Boolean;

   procedure Add_Video_To_Playlists
     (This : in out T_Room; Session_ID : in AWS.Session.ID; Video : in T_Video);

   procedure Remove_From_Playlists (This : in out T_Room; Item_ID : in T_Playlist_Item_ID);

   procedure Up_Vote_Playlist_Item (This : in out T_Room; Item_ID : in T_Playlist_Item_ID);

   procedure Add_Like (This : in out T_Room; Video : in T_Video);

   procedure Remove_Like (This : in out T_Room; Video : in T_Video);

   procedure Next_Room_Video (This : in out T_Room);

   procedure Next_Client_Video (This : in out T_Room; Session_ID : in AWS.Session.ID);

   procedure Set_Client_Display_Player
     (This : in out T_Room; Session_ID : in AWS.Session.ID; Display : in Boolean);

   procedure Set_Client_Sync_With_Room
     (This : in out T_Room; Session_ID : in AWS.Session.ID; Sync : in Boolean);

   function Get_Name (This : in T_Room) return String;

   function Get_Current_Video (This : in out T_Room) return T_Video;

   function Get_Video_Search_Results (This : in T_Room; Search_Input : in String)
     return Video_Vectors.Vector;

   function Get_Historic (This : in T_Room) return Video_Vectors.Vector;

   function Get_Likes (This : in T_Room) return Video_Vectors.Vector;

   function Is_Video_Liked (This : in T_Room; Video : in T_Video) return Boolean;

   function Get_Current_Client_Video (This : in T_Room; Session_ID : in AWS.Session.ID)
     return T_Video;

   function Get_Client_Playlist (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Playlist_Vectors.Vector;

   function Get_Client_Display_Player (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Boolean;

   function Get_Client_Sync_With_Room (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Boolean;

   function Client_Has_Nothing_To_Play (This : in out T_Room; Session_ID : in AWS.Session.ID)
     return Boolean;

   function Get_Number_Clients (This : in T_Room) return Natural;

   function Get_Number_Clients_Sync (This : in T_Room) return Natural;

   function Get_Room_Next_Video_Ready (This : in T_Room) return Boolean;

private

   procedure Update_No_Player_Clients (This : in out T_Room);

   function Count_Number_Of_Clients_Sync (This : in T_Room) return Natural;

   function Find_Client_From_Session_ID (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Client.T_Client_Class_Access;

   function Is_Client_Sync_And_Play (This : in out T_Room) return Boolean;

   procedure Remove_Disconnected_Client (This : in out T_Room);

   function Select_Related_Video (This : in out T_Room; Related_Videos : in Video_Vectors.Vector)
     return T_Video;

   -- Accessors protected by mutex
   procedure Set_Video (This : in out T_Room; Video : in T_Video);
   procedure Playlist_Append (This : in out T_Room; Item : in T_Playlist_Item);
   procedure Playlist_Delete_First (This : in out T_Room);
   procedure Playlist_Remove_Item (This : in out T_Room; Item_ID : in T_Playlist_Item_ID);
   procedure Playlist_Up_Vote_Item (This : in out T_Room; Item_ID : in T_Playlist_Item_ID);
   function Get_Video (This : in out T_Room) return T_Video;
   function Get_Playlist (This : in out T_Room) return Playlist_Vectors.Vector;
   function Get_Playlist_First (This : in out T_Room) return T_Playlist_Item;
   function Get_Playlist_Is_Empty (This : in out T_Room) return Boolean;

   -- Dummy function to instantiate a vector, for now comparing Client.T_Client type is useless
   function Client_Compare (Left, Right : Client.T_Client_Class_Access) return Boolean is (False);

   package Client_Vectors is new Ada.Containers.Vectors
     (Natural, Client.T_Client_Class_Access, Client_Compare);

   protected type T_Mutex is
      entry Seize;
      procedure Release;
   private
      Owned : Boolean := False;
   end T_Mutex;

   MAX_LAST_ROOM_VIDEOS : constant := 10;

   type T_Room is tagged limited record
      Name               : Unbounded_String;
      Room_Current_Video : T_Video :=
        (Video_Title => To_Unbounded_String ("no video played"), others => <>);
      Room_Playlist : Playlist_Vectors.Vector := Playlist_Vectors.Empty_Vector;
      Current_Playlist_Item_ID  : T_Playlist_Item_ID := T_Playlist_Item_ID'First;
      Room_Sync_Task            : T_Room_Sync_Task_Access;
      Room_Current_Video_Active : Boolean := False;
      Room_Next_Video_Ready     : Boolean := False;
      Room_Video_Playlist_Mutex : T_Mutex;
      Room_Callback_Mutex       : T_Mutex;
      Client_List               : Client_Vectors.Vector := Client_Vectors.Empty_Vector;
      Number_Of_Clients_Sync    : Natural := 0;
      Last_Request_Time         : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      DB                        : Database.T_Database_Class_Access;
   end record;

end Room;
