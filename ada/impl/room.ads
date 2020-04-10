with Ada.Containers.Vectors;
with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aws.Session;

with Api.Dispatcher;
with Client;
with Database;
with Playlist;      use Playlist;
with Playlist_Item; use Playlist_Item;
with Song;          use Song;
with Song_Vector;   use Song_Vector;

package Room is

   type T_Room is tagged limited private;
   type T_Room_Class_Access is access all T_Room'Class;

   task type T_Room_Sync_Task (This : T_Room_Class_Access := null) is
      entry Start_Room_Playlist;
      entry Next_Room_Song;
   end T_Room_Sync_Task;

   type T_Room_Sync_Task_Access is access T_Room_Sync_Task;

   function New_And_Initialize
     (Name           : in String;
      Db             : in not null Database.T_Database_Access;
      Api_Dispatcher : in Api.Dispatcher.T_Dispatcher_Access) return T_Room_Class_Access;

   procedure Set_Room_Sync_Task
     (This      : in out T_Room;
      Sync_Task : in     not null T_Room_Sync_Task_Access);

   procedure Lock (This : in out T_Room);
   procedure Unlock (This : in out T_Room);

   procedure Add_Client (This : in out T_Room; Session_Id : in Aws.Session.Id);

   procedure Set_Client_Last_Request_Time (This : in out T_Room; Session_Id : in Aws.Session.Id);

   function Is_Registered (This : in out T_Room; Session_Id : in Aws.Session.Id) return Boolean;

   procedure Add_Song_To_Playlists
     (This         : in out T_Room;
      Session_Id   : in     Aws.Session.Id;
      New_Song     : in     T_Song;
      Low_Priority : in     Boolean := False);

   procedure Remove_From_Playlists (This : in out T_Room; Item_Id : in T_Playlist_Item_Id);

   procedure Up_Vote_Playlist_Item (This : in out T_Room; Item_Id : in T_Playlist_Item_Id);

   procedure Add_Like (This : in out T_Room; New_Song : in T_Song);

   procedure Remove_Like (This : in out T_Room; Old_Song : in T_Song);

   procedure Next_Room_Song (This : in out T_Room);

   procedure Next_Client_Song (This : in out T_Room; Session_Id : in Aws.Session.Id);

   procedure Set_Client_Display_Player
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id;
      Display    : in     Boolean);

   procedure Set_Client_Sync_With_Room
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id;
      Sync       : in     Boolean);

   function Get_Name (This : in T_Room) return String;

   function Get_Current_Song (This : in out T_Room) return T_Song;

   function Get_Song_Search_Results
     (This         : in out T_Room;
      Session_Id   : in     Aws.Session.Id;
      Search_Input : in     String;
      Direct_Link  :    out Boolean) return T_Song_Vector;

   function Get_Historic (This : in T_Room) return T_Song_Vector;

   function Get_Likes (This : in T_Room) return T_Song_Vector;

   function Is_Song_Liked (This : in T_Room; Song_To_Check : in T_Song) return Boolean;

   function Get_Current_Client_Song
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return T_Song;

   function Get_Client_Playlist
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return T_Playlist;

   function Get_Client_Display_Player
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return Boolean;

   function Get_Client_Sync_With_Room
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return Boolean;

   function Client_Has_Nothing_To_Play
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id) return Boolean;

   function Get_Number_Clients (This : in T_Room) return Natural;

   function Get_Number_Clients_Sync (This : in T_Room) return Natural;

   function Get_Room_Next_Song_Ready (This : in T_Room) return Boolean;

private

   procedure Update_No_Player_Clients (This : in out T_Room);

   function Count_Number_Of_Clients_Sync (This : in T_Room) return Natural;

   function Find_Client_From_Session_Id
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return Client.T_Client_Class_Access;

   function Is_Client_Sync_And_Play (This : in out T_Room) return Boolean;

   procedure Remove_Disconnected_Client (This : in out T_Room);

   function Select_Related_Song
     (This          : in out T_Room;
      Related_Songs : in     T_Song_Vector) return T_Song;

   -- Accessors protected by mutex
   procedure Set_Song (This : in out T_Room; Current_Song : in T_Song);
   procedure Playlist_Append (This : in out T_Room; Item : in T_Playlist_Item);
   procedure Playlist_Delete_First (This : in out T_Room);
   procedure Playlist_Remove_Item (This : in out T_Room; Item_Id : in T_Playlist_Item_Id);
   procedure Playlist_Up_Vote_Item (This : in out T_Room; Item_Id : in T_Playlist_Item_Id);
   function Get_Song (This : in out T_Room) return T_Song;
   function Get_Playlist (This : in out T_Room) return T_Playlist;
   function Get_Playlist_First (This : in out T_Room) return T_Playlist_Item;
   function Get_Playlist_Is_Empty (This : in out T_Room) return Boolean;

   -- Dummy function to instantiate a vector, for now comparing Client.T_Client type is useless
   function Client_Compare (Left, Right : Client.T_Client_Class_Access) return Boolean is (False);

   package Client_Vectors is new Ada.Containers.Vectors
     (Natural,
      Client.T_Client_Class_Access,
      Client_Compare);

   protected type T_Mutex is
      entry Seize;
      procedure Release;
   private
      Owned : Boolean := False;
   end T_Mutex;

   Max_Last_Room_Songs : constant := 10;

   type T_Room is tagged limited record
      Name                     : Unbounded_String;
      Room_Current_Song        : T_Song                := Song.Constructors.Initialize;
      Room_Playlist            : T_Playlist            := Playlist.Constructors.Initialize;
      Current_Playlist_Item_Id : T_Playlist_Item_Id    := T_Playlist_Item_Id'First;
      Room_Sync_Task           : T_Room_Sync_Task_Access;
      Room_Current_Song_Active : Boolean               := False;
      Room_Next_Song_Ready     : Boolean               := False;
      Room_Song_Playlist_Mutex : T_Mutex;
      Room_Callback_Mutex      : T_Mutex;
      Client_List              : Client_Vectors.Vector := Client_Vectors.Empty_Vector;
      Number_Of_Clients_Sync   : Natural               := 0;
      Last_Request_Time        : Ada.Real_Time.Time    := Ada.Real_Time.Clock;
      Db                       : Database.T_Database_Access;
      Api_Dispatcher           : Api.Dispatcher.T_Dispatcher_Access;
   end record;

end Room;
