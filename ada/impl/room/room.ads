with Ada.Numerics.Float_Random;
with Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aws.Session;

with Api.Dispatcher;
with Client.List;
with Database;
with Mutex;
with Song.List;
with Song.Item.List;
with Web_Methods.Websocket;

package Room is

   type T_Room (Name_Length : Positive) is tagged limited private;
   type T_Room_Access is access all T_Room;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize
     (Name           : in String;
      Ambi_Database  : in not null Database.T_Database_Class_Access;
      Api_Dispatcher : in not null Api.Dispatcher.T_Dispatcher_Access;
      Websocket : in not null Web_Methods.Websocket.T_Websocket_Class_Access) return T_Room_Access;

   -------------------------------------------------------------------------------------------------
   -- Equality operator
   -------------------------------------------------------------------------------------------------
   function "=" (Left, Right : in T_Room) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Lock
   -------------------------------------------------------------------------------------------------
   procedure Lock (This : in out T_Room);

   -------------------------------------------------------------------------------------------------
   -- Unlock
   -------------------------------------------------------------------------------------------------
   procedure Unlock (This : in out T_Room);

   -------------------------------------------------------------------------------------------------
   -- Add_Client
   -------------------------------------------------------------------------------------------------
   procedure Add_Client (This : in out T_Room; Session_Id : in Aws.Session.Id);

   -------------------------------------------------------------------------------------------------
   -- Display_Client_Player
   -------------------------------------------------------------------------------------------------
   procedure Display_Client_Player
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id;
      Display    : in     Boolean);

   -------------------------------------------------------------------------------------------------
   -- Sync_Client_With_Room
   -------------------------------------------------------------------------------------------------
   procedure Sync_Client_With_Room
     (This       : in out T_Room;
      Session_Id : in     Aws.Session.Id;
      Sync       : in     Boolean);

   -------------------------------------------------------------------------------------------------
   -- Add_Song_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Add_Song_To_Playlist
     (This         : in out T_Room;
      Session_Id   : in     Aws.Session.Id;
      New_Song     : in     Song.T_Song;
      Low_Priority : in     Boolean := False);

   -------------------------------------------------------------------------------------------------
   -- Add_First_Result_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Add_First_Result_To_Playlist (This : in out T_Room; Search_Input : in String);

   -------------------------------------------------------------------------------------------------
   -- Remove_Item_From_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Remove_Item_From_Playlist (This : in out T_Room; Item_Id : in Song.Item.T_Item_Id);

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote_Playlist_Item (This : in out T_Room; Item_Id : in Song.Item.T_Item_Id);

   -------------------------------------------------------------------------------------------------
   -- Add_Like
   -------------------------------------------------------------------------------------------------
   procedure Add_Like (This : in out T_Room; New_Song : in Song.T_Song);

   -------------------------------------------------------------------------------------------------
   -- Remove_Like
   -------------------------------------------------------------------------------------------------
   procedure Remove_Like (This : in out T_Room; Old_Song : in Song.T_Song);

   -------------------------------------------------------------------------------------------------
   -- Next_Song
   -------------------------------------------------------------------------------------------------
   procedure Next_Song (This : in out T_Room);

   -------------------------------------------------------------------------------------------------
   -- Add_Chat_Message
   -------------------------------------------------------------------------------------------------
   procedure Add_Chat_Message (This : in out T_Room; Message : in String);

   -------------------------------------------------------------------------------------------------
   -- Shuffle_Likes_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Shuffle_Likes_To_Playlist (This : in out T_Room; Session_Id : in Aws.Session.Id);

   -------------------------------------------------------------------------------------------------
   -- Get_Name
   -------------------------------------------------------------------------------------------------
   function Get_Name (This : in T_Room) return String;

   -------------------------------------------------------------------------------------------------
   -- Is_Client_Registered
   -------------------------------------------------------------------------------------------------
   function Is_Client_Registered (This : in T_Room; Session_Id : in Aws.Session.Id) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Get_Client
   -------------------------------------------------------------------------------------------------
   function Get_Client
     (This       : in T_Room;
      Session_Id : in Aws.Session.Id) return Client.T_Client_Access;

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Of_Clients
   -------------------------------------------------------------------------------------------------
   function Get_Number_Of_Clients (This : in T_Room) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Song
   -------------------------------------------------------------------------------------------------
   function Get_Current_Song (This : in T_Room) return Song.T_Song;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (This : in T_Room) return Song.Item.List.T_Item_List;

   -------------------------------------------------------------------------------------------------
   -- Get_Suggestions
   -------------------------------------------------------------------------------------------------
   function Get_Suggestions (This : in T_Room) return Song.List.T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in out T_Room;
      Session_Id   : in     Aws.Session.Id;
      Search_Input : in     String) return Song.List.T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Get_History
   -------------------------------------------------------------------------------------------------
   function Get_History (This : in T_Room) return Song.List.T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Get_Likes
   -------------------------------------------------------------------------------------------------
   function Get_Likes (This : in T_Room) return Song.List.T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Is_Song_Liked
   -------------------------------------------------------------------------------------------------
   function Is_Song_Liked (This : in T_Room; Song_To_Check : in Song.T_Song) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Is_Auto_Playback_Requested
   -------------------------------------------------------------------------------------------------
   function Is_Auto_Playback_Requested (This : in T_Room) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Get_Next_Song_Votes
   -------------------------------------------------------------------------------------------------
   function Get_Next_Song_Votes (This : in T_Room) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Get_Chat_Log
   -------------------------------------------------------------------------------------------------
   function Get_Chat_Log (This : in T_Room) return String;

private

   -------------------------------------------------------------------------------------------------
   -- T_Sync_Task
   -------------------------------------------------------------------------------------------------
   task type T_Sync_Task (This : T_Room_Access := null) is
      entry Start_Playing;
      entry Skip_Song;
   end T_Sync_Task;

   type T_Sync_Task_Access is access T_Sync_Task;

   -------------------------------------------------------------------------------------------------
   -- Update_Auto_Playback_Requested
   -------------------------------------------------------------------------------------------------
   procedure Update_Auto_Playback_Requested (This : in out T_Room);

   -------------------------------------------------------------------------------------------------
   -- Remove_Disconnected_Client
   -------------------------------------------------------------------------------------------------
   procedure Remove_Disconnected_Client (This : in out T_Room);

   Number_Of_Suggestions    : constant := 10;
   Number_Of_Excluded_Songs : constant := 10;

   type T_Room (Name_Length : Positive) is tagged limited record
      Name                    : String (Positive'First .. Name_Length);
      Db                      : Database.T_Database_Class_Access;
      Api_Dispatcher          : Api.Dispatcher.T_Dispatcher_Access;
      Websocket               : Web_Methods.Websocket.T_Websocket_Class_Access;
      Client_List             : Client.List.T_Client_List;
      Auto_Playback_Requested : Boolean;
      Current_Song            : Song.T_Song;
      Playlist                : Song.Item.List.T_Item_List;
      Song_Suggestions        : Song.List.T_Song_List;
      Current_Item_Id         : Song.Item.T_Item_Id;
      Next_Song_Votes         : Natural;
      Sync_Task               : T_Sync_Task_Access;
      Next_Song_Ready         : Boolean;
      Last_Request_Time       : Ada.Real_Time.Time;
      Block_Websocket         : Boolean;
      Global_Mutex            : Mutex.T_Mutex;
      Chat_Log                : Unbounded_String;
      Random_Generator        : Ada.Numerics.Float_Random.Generator;
   end record;

end Room;
