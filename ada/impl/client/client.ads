with Ada.Real_Time;

with Aws.Session;

with Song.Item.List;

package Client is

   type T_Client is tagged limited private;
   type T_Client_Access is access all T_Client;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize (Session_Id : in Aws.Session.Id) return T_Client_Access;

   -------------------------------------------------------------------------------------------------
   -- Equality operator
   -------------------------------------------------------------------------------------------------
   function "=" (Left, Right : in T_Client) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Display_Player
   -------------------------------------------------------------------------------------------------
   procedure Display_Player (This : in out T_Client; Display : in Boolean);

   -------------------------------------------------------------------------------------------------
   -- Sync_With_Room
   -------------------------------------------------------------------------------------------------
   procedure Sync_With_Room
     (This              : in out T_Client;
      Synced            : in     Boolean;
      Room_Current_Song : in     Song.T_Song;
      Room_Playlist     : in     Song.Item.List.T_Item_List);

   -------------------------------------------------------------------------------------------------
   -- Update_Last_Request_Time
   -------------------------------------------------------------------------------------------------
   procedure Update_Last_Request_Time (This : in out T_Client);

   -------------------------------------------------------------------------------------------------
   -- Next_Song
   -------------------------------------------------------------------------------------------------
   procedure Next_Song (This : in out T_Client);

   -------------------------------------------------------------------------------------------------
   -- Add_Item_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Add_Item_To_Playlist (This : in out T_Client; Item : in Song.Item.T_Item);

   -------------------------------------------------------------------------------------------------
   -- Remove_Item_From_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Remove_Item_From_Playlist (This : in out T_Client; Item_Id : in Song.Item.T_Item_Id);

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote_Playlist_Item (This : in out T_Client; Item_Id : in Song.Item.T_Item_Id);

   -------------------------------------------------------------------------------------------------
   -- Get_Id
   -------------------------------------------------------------------------------------------------
   function Get_Id (This : in T_Client) return Aws.Session.Id;

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Song
   -------------------------------------------------------------------------------------------------
   function Get_Current_Song (This : in T_Client) return Song.T_Song;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (This : in T_Client) return Song.Item.List.T_Item_List;

   -------------------------------------------------------------------------------------------------
   -- Is_Player_Displayed
   -------------------------------------------------------------------------------------------------
   function Is_Player_Displayed (This : in T_Client) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Is_Sync_With_Room
   -------------------------------------------------------------------------------------------------
   function Is_Sync_With_Room (This : in T_Client) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Get_Last_Request_Time
   -------------------------------------------------------------------------------------------------
   function Get_Last_Request_Time (This : in out T_Client) return Ada.Real_Time.Time;

private

   type T_Client_State is (Sync, Desync, No_Player);

   type T_Client is tagged limited record
      Id                : Aws.Session.Id;
      Current_Song      : Song.T_Song;
      Playlist          : Song.Item.List.T_Item_List;
      State             : T_Client_State;
      Last_Request_Time : Ada.Real_Time.Time;
   end record;

end Client;
