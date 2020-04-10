with Ada.Real_Time;

with Aws.Session;

with Playlist;      use Playlist;
with Playlist_Item; use Playlist_Item;
with Song;          use Song;

package Client is

   type T_Client is tagged limited private;
   type T_Client_Class_Access is access all T_Client'Class;

   procedure Set_Session_Id (This : in out T_Client; Session_Id : in Aws.Session.Id);

   procedure Set_Current_Song (This : in out T_Client);

   procedure Set_Current_Song (This : in out T_Client; Current_Song : in T_Song);

   procedure Set_Last_Request_Time (This : in out T_Client);

   procedure Add_Item_To_Playlist (This : in out T_Client; Item : in T_Playlist_Item);

   procedure Remove_First_Playlist_Item (This : in out T_Client);

   procedure Remove_Item_From_Playlist (This : in out T_Client; Item_Id : in T_Playlist_Item_Id);

   procedure Up_Vote_Playlist_Item (This : in out T_Client; Item_Id : in T_Playlist_Item_Id);

   procedure Set_Playlist (This : in out T_Client; Client_Playlist : in T_Playlist);

   procedure Set_Display_Player (This : in out T_Client; Display : in Boolean);

   procedure Set_Sync_With_Room (This : in out T_Client; Sync : in Boolean);

   function Get_Session_Id (This : in T_Client) return Aws.Session.Id;

   function Get_Current_Song (This : in T_Client) return T_Song;

   function Get_Playlist (This : in T_Client) return T_Playlist;

   function Get_Display_Player (This : in T_Client) return Boolean;

   function Get_Sync_With_Room (This : in T_Client) return Boolean;

   function Has_Nothing_To_Play (This : in T_Client) return Boolean;

   function Get_Last_Request_Time (This : in out T_Client) return Ada.Real_Time.Time;

private

   type T_Client is tagged limited record
      Session_Id          : Aws.Session.Id;
      Client_Current_Song : T_Song             := Song.Constructors.Initialize;
      Client_Playlist     : T_Playlist         := Playlist.Constructors.Initialize;
      Display_Player      : Boolean            := True;
      Sync_With_Room      : Boolean            := False;
      Nothing_To_Play     : Boolean            := True;
      Last_Request_Time   : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   end record;

end Client;
