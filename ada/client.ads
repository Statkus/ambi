with Ada.Real_Time;

with AWS.Session;

with List; use List;

package Client is

   type T_Client is tagged limited private;

   type T_Client_Class_Access is access all T_Client'Class;

   procedure Set_Session_ID (This : in out T_Client; Session_ID : in AWS.Session.ID);

   procedure Set_Current_Video (This : in out T_Client);

   procedure Set_Current_Video (This : in out T_Client; Video : in T_Video);

   procedure Set_Last_Request_Time (This : in out T_Client);

   procedure Add_Item_To_Playlist (This : in out T_Client; Item : in T_Playlist_Item);

   procedure Remove_First_Playlist_Item (This : in out T_Client);

   procedure Remove_Item_From_Playlist (This : in out T_Client; Item_ID : in T_Playlist_Item_ID);

   procedure Up_Vote_Playlist_Item (This : in out T_Client; Item_ID : in T_Playlist_Item_ID);

   procedure Set_Playlist
     (This : in out T_Client; Client_Playlist : in Playlist_Vectors.Vector);

   procedure Set_Display_Player (This : in out T_Client; Display : in Boolean);

   procedure Set_Sync_With_Room (This : in out T_Client; Sync : in Boolean);

   function Get_Session_ID (This : in T_Client) return AWS.Session.ID;

   function Get_Current_Video (This : in T_Client) return T_Video;

   function Get_Playlist (This : in T_Client) return Playlist_Vectors.Vector;

   function Get_Display_Player (This : in T_Client) return Boolean;

   function Get_Sync_With_Room (This : in T_Client) return Boolean;

   function Has_Nothing_To_Play (This : in T_Client) return Boolean;

   function Get_Last_Request_Time (This : in out T_Client) return Ada.Real_Time.Time;

private

   type T_Client is tagged limited record
      Session_ID           : AWS.Session.ID;
      Client_Current_Video : T_Video;
      Client_Playlist      : Playlist_Vectors.Vector := Playlist_Vectors.Empty_Vector;
      Display_Player       : Boolean := True;
      Sync_With_Room       : Boolean := False;
      Nothing_To_Play      : Boolean := True;
      Last_Request_Time    : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   end record;

end Client;
