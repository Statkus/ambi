with AWS.Session;

with Video_List; use Video_List;
with YT_API;

package Client is

   type T_Client is tagged limited private;

   type T_Client_Class_Access is access all T_Client'Class;

   procedure Set_Session_ID (This : in out T_Client; Session_ID : in AWS.Session.ID);

   procedure Set_Current_Video (This : in out T_Client);

   procedure Set_Current_Video (This : in out T_Client; Video : in YT_API.T_Video);

   procedure Add_Video_To_Playlist (This : in out T_Client; Video : in YT_API.T_Video);

   procedure Remove_First_Playlist_Video (This : in out T_Client);

   procedure Set_Playlist
     (This : in out T_Client; Client_Playlist : in Video_Vectors.Vector);

   procedure Set_Display_Player (This : in out T_Client; Display : in Boolean);

   procedure Set_Sync_With_Room (This : in out T_Client; Sync : in Boolean);

   function Get_Session_ID (This : in T_Client) return AWS.Session.ID;

   function Get_Current_Video (This : in T_Client) return YT_API.T_Video;

   function Get_Playlist (This : in T_Client) return Video_Vectors.Vector;

   function Get_Display_Player (This : in T_Client) return Boolean;

   function Get_Sync_With_Room (This : in T_Client) return Boolean;

   function Has_Nothing_To_Play (This : in T_Client) return Boolean;

private

   type T_Client is tagged limited record
      Session_ID           : AWS.Session.ID;
      Client_Current_Video : YT_API.T_Video;
      Client_Playlist      : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
      Display_Player       : Boolean := True;
      Sync_With_Room       : Boolean := False;
      Nothing_To_Play      : Boolean := True;
   end record;

end Client;
