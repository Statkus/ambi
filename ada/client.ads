with AWS.Session;

with Playlist;
with YT_API;

package Client is

   type T_Client_ID is new Natural;

   type T_Client is tagged private;

   type T_Client_Class_Access is access all T_Client'Class;

   procedure Set_Session_ID (This : in out T_Client; Session_ID : in AWS.Session.ID);

   procedure Set_Current_Video (This : in out T_Client);

   procedure Add_Video_To_Playlist (This : in out T_Client; Video : in YT_API.T_Video);

   procedure Remove_First_Playlist_Video (This : in out T_Client);

   function Get_Session_ID (This : in T_Client) return AWS.Session.ID;

   function Get_Current_Video (This : in T_Client) return YT_API.T_Video;

   function Get_Playlist (This : in T_Client) return Playlist.Video_Vectors.Vector;

private

   type T_Client is tagged record
      Session_ID           : AWS.Session.ID;
      Client_Current_Video : YT_API.T_Video;
      Client_Playlist      : Playlist.Video_Vectors.Vector := Playlist.Video_Vectors.Empty_Vector;
   end record;

end Client;
