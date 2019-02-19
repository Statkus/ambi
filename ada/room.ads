with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Session;

with Client;
with Playlist;
with YT_API;

package Room is

   type T_Room is tagged private;

   -- Dummy function to instantiate a vector, for now comparing Client.T_Client type is useless
   function Client_Compare (Left, Right : Client.T_Client_Class_Access) return Boolean;

   package Client_Vectors is new Ada.Containers.Vectors (Natural, Client.T_Client_Class_Access, Client_Compare);

   procedure Add_Client (This : in out T_Room; Session_ID : in AWS.Session.ID);

   procedure Add_Video_To_Clients_Playlist (This : in out T_Room; Video : in YT_API.T_Video);

   procedure Remove_First_Client_Playlist_Video
     (This : in out T_Room; Session_ID : in AWS.Session.ID);

   procedure Set_Current_Client_Video (This : in out T_Room; Session_ID : in AWS.Session.ID);

   procedure Set_Video_Search_Results
     (This : in out T_Room; Video_Search_Results : in YT_API.T_Video_Search_Results);

   function Get_Current_Client_Video (This : in T_Room; Session_ID : in AWS.Session.ID)
     return YT_API.T_Video;

   function Get_Video_Search_Results (This : in T_Room) return YT_API.T_Video_Search_Results;

   function Get_Client_Playlist (This : in T_Room; Session_ID : in AWS.Session.ID)
     return Playlist.Video_Vectors.Vector;

private

   type T_Room is tagged record
      Video_Search_Results : YT_API.T_Video_Search_Results;
      Client_List          : Client_Vectors.Vector := Client_Vectors.Empty_Vector;
      Client_ID_Counter    : Integer := 0;
   end record;

end Room;
