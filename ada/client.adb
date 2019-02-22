package body Client is

   -------------------------------------------------------------------------------------------------
   -- Set_Session_ID
   -------------------------------------------------------------------------------------------------
   procedure Set_Session_ID (This : in out T_Client; Session_ID : in AWS.Session.ID) is
   begin
      This.Session_ID := Session_ID;
   end Set_Session_ID;

   -------------------------------------------------------------------------------------------------
   -- Set_Current_Video
   -------------------------------------------------------------------------------------------------
   procedure Set_Current_Video (This : in out T_Client) is
   begin
      This.Client_Current_Video := Playlist.Video_Vectors.Element (This.Client_Playlist.First);
   end Set_Current_Video;

   -------------------------------------------------------------------------------------------------
   -- Set_Current_Video
   -------------------------------------------------------------------------------------------------
   procedure Set_Current_Video (This : in out T_Client; Video : in YT_API.T_Video) is
   begin
      This.Client_Current_Video := Video;
   end Set_Current_Video;

   -------------------------------------------------------------------------------------------------
   -- Add_Video_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Add_Video_To_Playlist (This : in out T_Client; Video : in YT_API.T_Video) is
   begin
      This.Client_Playlist.Append (Video);
   end Add_Video_To_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Remove_First_Playlist_Video
   -------------------------------------------------------------------------------------------------
   procedure Remove_First_Playlist_Video (This : in out T_Client) is
   begin
      This.Client_Playlist.Delete_First;
   end Remove_First_Playlist_Video;

   -------------------------------------------------------------------------------------------------
   -- Set_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Set_Playlist
     (This : in out T_Client; Client_Playlist : in Playlist.Video_Vectors.Vector) is
   begin
      This.Client_Playlist := Client_Playlist;
   end Set_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Set_Display_Player
   -------------------------------------------------------------------------------------------------
   procedure Set_Display_Player (This : in out T_Client; Display : in Boolean) is
   begin
      This.Display_Player := Display;
   end Set_Display_Player;

   -------------------------------------------------------------------------------------------------
   -- Get_Session_ID
   -------------------------------------------------------------------------------------------------
   function Get_Session_ID (This : in T_Client) return AWS.Session.ID is (This.Session_ID);

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Video
   -------------------------------------------------------------------------------------------------
   function Get_Current_Video (This : in T_Client) return YT_API.T_Video is
     (This.Client_Current_Video);

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (This : in T_Client) return Playlist.Video_Vectors.Vector is
     (This.Client_Playlist);

   -------------------------------------------------------------------------------------------------
   -- Get_Display_Player
   -------------------------------------------------------------------------------------------------
   function Get_Display_Player (This : in T_Client) return Boolean is (This.Display_Player);

end Client;
