with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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
      if not This.Client_Playlist.Is_Empty then
         This.Client_Current_Video := Playlist_Vectors.Element (This.Client_Playlist.First).Video;
         This.Nothing_To_Play := False;
      else
         This.Nothing_To_Play := True;
      end if;
   end Set_Current_Video;

   -------------------------------------------------------------------------------------------------
   -- Set_Current_Video
   -------------------------------------------------------------------------------------------------
   procedure Set_Current_Video (This : in out T_Client; Video : in T_Video) is
   begin
      This.Client_Current_Video := Video;
      This.Nothing_To_Play      := To_String (Video.Video_Title) = "no video played";
   end Set_Current_Video;

   -------------------------------------------------------------------------------------------------
   -- Set_Last_Request_Time
   -------------------------------------------------------------------------------------------------
   procedure Set_Last_Request_Time (This : in out T_Client) is
   begin
      This.Last_Request_Time := Ada.Real_Time.Clock;
   end Set_Last_Request_Time;

   -------------------------------------------------------------------------------------------------
   -- Add_Item_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Add_Item_To_Playlist (This : in out T_Client; Item : in T_Playlist_Item) is
   begin
      This.Client_Playlist.Append (Item);
   end Add_Item_To_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Remove_First_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Remove_First_Playlist_Item (This : in out T_Client) is
   begin
      if not This.Client_Playlist.Is_Empty then
         This.Client_Playlist.Delete_First;
      end if;
   end Remove_First_Playlist_Item;

   -------------------------------------------------------------------------------------------------
   -- Remove_Item_From_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Remove_Item_From_Playlist (This : in out T_Client; Item_ID : in T_Playlist_Item_ID) is
      Item_Cursor : Playlist_Vectors.Cursor := This.Client_Playlist.First;
   begin
      while Playlist_Vectors.Has_Element (Item_Cursor) loop
         if Playlist_Vectors.Element (Item_Cursor).ID = Item_ID then
            This.Client_Playlist.Delete (Item_Cursor);
         end if;

         Playlist_Vectors.Next (Item_Cursor);
      end loop;
   end Remove_Item_From_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Set_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Set_Playlist
     (This : in out T_Client; Client_Playlist : in Playlist_Vectors.Vector) is
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
   -- Set_Sync_With_Room
   -------------------------------------------------------------------------------------------------
   procedure Set_Sync_With_Room (This : in out T_Client; Sync : in Boolean) is
   begin
      This.Sync_With_Room := Sync;
   end Set_Sync_With_Room;

   -------------------------------------------------------------------------------------------------
   -- Get_Session_ID
   -------------------------------------------------------------------------------------------------
   function Get_Session_ID (This : in T_Client) return AWS.Session.ID is (This.Session_ID);

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Video
   -------------------------------------------------------------------------------------------------
   function Get_Current_Video (This : in T_Client) return T_Video is
     (This.Client_Current_Video);

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (This : in T_Client) return Playlist_Vectors.Vector is
     (This.Client_Playlist);

   -------------------------------------------------------------------------------------------------
   -- Get_Display_Player
   -------------------------------------------------------------------------------------------------
   function Get_Display_Player (This : in T_Client) return Boolean is (This.Display_Player);

   -------------------------------------------------------------------------------------------------
   -- Get_Sync_With_Room
   -------------------------------------------------------------------------------------------------
   function Get_Sync_With_Room (This : in T_Client) return Boolean is (This.Sync_With_Room);

   -------------------------------------------------------------------------------------------------
   -- Has_Nothing_To_Play
   -------------------------------------------------------------------------------------------------
   function Has_Nothing_To_Play (This : in T_Client) return Boolean is (This.Nothing_To_Play);

   -------------------------------------------------------------------------------------------------
   -- Get_Last_Request_Time
   -------------------------------------------------------------------------------------------------
   function Get_Last_Request_Time (This : in out T_Client) return Ada.Real_Time.Time is
     (This.Last_Request_Time);

end Client;
