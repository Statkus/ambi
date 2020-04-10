with Api;

package body Client is

   -------------------------------------------------------------------------------------------------
   -- Set_Session_ID
   -------------------------------------------------------------------------------------------------
   procedure Set_Session_Id (This : in out T_Client; Session_Id : in Aws.Session.Id) is
   begin
      This.Session_Id := Session_Id;
   end Set_Session_Id;

   -------------------------------------------------------------------------------------------------
   -- Set_Current_Song
   -------------------------------------------------------------------------------------------------
   procedure Set_Current_Song (This : in out T_Client) is
   begin
      if not This.Client_Playlist.Is_Empty then
         This.Client_Current_Song :=
           Playlist_Item_Vectors.Element (This.Client_Playlist.First).Get_Song;
         This.Nothing_To_Play := False;
      else
         This.Nothing_To_Play := True;
      end if;
   end Set_Current_Song;

   -------------------------------------------------------------------------------------------------
   -- Set_Current_Song
   -------------------------------------------------------------------------------------------------
   procedure Set_Current_Song (This : in out T_Client; Current_Song : in T_Song) is
      use type Api.T_Api_Provider;
   begin
      This.Client_Current_Song := Current_Song;
      This.Nothing_To_Play     := Current_Song.Get_Provider = Api.No_Provider_Api;
   end Set_Current_Song;

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
   procedure Remove_Item_From_Playlist (This : in out T_Client; Item_Id : in T_Playlist_Item_Id) is
      Item_Cursor : Playlist_Item_Vectors.Cursor := This.Client_Playlist.First;
   begin
      while Playlist_Item_Vectors.Has_Element (Item_Cursor) loop
         if Playlist_Item_Vectors.Element (Item_Cursor).Get_Id = Item_Id then
            This.Client_Playlist.Delete (Item_Cursor);
         end if;

         Playlist_Item_Vectors.Next (Item_Cursor);
      end loop;
   end Remove_Item_From_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote_Playlist_Item (This : in out T_Client; Item_Id : in T_Playlist_Item_Id) is
   begin
      Up_Vote_Playlist_Item (This.Client_Playlist, Item_Id);
   end Up_Vote_Playlist_Item;

   -------------------------------------------------------------------------------------------------
   -- Set_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Set_Playlist (This : in out T_Client; Client_Playlist : in T_Playlist) is
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
   function Get_Session_Id (This : in T_Client) return Aws.Session.Id is (This.Session_Id);

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Song
   -------------------------------------------------------------------------------------------------
   function Get_Current_Song (This : in T_Client) return T_Song is (This.Client_Current_Song);

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (This : in T_Client) return T_Playlist is (This.Client_Playlist);

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
   function Get_Last_Request_Time
     (This : in out T_Client) return Ada.Real_Time.Time is
     (This.Last_Request_Time);

end Client;
