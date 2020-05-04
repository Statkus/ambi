package body Client is

   use type Aws.Session.Id;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize
     (Session_Id : in Aws.Session.Id) return T_Client_Access is
     (new T_Client'
        (Id                => Session_Id,
         Current_Song      => Song.Initialize,
         Playlist          => Song.Item.List.Initialize,
         State             => Sync,
         Last_Request_Time => Ada.Real_Time.Clock));

   -------------------------------------------------------------------------------------------------
   -- Equality operator
   -------------------------------------------------------------------------------------------------
   function "=" (Left, Right : in T_Client) return Boolean is (Left.Id = Right.Id);

   -------------------------------------------------------------------------------------------------
   -- Display_Player
   -------------------------------------------------------------------------------------------------
   procedure Display_Player (This : in out T_Client; Display : in Boolean) is
      Display_To_State : constant array (Boolean, T_Client_State) of T_Client_State :=
        (False => (others => No_Player), True => (Desync => Desync, others => Sync));
   begin
      This.State := Display_To_State (Display, This.State);
   end Display_Player;

   -------------------------------------------------------------------------------------------------
   -- Sync_With_Room
   -------------------------------------------------------------------------------------------------
   procedure Sync_With_Room
     (This              : in out T_Client;
      Synced            : in     Boolean;
      Room_Current_Song : in     Song.T_Song;
      Room_Playlist     : in     Song.Item.List.T_Item_List)
   is
      Sync_To_State : constant array (Boolean) of T_Client_State := (False => Desync, True => Sync);
   begin
      if This.State /= Desync and not Synced then
         This.Current_Song := Room_Current_Song;
         This.Playlist.Set (Room_Playlist.Get);
      end if;

      This.State := Sync_To_State (Synced);
   end Sync_With_Room;

   -------------------------------------------------------------------------------------------------
   -- Update_Last_Request_Time
   -------------------------------------------------------------------------------------------------
   procedure Update_Last_Request_Time (This : in out T_Client) is
   begin
      This.Last_Request_Time := Ada.Real_Time.Clock;
   end Update_Last_Request_Time;

   -------------------------------------------------------------------------------------------------
   -- Next_Song
   -------------------------------------------------------------------------------------------------
   procedure Next_Song (This : in out T_Client) is
   begin
      if This.State = Desync then
         if not This.Playlist.Is_Empty then
            This.Current_Song := This.Playlist.First_Element.Get_Song;
            This.Playlist.Delete_First;
         else
            This.Current_Song := Song.Initialize;
         end if;
      end if;
   end Next_Song;

   -------------------------------------------------------------------------------------------------
   -- Add_Item_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Add_Item_To_Playlist (This : in out T_Client; Item : in Song.Item.T_Item) is
   begin
      if This.State = Desync then
         This.Playlist.Append (Item);
      end if;
   end Add_Item_To_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Remove_Item_From_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Remove_Item_From_Playlist (This : in out T_Client; Item_Id : in Song.Item.T_Item_Id) is
   begin
      if This.State = Desync then
         This.Playlist.Delete (Item_Id);
      end if;
   end Remove_Item_From_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote_Playlist_Item (This : in out T_Client; Item_Id : in Song.Item.T_Item_Id) is
   begin
      if This.State = Desync then
         This.Playlist.Up_Vote (Item_Id);
      end if;
   end Up_Vote_Playlist_Item;

   -------------------------------------------------------------------------------------------------
   -- Get_Id
   -------------------------------------------------------------------------------------------------
   function Get_Id (This : in T_Client) return Aws.Session.Id is (This.Id);

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Song
   -------------------------------------------------------------------------------------------------
   function Get_Current_Song (This : in T_Client) return Song.T_Song is (This.Current_Song);

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (This : in T_Client) return Song.Item.List.T_Item_List is
   begin
      return Item_List : Song.Item.List.T_Item_List do
         Item_List.Set (This.Playlist.Get);
      end return;
   end Get_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Is_Player_Displayed
   -------------------------------------------------------------------------------------------------
   function Is_Player_Displayed (This : in T_Client) return Boolean is (This.State /= No_Player);

   -------------------------------------------------------------------------------------------------
   -- Is_Sync_With_Room
   -------------------------------------------------------------------------------------------------
   function Is_Sync_With_Room (This : in T_Client) return Boolean is (This.State /= Desync);

   -------------------------------------------------------------------------------------------------
   -- Get_Last_Request_Time
   -------------------------------------------------------------------------------------------------
   function Get_Last_Request_Time
     (This : in out T_Client) return Ada.Real_Time.Time is
     (This.Last_Request_Time);

end Client;
