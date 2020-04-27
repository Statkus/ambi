with Ada.Unchecked_Deallocation;

with Aunit.Assertions; use Aunit.Assertions;

with Api;

package body Client.Test is

   use type Song.Item.List.Item_Vectors.Vector;

   -------------------------------------------------------------------------------------------------
   -- Free_Client
   -------------------------------------------------------------------------------------------------
   procedure Free_Client is new Ada.Unchecked_Deallocation (T_Client, T_Client_Access);

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Client_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine (This, Test_New_And_Initialize'Access, "Test New_And_Initialize");
      Register_Routine (This, Test_Equality_Operator'Access, "Test equality operator");
      Register_Routine (This, Test_Display_Player'Access, "Test Display_Player");
      Register_Routine (This, Test_Sync_With_Room'Access, "Test Sync_With_Room");
      Register_Routine
        (This,
         Test_Update_Last_Request_Time'Access,
         "Test Update_Last_Request_Time");
      Register_Routine (This, Test_Next_Song'Access, "Test Next_Song");
      Register_Routine (This, Test_Add_Item_To_Playlist'Access, "Test Add_Item_To_Playlist");
      Register_Routine
        (This,
         Test_Remove_Item_From_Playlist'Access,
         "Test Remove_Item_From_Playlist");
      Register_Routine (This, Test_Up_Vote_Playlist_Item'Access, "Test Up_Vote_Playlist_Item");
      Register_Routine (This, Test_Getter'Access, "Test getter");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Client_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Client tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_New_And_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use type Ada.Real_Time.Time;
      use type Aws.Session.Id;
      use type Song.T_Song;
      use type Song.Item.List.T_Item_List;

      Current_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

      New_Client : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
   begin
      Assert (New_Client /= null, "Null client returned.");
      Assert (New_Client.Id = Aws.Session.No_Session, "Wrong client ID.");
      Assert (New_Client.Current_Song = Song.Initialize, "Wrong current song.");
      Assert (New_Client.Playlist.Is_Empty, "Playlist not empty.");
      Assert (New_Client.Display_Player, "Player not displayed.");
      Assert (New_Client.Sync_With_Room, "Not synced with the room.");
      Assert
        (Ada.Real_Time.To_Duration (Current_Time - New_Client.Last_Request_Time) < 0.001,
         "Wrong last request time.");

      Free_Client (New_Client);
   end Test_New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Equality_Operator
   -------------------------------------------------------------------------------------------------
   procedure Test_Equality_Operator (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Client_1 : constant T_Client := (Id => Aws.Session.No_Session, others => <>);
      Client_2 : T_Client          := (Id => Aws.Session.No_Session, others => <>);
   begin
      Assert (Client_1 = Client_2, "Wrong equality operator.");

      Client_2.Id := Aws.Session.Create;

      Assert (Client_1 /= Client_2, "Wrong equality operator.");
   end Test_Equality_Operator;

   -------------------------------------------------------------------------------------------------
   -- Test_Display_Player
   -------------------------------------------------------------------------------------------------
   procedure Test_Display_Player (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      New_Client : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
   begin
      Display_Player (New_Client.all, True);

      Assert (New_Client.Display_Player, "Player not displayed.");
      Assert (New_Client.Sync_With_Room, "Not synced with the room.");

      Display_Player (New_Client.all, False);

      Assert (not New_Client.Display_Player, "Player displayed.");
      Assert (New_Client.Sync_With_Room, "Not synced with the room.");

      New_Client.Sync_With_Room := False;
      Display_Player (New_Client.all, True);

      Assert (New_Client.Display_Player, "Player not displayed.");
      Assert (not New_Client.Sync_With_Room, "Synced with the room.");

      New_Client.Sync_With_Room := False;
      New_Client.Display_Player := False;
      Display_Player (New_Client.all, False);

      Assert (not New_Client.Display_Player, "Player displayed.");
      Assert (New_Client.Sync_With_Room, "Not synced with the room.");

      New_Client.Display_Player := True;
      Display_Player (New_Client.all, False);

      Assert (not New_Client.Display_Player, "Player displayed.");
      Assert (New_Client.Sync_With_Room, "Not Synced with the room.");

      Free_Client (New_Client);
   end Test_Display_Player;

   -------------------------------------------------------------------------------------------------
   -- Test_Sync_With_Room
   -------------------------------------------------------------------------------------------------
   procedure Test_Sync_With_Room (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use type Song.T_Song;
      use type Song.Item.List.T_Item_List;

      New_Client : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);

      Room_Song : constant Song.T_Song :=
        Song.Initialize
          (Id             => "test id",
           Title          => "",
           Thumbnail_Link => "",
           Provider       => Api.No_Provider_Api);
      Room_Playlist : Song.Item.List.T_Item_List := Song.Item.List.Initialize;
   begin
      Room_Playlist.Append
        (Song.Item.Initialize
           (Id        => 1,
            Item_Song => Room_Song,
            Client_Id => Aws.Session.No_Session));

      Sync_With_Room (New_Client.all, True, Room_Song, Room_Playlist);

      Assert (New_Client.Sync_With_Room, "Not synced with the room.");
      Assert (New_Client.Current_Song = Song.Initialize, "Wrong current song.");
      Assert (New_Client.Playlist.Is_Empty, "Playlist not empty.");

      Sync_With_Room (New_Client.all, False, Room_Song, Room_Playlist);

      Assert (not New_Client.Sync_With_Room, "Synced with the room.");
      Assert (New_Client.Current_Song = Room_Song, "Wrong current song.");
      Assert (New_Client.Playlist.Get = Room_Playlist.Get, "Wrong playlist.");

      Sync_With_Room (New_Client.all, True, Song.Initialize, Song.Item.List.Initialize);

      Assert (New_Client.Sync_With_Room, "Not synced with the room.");
      Assert (New_Client.Current_Song = Room_Song, "Wrong current song.");
      Assert (New_Client.Playlist.Get = Room_Playlist.Get, "Wrong playlist.");

      Free_Client (New_Client);
   end Test_Sync_With_Room;

   -------------------------------------------------------------------------------------------------
   -- Test_Update_Last_Request_Time
   -------------------------------------------------------------------------------------------------
   procedure Test_Update_Last_Request_Time (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use type Ada.Real_Time.Time;

      New_Client : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);

      Time_Before_Update : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Time_After_Update  : Ada.Real_Time.Time;
   begin
      Assert (Time_Before_Update > New_Client.Last_Request_Time, "Wrong last request time.");

      New_Client.Update_Last_Request_Time;

      Time_After_Update := Ada.Real_Time.Clock;
      Assert (Time_Before_Update < New_Client.Last_Request_Time, "Last request time too small.");
      Assert (Time_After_Update > New_Client.Last_Request_Time, "Last request time too big.");

      Free_Client (New_Client);
   end Test_Update_Last_Request_Time;

   -------------------------------------------------------------------------------------------------
   -- Test_Next_Song
   -------------------------------------------------------------------------------------------------
   procedure Test_Next_Song (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use type Song.T_Song;
      use type Song.Item.List.T_Item_List;

      New_Client : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);

      Client_Song : constant Song.T_Song :=
        Song.Initialize
          (Id             => "test id",
           Title          => "",
           Thumbnail_Link => "",
           Provider       => Api.No_Provider_Api);
   begin
      New_Client.Current_Song := Client_Song;

      New_Client.Next_Song;

      Assert (New_Client.Current_Song = Client_Song, "Wrong current song.");
      Assert (New_Client.Playlist.Is_Empty, "Playlist not empty.");

      New_Client.Sync_With_Room := False;
      New_Client.Next_Song;

      Assert (New_Client.Current_Song = Song.Initialize, "Wrong current song.");
      Assert (New_Client.Playlist.Is_Empty, "Playlist not empty.");

      New_Client.Playlist.Append
        (Song.Item.Initialize
           (Id        => 1,
            Item_Song => Client_Song,
            Client_Id => Aws.Session.No_Session));
      New_Client.Next_Song;

      Assert (New_Client.Current_Song = Client_Song, "Wrong current song.");
      Assert (New_Client.Playlist.Is_Empty, "Playlist not empty.");

      Free_Client (New_Client);
   end Test_Next_Song;

   -------------------------------------------------------------------------------------------------
   -- Test_Add_Item_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_Item_To_Playlist (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use type Song.Item.T_Item;

      New_Client : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);

      New_Item : constant Song.Item.T_Item :=
        (Song.Item.Initialize
           (Id        => 1,
            Item_Song =>
              Song.Initialize
                (Id             => "test id",
                 Title          => "",
                 Thumbnail_Link => "",
                 Provider       => Api.No_Provider_Api),
            Client_Id => Aws.Session.No_Session));
   begin
      New_Client.Add_Item_To_Playlist (New_Item);

      Assert (New_Client.Playlist.Is_Empty, "Playlist not empty.");

      New_Client.Sync_With_Room := False;
      New_Client.Add_Item_To_Playlist (New_Item);

      Assert (Natural (New_Client.Playlist.Get.Length) = 1, "Wrong playlist length.");
      Assert (New_Client.Playlist.First_Element = New_Item, "Wrong playlist first element.");

      Free_Client (New_Client);
   end Test_Add_Item_To_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Test_Remove_Item_From_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Test_Remove_Item_From_Playlist (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use type Song.Item.T_Item;

      New_Client : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);

      New_Item : constant Song.Item.T_Item :=
        (Song.Item.Initialize
           (Id        => 1,
            Item_Song =>
              Song.Initialize
                (Id             => "test id",
                 Title          => "",
                 Thumbnail_Link => "",
                 Provider       => Api.No_Provider_Api),
            Client_Id => Aws.Session.No_Session));
   begin
      New_Client.Playlist.Append (New_Item);
      Assert (Natural (New_Client.Playlist.Get.Length) = 1, "Wrong playlist length.");
      Assert (New_Client.Playlist.First_Element = New_Item, "Wrong playlist first element.");

      New_Client.Remove_Item_From_Playlist (1);

      Assert (Natural (New_Client.Playlist.Get.Length) = 1, "Wrong playlist length.");
      Assert (New_Client.Playlist.First_Element = New_Item, "Wrong playlist first element.");

      New_Client.Sync_With_Room := False;
      New_Client.Remove_Item_From_Playlist (1);

      Assert (New_Client.Playlist.Is_Empty, "Playlist not empty.");

      Free_Client (New_Client);
   end Test_Remove_Item_From_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Test_Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Test_Up_Vote_Playlist_Item (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use type Song.Item.T_Item;

      New_Client : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);

      New_Item : constant Song.Item.T_Item :=
        (Song.Item.Initialize
           (Id        => 1,
            Item_Song =>
              Song.Initialize
                (Id             => "test id",
                 Title          => "",
                 Thumbnail_Link => "",
                 Provider       => Api.No_Provider_Api),
            Client_Id => Aws.Session.No_Session));
   begin
      New_Client.Playlist.Append (New_Item);
      Assert (New_Client.Playlist.First_Element.Get_Up_Votes = 0, "Wrong playlist item up votes.");

      New_Client.Up_Vote_Playlist_Item (1);

      Assert (New_Client.Playlist.First_Element.Get_Up_Votes = 0, "Wrong playlist item up votes.");

      New_Client.Sync_With_Room := False;
      New_Client.Up_Vote_Playlist_Item (1);

      Assert (New_Client.Playlist.First_Element.Get_Up_Votes = 1, "Wrong playlist item up votes.");

      Free_Client (New_Client);
   end Test_Up_Vote_Playlist_Item;

   -------------------------------------------------------------------------------------------------
   -- Test_Getter
   -------------------------------------------------------------------------------------------------
   procedure Test_Getter (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use type Aws.Session.Id;
      use type Song.T_Song;
      use type Song.Item.List.T_Item_List;
      use type Ada.Real_Time.Time;

      Current_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

      New_Client : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
   begin
      Assert (New_Client.Get_Id = Aws.Session.No_Session, "Wrong client ID.");
      Assert (New_Client.Get_Current_Song = Song.Initialize, "Wrong current song.");
      Assert (New_Client.Get_Playlist.Is_Empty, "Playlist not empty.");
      Assert (New_Client.Is_Player_Displayed, "Player not displayed.");
      Assert (New_Client.Is_Sync_With_Room, "Not synced with the room.");
      Assert
        (Ada.Real_Time.To_Duration (Current_Time - New_Client.Get_Last_Request_Time) < 0.001,
         "Wrong last request time.");

      Free_Client (New_Client);
   end Test_Getter;

end Client.Test;
