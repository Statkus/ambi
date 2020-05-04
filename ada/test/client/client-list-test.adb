with Ada.Unchecked_Deallocation;

with Aunit.Assertions; use Aunit.Assertions;

with Api;

package body Client.List.Test is

   -------------------------------------------------------------------------------------------------
   -- Free_Client
   -------------------------------------------------------------------------------------------------
   procedure Free_Client is new Ada.Unchecked_Deallocation (T_Client, T_Client_Access);

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Client_List_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine (This, Test_Initialize'Access, "Test Initialize");
      Register_Routine (This, Test_Append'Access, "Test Append");
      Register_Routine
        (This,
         Test_Remove_Disconnected_Client'Access,
         "Test Remove_Disconnected_Client");
      Register_Routine (This, Test_Add_Item_To_Playlist'Access, "Test Add_Item_To_Playlist");
      Register_Routine
        (This,
         Test_Remove_Item_From_Playlist'Access,
         "Test Remove_Item_From_Playlist");
      Register_Routine (This, Test_Up_Vote_Playlist_Item'Access, "Test Up_Vote_Playlist_Item");
      Register_Routine (This, Test_Length'Access, "Test Length");
      Register_Routine (This, Test_Get_Client'Access, "Test Get_Client");
      Register_Routine
        (This,
         Test_Is_Auto_Playback_Requested'Access,
         "Test Is_Auto_Playback_Requested");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Client_List_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Client.List tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Client_List : constant T_Client_List := Initialize;
   begin
      Assert (Client_List.Length = 0, "Client list not empty.");
   end Test_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Append
   -------------------------------------------------------------------------------------------------
   procedure Test_Append (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Client_List : T_Client_List;
      New_Client  : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
   begin
      Client_List.Append (New_Client);
      Assert (Client_List.Length = 1, "Wrong number of clients.");

      Free_Client (New_Client);
   end Test_Append;

   -------------------------------------------------------------------------------------------------
   -- Test_Remove_Disconnected_Client
   -------------------------------------------------------------------------------------------------
   procedure Test_Remove_Disconnected_Client (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use Ada.Real_Time;

      Client_List : T_Client_List;

      Client_1 : T_Client_Access          := New_And_Initialize (Session_Id => Aws.Session.Create);
      Client_2 : constant T_Client_Access :=
        New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_3 : constant T_Client_Access :=
        New_And_Initialize (Session_Id => Aws.Session.No_Session);
   begin
      Client_List.Remove_Disconnected_Client (Time_Last);
      Assert (Client_List.Length = 0, "Wrong number of clients.");

      Client_1.Last_Request_Time := Time_First;
      Client_List.Append (Client_1);
      Assert (Client_List.Length = 1, "Wrong number of clients.");

      Client_List.Remove_Disconnected_Client (Time_First + To_Time_Span (60.0));

      Assert (Client_List.Length = 1, "Wrong number of clients.");

      Client_List.Remove_Disconnected_Client (Time_First + To_Time_Span (Duration'Succ (60.0)));

      Assert (Client_List.Length = 0, "Wrong number of clients.");

      Client_1                   := New_And_Initialize (Session_Id => Aws.Session.Create);
      Client_1.Last_Request_Time := Time_Last;
      Client_List.Append (Client_1);
      Assert (Client_List.Length = 1, "Wrong number of clients.");

      Client_List.Remove_Disconnected_Client (Time_First);

      Assert (Client_List.Length = 1, "Wrong number of clients.");

      Client_1.Last_Request_Time := Time_First;
      Client_1.Id                := Aws.Session.No_Session;

      Client_List.Remove_Disconnected_Client (Time_First);

      Assert (Client_List.Length = 0, "Wrong number of clients.");

      Client_1 := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_List.Append (Client_1);
      Client_List.Append (Client_2);
      Client_List.Append (Client_3);
      Assert (Client_List.Length = 3, "Wrong number of clients.");

      Client_List.Remove_Disconnected_Client (Time_First);

      Assert (Client_List.Length = 0, "Wrong number of clients.");
   end Test_Remove_Disconnected_Client;

   -------------------------------------------------------------------------------------------------
   -- Test_Add_Item_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_Item_To_Playlist (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Client_List : T_Client_List;

      Client_1 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_2 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_3 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);

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
      Sync_With_Room (Client_1.all, False, Song.Initialize, Song.Item.List.Initialize);
      Sync_With_Room (Client_2.all, False, Song.Initialize, Song.Item.List.Initialize);
      Client_List.Append (Client_1);
      Client_List.Append (Client_2);
      Client_List.Append (Client_3);
      Assert (Client_List.Length = 3, "Wrong number of clients.");

      Client_List.Add_Item_To_Playlist (New_Item);

      Assert (Natural (Client_1.Get_Playlist.Get.Length) = 1, "Wrong playlist size.");
      Assert (Natural (Client_2.Get_Playlist.Get.Length) = 1, "Wrong playlist size.");
      Assert (Natural (Client_3.Get_Playlist.Get.Length) = 0, "Wrong playlist size.");

      Free_Client (Client_1);
      Free_Client (Client_2);
      Free_Client (Client_3);
   end Test_Add_Item_To_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Test_Remove_Item_From_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Test_Remove_Item_From_Playlist (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Client_List : T_Client_List;

      Client_1 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_2 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_3 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);

      Item_1 : constant Song.Item.T_Item :=
        (Song.Item.Initialize
           (Id        => 1,
            Item_Song =>
              Song.Initialize
                (Id             => "test id",
                 Title          => "",
                 Thumbnail_Link => "",
                 Provider       => Api.No_Provider_Api),
            Client_Id => Aws.Session.No_Session));
      Item_2 : constant Song.Item.T_Item :=
        (Song.Item.Initialize
           (Id        => 2,
            Item_Song =>
              Song.Initialize
                (Id             => "test id",
                 Title          => "",
                 Thumbnail_Link => "",
                 Provider       => Api.No_Provider_Api),
            Client_Id => Aws.Session.No_Session));
   begin
      Sync_With_Room (Client_1.all, False, Song.Initialize, Song.Item.List.Initialize);
      Sync_With_Room (Client_2.all, False, Song.Initialize, Song.Item.List.Initialize);
      Sync_With_Room (Client_3.all, False, Song.Initialize, Song.Item.List.Initialize);
      Client_1.Add_Item_To_Playlist (Item_1);
      Client_2.Add_Item_To_Playlist (Item_1);
      Client_3.Add_Item_To_Playlist (Item_2);
      Client_List.Append (Client_1);
      Client_List.Append (Client_2);
      Client_List.Append (Client_3);
      Assert (Client_List.Length = 3, "Wrong number of clients.");

      Client_List.Remove_Item_From_Playlist (1);

      Assert (Natural (Client_1.Get_Playlist.Get.Length) = 0, "Wrong playlist size.");
      Assert (Natural (Client_2.Get_Playlist.Get.Length) = 0, "Wrong playlist size.");
      Assert (Natural (Client_3.Get_Playlist.Get.Length) = 1, "Wrong playlist size.");

      Free_Client (Client_1);
      Free_Client (Client_2);
      Free_Client (Client_3);
   end Test_Remove_Item_From_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Test_Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Test_Up_Vote_Playlist_Item (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Client_List : T_Client_List;

      Client_1 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_2 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_3 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);

      Item_1 : constant Song.Item.T_Item :=
        (Song.Item.Initialize
           (Id        => 1,
            Item_Song =>
              Song.Initialize
                (Id             => "test id",
                 Title          => "",
                 Thumbnail_Link => "",
                 Provider       => Api.No_Provider_Api),
            Client_Id => Aws.Session.No_Session));
      Item_2 : constant Song.Item.T_Item :=
        (Song.Item.Initialize
           (Id        => 2,
            Item_Song =>
              Song.Initialize
                (Id             => "test id",
                 Title          => "",
                 Thumbnail_Link => "",
                 Provider       => Api.No_Provider_Api),
            Client_Id => Aws.Session.No_Session));
   begin
      Sync_With_Room (Client_1.all, False, Song.Initialize, Song.Item.List.Initialize);
      Sync_With_Room (Client_2.all, False, Song.Initialize, Song.Item.List.Initialize);
      Sync_With_Room (Client_3.all, False, Song.Initialize, Song.Item.List.Initialize);
      Client_1.Add_Item_To_Playlist (Item_1);
      Client_2.Add_Item_To_Playlist (Item_1);
      Client_3.Add_Item_To_Playlist (Item_2);
      Client_List.Append (Client_1);
      Client_List.Append (Client_2);
      Client_List.Append (Client_3);
      Assert (Client_List.Length = 3, "Wrong number of clients.");

      Client_List.Up_Vote_Playlist_Item (1);

      Assert
        (Client_1.Get_Playlist.First_Element.Get_Up_Votes = 1,
         "Wrong playlist first element up votes.");
      Assert
        (Client_2.Get_Playlist.First_Element.Get_Up_Votes = 1,
         "Wrong playlist first element up votes.");
      Assert
        (Client_3.Get_Playlist.First_Element.Get_Up_Votes = 0,
         "Wrong playlist first element up votes.");

      Free_Client (Client_1);
      Free_Client (Client_2);
      Free_Client (Client_3);
   end Test_Up_Vote_Playlist_Item;

   -------------------------------------------------------------------------------------------------
   -- Test_Length
   -------------------------------------------------------------------------------------------------
   procedure Test_Length (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Client_List : T_Client_List;

      Client_1 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_2 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_3 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
   begin
      Client_List.Append (Client_1);
      Assert (Client_List.Length = 1, "Wrong number of clients.");

      Client_List.Append (Client_2);
      Assert (Client_List.Length = 2, "Wrong number of clients.");

      Client_List.Append (Client_3);
      Assert (Client_List.Length = 3, "Wrong number of clients.");

      Free_Client (Client_1);
      Free_Client (Client_2);
      Free_Client (Client_3);
   end Test_Length;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Client
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Client (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Client_List : T_Client_List;

      Client_1         : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.Create);
      Client_2         : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.Create);
      Client_3         : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.Create);
      Client_Requested : T_Client_Access := null;
   begin
      Client_List.Append (Client_1);
      Client_List.Append (Client_2);
      Client_List.Append (Client_3);
      Assert (Client_List.Length = 3, "Wrong number of clients.");

      Client_Requested := Client_List.Get_Client (Aws.Session.No_Session);
      Assert (Client_Requested = null, "Not null Client returned.");

      Client_Requested := Client_List.Get_Client (Client_1.Get_Id);
      Assert (Client_Requested = Client_1, "Wrong client returned.");

      Client_Requested := Client_List.Get_Client (Client_2.Get_Id);
      Assert (Client_Requested = Client_2, "Wrong client returned.");

      Client_Requested := Client_List.Get_Client (Client_3.Get_Id);
      Assert (Client_Requested = Client_3, "Wrong client returned.");

      Free_Client (Client_1);
      Free_Client (Client_2);
      Free_Client (Client_3);
   end Test_Get_Client;

   -------------------------------------------------------------------------------------------------
   -- Test_Is_Auto_Playback_Requested
   -------------------------------------------------------------------------------------------------
   procedure Test_Is_Auto_Playback_Requested (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Client_List : T_Client_List;

      Client_1 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_2 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
      Client_3 : T_Client_Access := New_And_Initialize (Session_Id => Aws.Session.No_Session);
   begin
      Client_List.Append (Client_1);
      Client_List.Append (Client_2);
      Client_List.Append (Client_3);
      Assert (Client_List.Length = 3, "Wrong number of clients.");

      Assert (Client_List.Is_Auto_Playback_Requested, "Auto playback not requested.");

      Display_Player (Client_1.all, False);
      Display_Player (Client_2.all, False);
      Display_Player (Client_3.all, False);

      Assert (not Client_List.Is_Auto_Playback_Requested, "Auto playback requested.");

      Sync_With_Room (Client_1.all, False, Song.Initialize, Song.Item.List.Initialize);
      Sync_With_Room (Client_2.all, False, Song.Initialize, Song.Item.List.Initialize);
      Sync_With_Room (Client_3.all, False, Song.Initialize, Song.Item.List.Initialize);

      Assert (not Client_List.Is_Auto_Playback_Requested, "Auto playback requested.");

      Display_Player (Client_1.all, False);
      Display_Player (Client_3.all, False);

      Assert (not Client_List.Is_Auto_Playback_Requested, "Auto playback requested.");

      Sync_With_Room (Client_1.all, True, Song.Initialize, Song.Item.List.Initialize);

      Assert (Client_List.Is_Auto_Playback_Requested, "Auto playback not requested.");

      Sync_With_Room (Client_2.all, True, Song.Initialize, Song.Item.List.Initialize);

      Assert (Client_List.Is_Auto_Playback_Requested, "Auto playback not requested.");

      Sync_With_Room (Client_3.all, True, Song.Initialize, Song.Item.List.Initialize);

      Assert (Client_List.Is_Auto_Playback_Requested, "Auto playback not requested.");

      Free_Client (Client_1);
      Free_Client (Client_2);
      Free_Client (Client_3);
   end Test_Is_Auto_Playback_Requested;

end Client.List.Test;
