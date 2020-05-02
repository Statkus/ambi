with Ada.Unchecked_Deallocation;

with Aunit.Assertions; use Aunit.Assertions;

package body Room.Test is

   use Web_Methods.Websocket;

   -------------------------------------------------------------------------------------------------
   -- Free_Room
   -------------------------------------------------------------------------------------------------
   procedure Free_Room is new Ada.Unchecked_Deallocation (T_Room, T_Room_Access);

   -------------------------------------------------------------------------------------------------
   -- Free_Database
   -------------------------------------------------------------------------------------------------
   procedure Free_Database is new Ada.Unchecked_Deallocation
     (Database.Mock.T_Database_Mock,
      Database.Mock.T_Database_Mock_Access);

   -------------------------------------------------------------------------------------------------
   -- Free_Dispatcher
   -------------------------------------------------------------------------------------------------
   procedure Free_Dispatcher is new Ada.Unchecked_Deallocation
     (Api.Dispatcher.T_Dispatcher,
      Api.Dispatcher.T_Dispatcher_Access);

   -------------------------------------------------------------------------------------------------
   -- Free_Websocket
   -------------------------------------------------------------------------------------------------
   procedure Free_Websocket is new Ada.Unchecked_Deallocation
     (Web_Methods.Websocket.Mock.T_Websocket_Mock,
      Web_Methods.Websocket.Mock.T_Websocket_Mock_Access);

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Room_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine (This, Test_New_And_Initialize'Access, "Test New_And_Initialize");
      Register_Routine (This, Test_Equality_Operator'Access, "Test equality operator");
      Register_Routine (This, Test_Add_Client'Access, "Test Add_Client");
      Register_Routine (This, Test_Add_Song_To_Playlist'Access, "Test Add_Song_To_Playlist");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Room_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Room tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Set_Up
   -------------------------------------------------------------------------------------------------
   procedure Set_Up (This : in out T_Room_Test_Case) is
   begin
      This.Db         := Database.Mock.New_And_Initialize;
      This.Dispatcher := Api.Dispatcher.New_And_Initialize;
      This.Websocket  := Web_Methods.Websocket.Mock.New_And_Initialize;
   end Set_Up;

   -------------------------------------------------------------------------------------------------
   -- Tear_Down
   -------------------------------------------------------------------------------------------------
   procedure Tear_Down (This : in out T_Room_Test_Case) is
   begin
      Free_Database (This.Db);
      Free_Dispatcher (This.Dispatcher);
      Free_Websocket (This.Websocket);
   end Tear_Down;

   -------------------------------------------------------------------------------------------------
   -- Test_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_New_And_Initialize (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      use type Ada.Real_Time.Time;
      use type Api.Dispatcher.T_Dispatcher_Access;
      use type Database.T_Database_Class_Access;
      use type Song.T_Song;
      use type Song.Item.T_Item_Id;

      This : constant T_Room_Test_Case :=
        T_Room_Test_Case'
          (Aunit.Test_Cases.Test_Case with
           Db         => T_Room_Test_Case (Test_Case).Db,
           Dispatcher => T_Room_Test_Case (Test_Case).Dispatcher,
           Websocket  => T_Room_Test_Case (Test_Case).Websocket);

      Current_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

      New_Room : T_Room_Access :=
        New_And_Initialize
          ("test",
           Database.T_Database_Class_Access (This.Db),
           This.Dispatcher,
           Web_Methods.Websocket.T_Websocket_Class_Access (This.Websocket));
   begin
      Assert (New_Room.Name = "test", "Wrong name.");
      Assert (New_Room.Db = Database.T_Database_Class_Access (This.Db), "Wrong database.");
      Assert (New_Room.Api_Dispatcher = This.Dispatcher, "Wrong API dispatcher.");
      Assert
        (New_Room.Websocket = Web_Methods.Websocket.T_Websocket_Class_Access (This.Websocket),
         "Wrong websocket.");
      Assert (New_Room.Client_List.Length = 0, "Wrong client list length.");
      Assert (New_Room.Current_Song = Song.Initialize, "Wrong current song.");
      Assert (New_Room.Playlist.Is_Empty, "Playlist not empty.");
      Assert (New_Room.Current_Item_Id = Song.Item.T_Item_Id'First, "Wrong current item ID.");
      Assert (New_Room.Sync_Task /= null, "Null sync task.");
      Assert (not New_Room.Next_Song_Ready, "Next song ready.");
      Assert
        (Ada.Real_Time.To_Duration (Current_Time - New_Room.Last_Request_Time) < 0.001,
         "Wrong last request time.");
      Assert (not New_Room.Block_Websocket, "Websocket blocked.");

      Free_Room (New_Room);
   end Test_New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Equality_Operator
   -------------------------------------------------------------------------------------------------
   procedure Test_Equality_Operator (Test_Case : in out Test_Cases.Test_Case'Class) is
      This : constant T_Room_Test_Case :=
        T_Room_Test_Case'
          (Aunit.Test_Cases.Test_Case with
           Db         => T_Room_Test_Case (Test_Case).Db,
           Dispatcher => T_Room_Test_Case (Test_Case).Dispatcher,
           Websocket  => T_Room_Test_Case (Test_Case).Websocket);

      Room_1 : T_Room_Access :=
        New_And_Initialize
          ("test",
           Database.T_Database_Class_Access (This.Db),
           This.Dispatcher,
           Web_Methods.Websocket.T_Websocket_Class_Access (This.Websocket));
      Room_2 : T_Room_Access :=
        New_And_Initialize
          ("test",
           Database.T_Database_Class_Access (This.Db),
           This.Dispatcher,
           Web_Methods.Websocket.T_Websocket_Class_Access (This.Websocket));
   begin
      Assert (Room_1.all = Room_2.all, "Wrong equality operator.");

      Free_Room (Room_2);
      Room_2 :=
        New_And_Initialize
          ("test2",
           Database.T_Database_Class_Access (This.Db),
           This.Dispatcher,
           Web_Methods.Websocket.T_Websocket_Class_Access (This.Websocket));

      Assert (Room_1.all /= Room_2.all, "Wrong equality operator.");

      Free_Room (Room_1);
      Free_Room (Room_2);
   end Test_Equality_Operator;

   -------------------------------------------------------------------------------------------------
   -- Test_Add_Client
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_Client (Test_Case : in out Test_Cases.Test_Case'Class) is
      use type Ada.Real_Time.Time;
      use type Client.T_Client_Access;

      This : constant T_Room_Test_Case :=
        T_Room_Test_Case'
          (Aunit.Test_Cases.Test_Case with
           Db         => T_Room_Test_Case (Test_Case).Db,
           Dispatcher => T_Room_Test_Case (Test_Case).Dispatcher,
           Websocket  => T_Room_Test_Case (Test_Case).Websocket);

      New_Room : T_Room_Access :=
        New_And_Initialize
          ("test",
           Database.T_Database_Class_Access (This.Db),
           This.Dispatcher,
           Web_Methods.Websocket.T_Websocket_Class_Access (This.Websocket));

      Current_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      Assert (New_Room.Client_List.Length = 0, "Wrong client list length.");

      Current_Time := Ada.Real_Time.Clock;
      New_Room.Add_Client (Aws.Session.No_Session);

      Assert (New_Room.Client_List.Length = 1, "Wrong client list length.");
      Assert
        (New_Room.Client_List.Get_Client (Aws.Session.No_Session) /= null,
         "New client does not exist.");
      Assert
        (This.Websocket.Is_Room_Request_Called (Update_Nb_Clients),
         "Update_Nb_Clients not called.");
      Assert (New_Room.Last_Request_Time > Current_Time, "Wrong last request time.");

      New_Room.Remove_Disconnected_Client;
      Assert (New_Room.Client_List.Length = 0, "Wrong client list length.");
      Free_Room (New_Room);
   end Test_Add_Client;

   -------------------------------------------------------------------------------------------------
   -- Test_Add_Song_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_Song_To_Playlist (Test_Case : in out Test_Cases.Test_Case'Class) is
      use type Aws.Session.Id;
      use type Song.T_Song;
      use type Song.Item.T_Item_Id;

      This : constant T_Room_Test_Case :=
        T_Room_Test_Case'
          (Aunit.Test_Cases.Test_Case with
           Db         => T_Room_Test_Case (Test_Case).Db,
           Dispatcher => T_Room_Test_Case (Test_Case).Dispatcher,
           Websocket  => T_Room_Test_Case (Test_Case).Websocket);

      New_Room : T_Room_Access :=
        New_And_Initialize
          ("test",
           Database.T_Database_Class_Access (This.Db),
           This.Dispatcher,
           Web_Methods.Websocket.T_Websocket_Class_Access (This.Websocket));

      New_Song : constant Song.T_Song :=
        Song.Initialize (Id => "", Title => "", Thumbnail_Link => "", Provider => Api.Youtube_Api);
   begin
      New_Room.Add_Client (Aws.Session.No_Session);
      New_Room.Get_Client (Aws.Session.No_Session).Sync_With_Room
      (False, Song.Initialize, Song.Item.List.Initialize);
      Assert (New_Room.Client_List.Length = 1, "Wrong client list length.");

      New_Room.Add_Song_To_Playlist (Aws.Session.No_Session, Song.Initialize);

      Assert (New_Room.Current_Song = Song.Initialize, "Wrong current song.");
      Assert (New_Room.Playlist.Is_Empty, "Playlist not empty.");
      Assert (This.Db.Get_History.Is_Empty, "History not empty.");
      Assert
        (New_Room.Get_Client (Aws.Session.No_Session).Get_Playlist.Is_Empty,
         "Client playlist not empty.");
      Assert (New_Room.Current_Item_Id = 0, "Wrong current item ID.");
      Assert
        (not This.Websocket.Is_Room_Request_Called (Update_Playlist),
         "Update_Playlist called.");

      New_Room.Add_Song_To_Playlist (Aws.Session.No_Session, New_Song);

      Assert (New_Room.Current_Song = New_Song, "Wrong current song.");
      Assert (New_Room.Playlist.Is_Empty, "Playlist not empty.");
      Assert (This.Db.Get_History.First_Element = New_Song, "Wrong first element in history.");
      Assert
        (New_Room.Get_Client (Aws.Session.No_Session).Get_Playlist.First_Element.Get_Id = 0,
         "Wrong client first playlist element ID.");
      Assert
        (New_Room.Get_Client (Aws.Session.No_Session).Get_Playlist.First_Element.Get_Song =
         New_Song,
         "Wrong client first playlist element song.");
      Assert
        (New_Room.Get_Client (Aws.Session.No_Session).Get_Playlist.First_Element.Get_Client_Id =
         Aws.Session.No_Session,
         "Wrong client first playlist element client ID.");
      Assert
        (New_Room.Get_Client (Aws.Session.No_Session).Get_Playlist.First_Element.Get_Up_Votes = 1,
         "Wrong client first playlist element up votes.");
      Assert (New_Room.Current_Item_Id = 1, "Wrong current item ID.");
      Assert
        (This.Websocket.Is_Room_Request_Called (Update_Playlist),
         "Update_Playlist not called.");

      This.Websocket.Reset_Room_Request_Called;
      New_Room.Block_Websocket := True;
      New_Room.Add_Song_To_Playlist (Aws.Session.No_Session, New_Song);

      Assert
        (not This.Websocket.Is_Room_Request_Called (Update_Playlist),
         "Update_Playlist called.");

      New_Room.Remove_Disconnected_Client;
      Assert (New_Room.Client_List.Length = 0, "Wrong client list length.");
      Free_Room (New_Room);
   end Test_Add_Song_To_Playlist;

end Room.Test;
