with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aunit.Assertions; use Aunit.Assertions;

with Database_Wrapper;

with Api;

package body Database.Test is

   use Song_Vector;

   use type Api.T_Api_Provider;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Database_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine
        (This,
         Test_Constructors_New_And_Initialize'Access,
         "Test New_And_Initialize constructor");

      Register_Routine (This, Test_Close'Access, "Test Close");

      Register_Routine (This, Test_Add_To_Rooms'Access, "Test Add_To_Rooms");

      Register_Routine (This, Test_Add_To_Room_Historic'Access, "Test Add_To_Room_Historic");

      Register_Routine (This, Test_Add_To_Room_Likes'Access, "Test Add_To_Room_Likes");

      Register_Routine (This, Test_Remove_From_Room_Likes'Access, "Test Remove_From_Room_Likes");

      Register_Routine (This, Test_Get_Rooms'Access, "Test Get_Rooms");

      Register_Routine (This, Test_Get_Room_Historic'Access, "Test Get_Room_Historic");

      Register_Routine (This, Test_Get_Room_Last_Songs'Access, "Test Get_Room_Last_Songs");

      Register_Routine (This, Test_Get_Room_Likes'Access, "Test Get_Room_Likes");

      Register_Routine (This, Test_Is_Room_Song_Liked'Access, "Test Is_Room_Song_Liked");

      Register_Routine (This, Test_Read_Rooms_In_Db'Access, "Test Read_Rooms_In_Db");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Database_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Database tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_Constructors_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Constructors_New_And_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Test_Db : constant T_Database_Access :=
        Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");

      Db_Wrapper : constant Database_Wrapper.T_Database_Wrapper_Access :=
        Database_Wrapper.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
   begin
      Assert (Test_Db /= null, "Null database returned.");
      Test_Db.Close;

      -- Check rooms table
      Assert (Db_Wrapper.Is_Table_Exist ("rooms"), "No table rooms created.");
      Assert
        (Db_Wrapper.Is_Column_Of_Table_Exist ("rooms", "room_name"),
         "No column room_name in table rooms.");

      -- Check historic table
      Assert (Db_Wrapper.Is_Table_Exist ("historic"), "No table historic created.");
      Assert
        (Db_Wrapper.Is_Column_Of_Table_Exist ("historic", "id"),
         "No column id in table historic.");
      Assert
        (Db_Wrapper.Is_Column_Of_Table_Exist ("historic", "song_id"),
         "No column song_id in table historic.");
      Assert
        (Db_Wrapper.Is_Column_Of_Table_Exist ("historic", "song_title"),
         "No column song_title in table historic.");
      Assert
        (Db_Wrapper.Is_Column_Of_Table_Exist ("historic", "song_thumbnail_link"),
         "No column song_thumbnail_link in table historic.");
      Assert
        (Db_Wrapper.Is_Column_Of_Table_Exist ("historic", "song_provider"),
         "No column song_provider in table historic.");

      -- Check likes table
      Assert (Db_Wrapper.Is_Table_Exist ("likes"), "No table likes created.");
      Assert
        (Db_Wrapper.Is_Column_Of_Table_Exist ("likes", "song_id"),
         "No column song_id in table likes.");
      Assert
        (Db_Wrapper.Is_Column_Of_Table_Exist ("likes", "song_title"),
         "No column song_title in table likes.");
      Assert
        (Db_Wrapper.Is_Column_Of_Table_Exist ("likes", "song_thumbnail_link"),
         "No column song_thumbnail_link in table likes.");
      Assert
        (Db_Wrapper.Is_Column_Of_Table_Exist ("likes", "song_provider"),
         "No column song_provider in table likes.");

      Db_Wrapper.Close;
   end Test_Constructors_New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Close
   -------------------------------------------------------------------------------------------------
   procedure Test_Close (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Test_Db : constant T_Database_Access :=
        Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
   begin
      Assert (Test_Db.Open, "Database not open.");

      Test_Db.Close;

      Assert (not Test_Db.Open, "Database still open.");
   end Test_Close;

   -------------------------------------------------------------------------------------------------
   -- Test_Add_To_Rooms
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_To_Rooms (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      use type Room_Name_Vector.T_Room_Name_Vector;

      pragma Unreferenced (Test_Case);

      Test_Db    : T_Database_Access;
      Db_Wrapper : Database_Wrapper.T_Database_Wrapper_Access;

      Rooms : Room_Name_Vector.T_Room_Name_Vector;
   begin
      Database_Wrapper.Delete ("obj/ambi_test.sqlite3");
      Test_Db    := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Db_Wrapper := Database_Wrapper.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");

      -- Add new rooms
      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Add_To_Rooms ("test_2");
      -- Try to add already existing room
      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Close;

      Rooms := Db_Wrapper.Get_Rooms;
      Assert (Natural (Rooms.Length) = 2, "Wrong number of rooms.");
      Assert (To_String (Rooms.Element (0)) = "test_1", "Wrong name for first room.");
      Assert (To_String (Rooms.Element (1)) = "test_2", "Wrong name for second room.");
      Assert (Rooms = Test_Db.Rooms, "Wrong rooms internally saved.");

      -- Try to add room while database is closed
      Test_Db.Add_To_Rooms ("test_3");

      Rooms := Db_Wrapper.Get_Rooms;
      Assert (Natural (Rooms.Length) = 2, "Wrong number of rooms.");
      Assert (To_String (Rooms.Element (0)) = "test_1", "Wrong name for first room.");
      Assert (To_String (Rooms.Element (1)) = "test_2", "Wrong name for second room.");
      Assert (Rooms = Test_Db.Rooms, "Wrong rooms internally saved.");

      Test_Db := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      -- Add room after reading previously saved rooms
      Test_Db.Add_To_Rooms ("test_4");
      Test_Db.Close;

      Rooms := Db_Wrapper.Get_Rooms;
      Assert (Natural (Rooms.Length) = 3, "Wrong number of rooms.");
      Assert (To_String (Rooms.Element (0)) = "test_1", "Wrong name for first room.");
      Assert (To_String (Rooms.Element (1)) = "test_2", "Wrong name for second room.");
      Assert (To_String (Rooms.Element (2)) = "test_4", "Wrong name for third room.");
      Assert (Rooms = Test_Db.Rooms, "Wrong rooms internally saved.");
   end Test_Add_To_Rooms;

   -------------------------------------------------------------------------------------------------
   -- Test_Add_To_Room_Historic
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_To_Room_Historic (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Test_Db    : T_Database_Access;
      Db_Wrapper : Database_Wrapper.T_Database_Wrapper_Access;

      Songs : Song_Vector.T_Song_Vector;
   begin
      Database_Wrapper.Delete ("obj/ambi_test.sqlite3");
      Test_Db    := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Db_Wrapper := Database_Wrapper.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Add_To_Rooms ("test_2");

      Test_Db.Close;
      -- Add song to room historic while database is closed
      Test_Db.Add_To_Room_Historic ("test_1", Song.Constructors.Initialize);

      Songs := Db_Wrapper.Get_Song_Table ("test_1", "historic");
      Assert (Songs.Is_Empty, "Wrong number of songs.");

      Test_Db := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      -- Add song to not existing room historic
      Test_Db.Add_To_Room_Historic ("test_3", Song.Constructors.Initialize);

      Songs := Db_Wrapper.Get_Song_Table ("test_3", "historic");
      Assert (Songs.Is_Empty, "Wrong number of songs.");

      -- Add songs to existing room historic
      Test_Db.Add_To_Room_Historic ("test_1", Song.Constructors.Initialize);
      Test_Db.Add_To_Room_Historic
      ("test_1", Song.Constructors.Initialize
         (Id             => "M0e_0P9OZuM",
          Title          => "test' 1",
          Thumbnail_Link => "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
          Provider       => Api.Youtube_Api));

      Songs := Db_Wrapper.Get_Song_Table ("test_1", "historic");
      Assert (Natural (Songs.Length) = 2, "Wrong number of songs.");
      -- Check first song
      Assert (Song_Vectors.Element (Songs.First).Get_Id = "", "Wrong first song ID.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Title = "no song played",
         "Wrong first song title.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Thumbnail_Link = "",
         "Wrong first song thumbnail link.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Provider = Api.No_Provider_Api,
         "Wrong first song provider.");
      -- Check last song
      Assert (Song_Vectors.Element (Songs.Last).Get_Id = "M0e_0P9OZuM", "Wrong last song ID.");
      Assert (Song_Vectors.Element (Songs.Last).Get_Title = "test' 1", "Wrong last song title.");
      Assert
        (Song_Vectors.Element (Songs.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
         "Wrong last song thumbnail link.");
      Assert
        (Song_Vectors.Element (Songs.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last song provider.");

      -- Add song to other existing room historic
      Test_Db.Add_To_Room_Historic
      ("test_2", Song.Constructors.Initialize
         (Id             => "IGBxWcu6iXE",
          Title          => "'test 2",
          Thumbnail_Link => "https://i.ytimg.com/vi/IGBxWcu6iXE/default.jpg",
          Provider       => Api.Youtube_Api));

      -- Check that first room historic has not changed
      Assert
        (Songs = Db_Wrapper.Get_Song_Table ("test_1", "historic"),
         "First room historic modified by second room.");

      -- Check second room
      Songs := Db_Wrapper.Get_Song_Table ("test_2", "historic");
      Assert (Natural (Songs.Length) = 1, "Wrong number of songs.");
      -- Check song
      Assert (Song_Vectors.Element (Songs.First).Get_Id = "IGBxWcu6iXE", "Wrong song ID.");
      Assert (Song_Vectors.Element (Songs.First).Get_Title = "'test 2", "Wrong song title.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/IGBxWcu6iXE/default.jpg",
         "Wrong song thumbnail link.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Provider = Api.Youtube_Api,
         "Wrong song provider.");
   end Test_Add_To_Room_Historic;

   -------------------------------------------------------------------------------------------------
   -- Test_Add_To_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_To_Room_Likes (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Test_Db    : T_Database_Access;
      Db_Wrapper : Database_Wrapper.T_Database_Wrapper_Access;

      Songs : Song_Vector.T_Song_Vector;
   begin
      Database_Wrapper.Delete ("obj/ambi_test.sqlite3");
      Test_Db    := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Db_Wrapper := Database_Wrapper.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Add_To_Rooms ("test_2");

      Test_Db.Close;
      -- Add song to room likes while database is closed
      Test_Db.Add_To_Room_Likes ("test_1", Song.Constructors.Initialize);

      Songs := Db_Wrapper.Get_Song_Table ("test_1", "likes");
      Assert (Songs.Is_Empty, "Wrong number of songs.");

      Test_Db := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      -- Add song to not existing room likes
      Test_Db.Add_To_Room_Likes ("test_3", Song.Constructors.Initialize);

      Songs := Db_Wrapper.Get_Song_Table ("test_3", "likes");
      Assert (Songs.Is_Empty, "Wrong number of songs.");

      -- Add songs to existing room likes
      Test_Db.Add_To_Room_Likes ("test_1", Song.Constructors.Initialize);
      Test_Db.Add_To_Room_Likes
      ("test_1", Song.Constructors.Initialize
         (Id             => "M0e_0P9OZuM",
          Title          => "test' 1",
          Thumbnail_Link => "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
          Provider       => Api.Youtube_Api));

      Songs := Db_Wrapper.Get_Song_Table ("test_1", "likes");
      Assert (Natural (Songs.Length) = 2, "Wrong number of songs.");
      -- Check first song
      Assert (Song_Vectors.Element (Songs.First).Get_Id = "", "Wrong first song ID.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Title = "no song played",
         "Wrong first song title.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Thumbnail_Link = "",
         "Wrong first song thumbnail link.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Provider = Api.No_Provider_Api,
         "Wrong first song provider.");
      -- Check last song
      Assert (Song_Vectors.Element (Songs.Last).Get_Id = "M0e_0P9OZuM", "Wrong last song ID.");
      Assert (Song_Vectors.Element (Songs.Last).Get_Title = "test' 1", "Wrong last song title.");
      Assert
        (Song_Vectors.Element (Songs.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
         "Wrong last song thumbnail link.");
      Assert
        (Song_Vectors.Element (Songs.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last song provider.");

      -- Add already liked song to room likes
      Test_Db.Add_To_Room_Likes ("test_1", Song.Constructors.Initialize);
      Test_Db.Add_To_Room_Likes
      ("test_1", Song.Constructors.Initialize
         (Id             => "M0e_0P9OZuM",
          Title          => "test' 1",
          Thumbnail_Link => "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
          Provider       => Api.Youtube_Api));

      Assert (Songs = Db_Wrapper.Get_Song_Table ("test_1", "likes"), "First room likes modified.");

      -- Add song to other existing room likes
      Test_Db.Add_To_Room_Likes
      ("test_2", Song.Constructors.Initialize
         (Id             => "IGBxWcu6iXE",
          Title          => "'test 2",
          Thumbnail_Link => "https://i.ytimg.com/vi/IGBxWcu6iXE/default.jpg",
          Provider       => Api.Youtube_Api));

      -- Check that first room likes has not changed
      Assert
        (Songs = Db_Wrapper.Get_Song_Table ("test_1", "likes"),
         "First room likes modified by second room.");

      -- Check second room
      Songs := Db_Wrapper.Get_Song_Table ("test_2", "likes");
      Assert (Natural (Songs.Length) = 1, "Wrong number of songs.");
      -- Check song
      Assert (Song_Vectors.Element (Songs.First).Get_Id = "IGBxWcu6iXE", "Wrong song ID.");
      Assert (Song_Vectors.Element (Songs.First).Get_Title = "'test 2", "Wrong song title.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/IGBxWcu6iXE/default.jpg",
         "Wrong song thumbnail link.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Provider = Api.Youtube_Api,
         "Wrong song provider.");
   end Test_Add_To_Room_Likes;

   -------------------------------------------------------------------------------------------------
   -- Test_Remove_From_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Test_Remove_From_Room_Likes (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Test_Db    : T_Database_Access;
      Db_Wrapper : Database_Wrapper.T_Database_Wrapper_Access;

      Songs : Song_Vector.T_Song_Vector;
   begin
      Database_Wrapper.Delete ("obj/ambi_test.sqlite3");
      Test_Db    := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Db_Wrapper := Database_Wrapper.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Add_To_Rooms ("test_2");
      Test_Db.Add_To_Room_Likes ("test_1", Song.Constructors.Initialize);
      Test_Db.Add_To_Room_Likes
      ("test_1", Song.Constructors.Initialize
         (Id             => "M0e_0P9OZuM",
          Title          => "test' 1",
          Thumbnail_Link => "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
          Provider       => Api.Youtube_Api));
      Test_Db.Add_To_Room_Likes
      ("test_2", Song.Constructors.Initialize
         (Id             => "IGBxWcu6iXE",
          Title          => "'test 2",
          Thumbnail_Link => "https://i.ytimg.com/vi/IGBxWcu6iXE/default.jpg",
          Provider       => Api.Youtube_Api));

      -- Check first room likes
      Songs := Db_Wrapper.Get_Song_Table ("test_1", "likes");
      Assert (Natural (Songs.Length) = 2, "Wrong number of songs.");
      -- Check first song
      Assert (Song_Vectors.Element (Songs.First).Get_Id = "", "Wrong first song ID.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Title = "no song played",
         "Wrong first song title.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Thumbnail_Link = "",
         "Wrong first song thumbnail link.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Provider = Api.No_Provider_Api,
         "Wrong first song provider.");
      -- Check last song
      Assert (Song_Vectors.Element (Songs.Last).Get_Id = "M0e_0P9OZuM", "Wrong last song ID.");
      Assert (Song_Vectors.Element (Songs.Last).Get_Title = "test' 1", "Wrong last song title.");
      Assert
        (Song_Vectors.Element (Songs.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
         "Wrong last song thumbnail link.");
      Assert
        (Song_Vectors.Element (Songs.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last song provider.");
      -- Check second room likes
      Songs := Db_Wrapper.Get_Song_Table ("test_2", "likes");
      Assert (Natural (Songs.Length) = 1, "Wrong number of songs.");
      Assert (Song_Vectors.Element (Songs.First).Get_Id = "IGBxWcu6iXE", "Wrong song ID.");
      Assert (Song_Vectors.Element (Songs.First).Get_Title = "'test 2", "Wrong song title.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/IGBxWcu6iXE/default.jpg",
         "Wrong song thumbnail link.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Provider = Api.Youtube_Api,
         "Wrong song provider.");

      Test_Db.Close;
      -- Remove song from room likes while database is closed
      Songs := Db_Wrapper.Get_Song_Table ("test_1", "likes");
      Test_Db.Remove_From_Room_Likes ("test_1", Song.Constructors.Initialize);

      Assert (Songs = Db_Wrapper.Get_Song_Table ("test_1", "likes"), "Wrong songs liked.");

      Test_Db := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      -- Remove song from not existing room likes
      Test_Db.Remove_From_Room_Likes ("test_3", Song.Constructors.Initialize);

      Assert (Songs = Db_Wrapper.Get_Song_Table ("test_1", "likes"), "Wrong songs liked.");

      -- Remove song from existing room likes
      Test_Db.Remove_From_Room_Likes
      ("test_1", Song.Constructors.Initialize
         (Id             => "M0e_0P9OZuM",
          Title          => "test' 1",
          Thumbnail_Link => "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
          Provider       => Api.Youtube_Api));

      Songs := Db_Wrapper.Get_Song_Table ("test_1", "likes");
      Assert (Natural (Songs.Length) = 1, "Wrong number of songs.");
      -- Check song
      Assert (Song_Vectors.Element (Songs.First).Get_Id = "", "Wrong first song ID.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Title = "no song played",
         "Wrong first song title.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Thumbnail_Link = "",
         "Wrong first song thumbnail link.");
      Assert
        (Song_Vectors.Element (Songs.First).Get_Provider = Api.No_Provider_Api,
         "Wrong first song provider.");

      -- Remove song from existing room likes
      Test_Db.Remove_From_Room_Likes ("test_1", Song.Constructors.Initialize);

      Songs := Db_Wrapper.Get_Song_Table ("test_1", "likes");
      Assert (Songs.Is_Empty, "Wrong number of songs.");

      -- Remove not liked song from room likes
      Test_Db.Remove_From_Room_Likes ("test_1", Song.Constructors.Initialize);
      Test_Db.Remove_From_Room_Likes
      ("test_1", Song.Constructors.Initialize
         (Id             => "M0e_0P9OZuM",
          Title          => "test' 1",
          Thumbnail_Link => "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
          Provider       => Api.Youtube_Api));

      Assert (Songs = Db_Wrapper.Get_Song_Table ("test_1", "likes"), "First room likes modified.");

      -- Remove song from other existing room likes
      Songs := Db_Wrapper.Get_Song_Table ("test_1", "likes");
      Test_Db.Remove_From_Room_Likes
      ("test_2", Song.Constructors.Initialize
         (Id             => "IGBxWcu6iXE",
          Title          => "'test 2",
          Thumbnail_Link => "https://i.ytimg.com/vi/IGBxWcu6iXE/default.jpg",
          Provider       => Api.Youtube_Api));

      -- Check that first room likes has not changed
      Assert
        (Songs = Db_Wrapper.Get_Song_Table ("test_1", "likes"),
         "First room likes modified by second room.");

      -- Check second room
      Songs := Db_Wrapper.Get_Song_Table ("test_2", "likes");
      Assert (Songs.Is_Empty, "Wrong number of songs.");
   end Test_Remove_From_Room_Likes;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Rooms
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Rooms (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      use type Room_Name_Vector.T_Room_Name_Vector;

      pragma Unreferenced (Test_Case);

      Test_Db : T_Database_Access;

      Rooms : Room_Name_Vector.T_Room_Name_Vector;
   begin
      Database_Wrapper.Delete ("obj/ambi_test.sqlite3");
      Test_Db := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");

      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Add_To_Rooms ("test_2");

      Rooms := Test_Db.Get_Rooms;
      Assert (Natural (Rooms.Length) = 2, "Wrong number of rooms.");
      Assert (To_String (Rooms.Element (0)) = "test_1", "Wrong name for first room.");
      Assert (To_String (Rooms.Element (1)) = "test_2", "Wrong name for second room.");
      Assert (Rooms = Test_Db.Rooms, "Wrong rooms internally saved.");
   end Test_Get_Rooms;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Room_Historic
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Room_Historic (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Test_Db    : T_Database_Access;
      Db_Wrapper : Database_Wrapper.T_Database_Wrapper_Access;

      Songs : Song_Vector.T_Song_Vector;
   begin
      Database_Wrapper.Delete ("obj/ambi_test.sqlite3");
      Test_Db    := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Db_Wrapper := Database_Wrapper.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Add_To_Room_Historic ("test_1", Song.Constructors.Initialize);
      Test_Db.Add_To_Room_Historic
      ("test_1", Song.Constructors.Initialize
         (Id             => "M0e_0P9OZuM",
          Title          => "test' 1",
          Thumbnail_Link => "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
          Provider       => Api.Youtube_Api));

      Test_Db.Close;
      -- Get song from room historic while database is closed
      Songs := Test_Db.Get_Room_Historic ("test_1");

      Assert (Songs.Is_Empty, "Wrong number of songs.");

      Test_Db := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      -- Get song from not existing room historic
      Songs := Test_Db.Get_Room_Historic ("test_2");

      Assert (Songs.Is_Empty, "Wrong number of songs.");

      -- Get songs from existing room historic
      Songs := Test_Db.Get_Room_Historic ("test_1");

      Assert (Natural (Songs.Length) = 2, "Wrong number of songs.");
      Assert
        (Songs = Db_Wrapper.Get_Song_Table ("test_1", "historic"),
         "Wrong songs get from historic.");

      -- Get songs from room historic while historic has more than 50 songs
      for I in Natural range 1 .. 60 loop
         Test_Db.Add_To_Room_Historic ("test_1", Song.Constructors.Initialize);
      end loop;

      Songs := Test_Db.Get_Room_Historic ("test_1");

      Assert (Natural (Songs.Length) = 50, "Wrong number of songs.");
   end Test_Get_Room_Historic;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Room_Last_Songs
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Room_Last_Songs (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Test_Db    : T_Database_Access;
      Db_Wrapper : Database_Wrapper.T_Database_Wrapper_Access;

      Songs : Song_Vector.T_Song_Vector;
   begin
      Database_Wrapper.Delete ("obj/ambi_test.sqlite3");
      Test_Db    := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Db_Wrapper := Database_Wrapper.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Add_To_Room_Historic ("test_1", Song.Constructors.Initialize);
      Test_Db.Add_To_Room_Historic
      ("test_1", Song.Constructors.Initialize
         (Id             => "M0e_0P9OZuM",
          Title          => "test' 1",
          Thumbnail_Link => "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
          Provider       => Api.Youtube_Api));

      Test_Db.Close;
      -- Get song from room historic while database is closed
      Songs := Test_Db.Get_Room_Last_Songs ("test_1", 2);

      Assert (Songs.Is_Empty, "Wrong number of songs.");

      Test_Db := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      -- Get song from not existing room historic
      Songs := Test_Db.Get_Room_Last_Songs ("test_2", 2);

      Assert (Songs.Is_Empty, "Wrong number of songs.");

      -- Get songs from existing room historic
      Songs := Test_Db.Get_Room_Last_Songs ("test_1", 2);

      Assert (Natural (Songs.Length) = 2, "Wrong number of songs.");
      Assert
        (Songs = Db_Wrapper.Get_Song_Table ("test_1", "historic"),
         "Wrong songs get from historic.");

      -- Request more than available song from room historic
      Songs := Test_Db.Get_Room_Last_Songs ("test_1", 25);

      Assert (Natural (Songs.Length) = 2, "Wrong number of songs.");
      Assert
        (Songs = Db_Wrapper.Get_Song_Table ("test_1", "historic"),
         "Wrong songs get from historic.");
   end Test_Get_Room_Last_Songs;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Room_Likes (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Test_Db    : T_Database_Access;
      Db_Wrapper : Database_Wrapper.T_Database_Wrapper_Access;

      Songs : Song_Vector.T_Song_Vector;
   begin
      Database_Wrapper.Delete ("obj/ambi_test.sqlite3");
      Test_Db    := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Db_Wrapper := Database_Wrapper.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Add_To_Room_Likes ("test_1", Song.Constructors.Initialize);
      Test_Db.Add_To_Room_Likes
      ("test_1", Song.Constructors.Initialize
         (Id             => "M0e_0P9OZuM",
          Title          => "test' 1",
          Thumbnail_Link => "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
          Provider       => Api.Youtube_Api));

      Test_Db.Close;
      -- Get song from room likes while database is closed
      Songs := Test_Db.Get_Room_Likes ("test_1");

      Assert (Songs.Is_Empty, "Wrong number of songs.");

      Test_Db := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      -- Get song from not existing room likes
      Songs := Test_Db.Get_Room_Likes ("test_2");

      Assert (Songs.Is_Empty, "Wrong number of songs.");

      -- Get songs from existing room likes
      Songs := Test_Db.Get_Room_Likes ("test_1");

      Assert (Natural (Songs.Length) = 2, "Wrong number of songs.");
      Assert (Songs = Db_Wrapper.Get_Song_Table ("test_1", "likes"), "Wrong songs get from likes.");
   end Test_Get_Room_Likes;

   -------------------------------------------------------------------------------------------------
   -- Test_Is_Room_Song_Liked
   -------------------------------------------------------------------------------------------------
   procedure Test_Is_Room_Song_Liked (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Test_Db : T_Database_Access;
   begin
      Database_Wrapper.Delete ("obj/ambi_test.sqlite3");
      Test_Db := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Add_To_Room_Likes ("test_1", Song.Constructors.Initialize);

      Test_Db.Close;
      -- Check if song is liked in room while database is closed
      Assert
        (not Test_Db.Is_Room_Song_Liked ("test_1", Song.Constructors.Initialize),
         "False not returned when database is closed.");

      Test_Db := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      -- Check if song is liked in not existing room
      Assert
        (not Test_Db.Is_Room_Song_Liked ("test_2", Song.Constructors.Initialize),
         "False not returned when room does not exist.");

      -- Check if song is liked in existing room
      Assert
        (Test_Db.Is_Room_Song_Liked ("test_1", Song.Constructors.Initialize),
         "True not returned for a liked song.");
      Assert
        (not Test_Db.Is_Room_Song_Liked
         ("test_1", Song.Constructors.Initialize
            (Id             => "M0e_0P9OZuM",
             Title          => "test' 1",
             Thumbnail_Link => "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
             Provider       => Api.Youtube_Api)),
         "False not returned for a not liked song.");
   end Test_Is_Room_Song_Liked;

   -------------------------------------------------------------------------------------------------
   -- Test_Read_Rooms_In_Db
   -------------------------------------------------------------------------------------------------
   procedure Test_Read_Rooms_In_Db (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      use type Room_Name_Vector.T_Room_Name_Vector;

      pragma Unreferenced (Test_Case);

      Test_Db    : T_Database_Access;
      Db_Wrapper : Database_Wrapper.T_Database_Wrapper_Access;
   begin
      Database_Wrapper.Delete ("obj/ambi_test.sqlite3");
      Test_Db    := Database.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Db_Wrapper := Database_Wrapper.Constructors.New_And_Initialize ("obj/ambi_test.sqlite3");
      Test_Db.Add_To_Rooms ("test_1");
      Test_Db.Add_To_Rooms ("test_2");

      Test_Db.Rooms := Room_Name_Vector.Constructors.Initialize;
      Test_Db.Read_Rooms_In_Db;

      Assert (Db_Wrapper.Get_Rooms = Test_Db.Rooms, "Wrong rooms read from database.");
   end Test_Read_Rooms_In_Db;

end Database.Test;
