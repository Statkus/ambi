with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Api;

package body Database is

   use type Api.T_Api_Provider;

   ----------------------------------------------------------------------------------------------
   -- New_And_Initialize
   ----------------------------------------------------------------------------------------------
   function New_And_Initialize (File_Name : in String) return T_Database_Class_Access is
      Ambi_Db : constant T_Database_Class_Access :=
        new T_Database'(Open => False, Rooms => Room_Name_List.Initialize, others => <>);
   begin
      Db.Sqlite.Connect (Ambi_Db.Db_Handle, File_Name);

      Ambi_Db.Open := True;

      -- Create the rooms table if it does not exist yet
      Db.Sqlite.Execute
        (Ambi_Db.Db_Handle,
         "CREATE TABLE IF NOT EXISTS rooms (room_name TEXT NOT NULL PRIMARY KEY);");

      -- Create the historic table if it does not exist yet
      Db.Sqlite.Execute
        (Ambi_Db.Db_Handle,
         "CREATE TABLE IF NOT EXISTS historic (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, song_id TEXT NOT NULL, song_title TEXT NOT NULL, song_thumbnail_link TEXT NOT NULL, song_provider TEXT NOT NULL, room_name TEXT NOT NULL);");

      -- Add an index on room_name for historic table if it does not exist yet
      Db.Sqlite.Execute
        (Ambi_Db.Db_Handle,
         "CREATE INDEX IF NOT EXISTS room_index_on_historic ON historic (room_name);");

      -- Create the likes table if it does not exist yet
      Db.Sqlite.Execute
        (Ambi_Db.Db_Handle,
         "CREATE TABLE IF NOT EXISTS likes (song_id TEXT NOT NULL PRIMARY KEY, song_title TEXT NOT NULL, song_thumbnail_link TEXT NOT NULL, song_provider TEXT NOT NULL, room_name TEXT NOT NULL);");

      -- Add an index on room_name for likes table if it does not exist yet
      Db.Sqlite.Execute
        (Ambi_Db.Db_Handle,
         "CREATE INDEX IF NOT EXISTS room_index_on_likes ON likes (room_name);");

      Ambi_Db.Read_Rooms_In_Db;

      return Ambi_Db;
   end New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Close
   -------------------------------------------------------------------------------------------------
   procedure Close (This : in out T_Database) is
   begin
      if This.Open then
         Db.Sqlite.Close (This.Db_Handle);
         This.Open := False;
      end if;
   end Close;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Rooms
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Rooms (This : in out T_Database; Room_Name : in String) is
   begin
      if This.Open and then not This.Rooms.Contains (To_Unbounded_String (Room_Name)) then
         Db.Sqlite.Execute
           (This.Db_Handle,
            "INSERT INTO rooms (room_name) VALUES ('" & Room_Name & "');");

         This.Rooms.Append (To_Unbounded_String (Room_Name));
      end if;
   end Add_To_Rooms;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Room_Historic
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Room_Historic
     (This      : in out T_Database;
      Room_Name : in     String;
      New_Song  : in     Song.T_Song)
   is
      Title      : constant String := New_Song.Get_Title;
      Song_Title : Unbounded_String;
   begin
      if This.Open and then This.Rooms.Contains (To_Unbounded_String (Room_Name)) then
         for C of Title loop
            if Character'Pos (C) = 39 then
               Append (Song_Title, "''");
            else
               Append (Song_Title, C);
            end if;
         end loop;

         Db.Sqlite.Execute
           (This.Db_Handle,
            "INSERT INTO historic (song_id, song_title, song_thumbnail_link, song_provider, room_name) VALUES ('" &
            New_Song.Get_Id &
            "', '" &
            To_String (Song_Title) &
            "', '" &
            New_Song.Get_Thumbnail_Link &
            "', '" &
            New_Song.Get_Provider'Img &
            "', '" &
            Room_Name &
            "');");
      end if;
   end Add_To_Room_Historic;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Room_Likes
     (This      : in out T_Database;
      Room_Name : in     String;
      New_Song  : in     Song.T_Song)
   is
      Title      : constant String := New_Song.Get_Title;
      Song_Title : Unbounded_String;
   begin
      if This.Open
        and then This.Rooms.Contains (To_Unbounded_String (Room_Name))
        and then not This.Is_Room_Song_Liked (Room_Name, New_Song)
      then
         for C of Title loop
            if Character'Pos (C) = 39 then
               Append (Song_Title, "''");
            else
               Append (Song_Title, C);
            end if;
         end loop;

         Db.Sqlite.Execute
           (This.Db_Handle,
            "INSERT INTO likes (song_id, song_title, song_thumbnail_link, song_provider, room_name) VALUES ('" &
            New_Song.Get_Id &
            "', '" &
            To_String (Song_Title) &
            "', '" &
            New_Song.Get_Thumbnail_Link &
            "', '" &
            New_Song.Get_Provider'Img &
            "', '" &
            Room_Name &
            "');");
      end if;
   end Add_To_Room_Likes;

   -------------------------------------------------------------------------------------------------
   -- Remove_From_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Remove_From_Room_Likes
     (This      : in out T_Database;
      Room_Name : in     String;
      Old_Song  : in     Song.T_Song)
   is
   begin
      if This.Open
        and then This.Rooms.Contains (To_Unbounded_String (Room_Name))
        and then This.Is_Room_Song_Liked (Room_Name, Old_Song)
      then
         Db.Sqlite.Execute
           (This.Db_Handle,
            "DELETE FROM likes WHERE room_name = '" &
            Room_Name &
            "' AND song_id = '" &
            Old_Song.Get_Id &
            "';");
      end if;
   end Remove_From_Room_Likes;

   -------------------------------------------------------------------------------------------------
   -- Get_Rooms
   -------------------------------------------------------------------------------------------------
   function Get_Rooms (This : in T_Database) return Room_Name_List.T_Room_Name_List is (This.Rooms);

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Historic
   -------------------------------------------------------------------------------------------------
   function Get_Room_Historic
     (This      : in T_Database;
      Room_Name : in String) return Song.List.T_Song_List is
     (This.Get_Room_Last_Songs (Room_Name, Historic_Length));

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Last_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Room_Last_Songs
     (This            : in T_Database;
      Room_Name       : in String;
      Number_Of_Songs : in Natural) return Song.List.T_Song_List
   is
      Db_Iterator             : Db.Sqlite.Iterator;
      Db_Row                  : Db.String_Vectors.Vector;
      Db_Row_Id_Cursor        : Db.String_Vectors.Cursor;
      Db_Row_Title_Cursor     : Db.String_Vectors.Cursor;
      Db_Row_Thumbnail_Cursor : Db.String_Vectors.Cursor;
      Db_Row_Provider_Cursor  : Db.String_Vectors.Cursor;

      Song_List : Song.List.T_Song_List := Song.List.Initialize;
   begin
      if This.Open and then This.Rooms.Contains (To_Unbounded_String (Room_Name)) then
         Db.Sqlite.Prepare_Select
           (This.Db_Handle,
            Db_Iterator,
            "SELECT * FROM historic INDEXED BY room_index_on_historic WHERE room_name = '" &
            Room_Name &
            "' ORDER BY id DESC;");

         while Db.Sqlite.More (Db_Iterator) and Natural (Song_List.Length) < Number_Of_Songs loop
            Db.Sqlite.Get_Line (Db_Iterator, Db_Row);

            Db_Row_Id_Cursor        := Db.String_Vectors.Next (Db_Row.First);
            Db_Row_Title_Cursor     := Db.String_Vectors.Next (Db_Row_Id_Cursor);
            Db_Row_Thumbnail_Cursor := Db.String_Vectors.Next (Db_Row_Title_Cursor);
            Db_Row_Provider_Cursor  := Db.String_Vectors.Next (Db_Row_Thumbnail_Cursor);

            Song_List.Prepend
            (Song.Initialize
               (Id             => Db.String_Vectors.Element (Db_Row_Id_Cursor),
                Title          => Db.String_Vectors.Element (Db_Row_Title_Cursor),
                Thumbnail_Link => Db.String_Vectors.Element (Db_Row_Thumbnail_Cursor),
                Provider       =>
                  Api.T_Api_Provider'Value (Db.String_Vectors.Element (Db_Row_Provider_Cursor))));
         end loop;

         Db.Sqlite.End_Select (Db_Iterator);
      end if;

      return Song_List;
   end Get_Room_Last_Songs;

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Likes
   -------------------------------------------------------------------------------------------------
   function Get_Room_Likes
     (This      : in T_Database;
      Room_Name : in String) return Song.List.T_Song_List
   is
      Db_Iterator             : Db.Sqlite.Iterator;
      Db_Row                  : Db.String_Vectors.Vector;
      Db_Row_Id_Cursor        : Db.String_Vectors.Cursor;
      Db_Row_Title_Cursor     : Db.String_Vectors.Cursor;
      Db_Row_Thumbnail_Cursor : Db.String_Vectors.Cursor;
      Db_Row_Provider_Cursor  : Db.String_Vectors.Cursor;

      Song_List : Song.List.T_Song_List := Song.List.Initialize;
   begin
      if This.Open and then This.Rooms.Contains (To_Unbounded_String (Room_Name)) then
         Db.Sqlite.Prepare_Select
           (This.Db_Handle,
            Db_Iterator,
            "SELECT * FROM likes INDEXED BY room_index_on_likes WHERE room_name = '" &
            Room_Name &
            "';");

         while Db.Sqlite.More (Db_Iterator) loop
            Db.Sqlite.Get_Line (Db_Iterator, Db_Row);

            Db_Row_Id_Cursor        := Db_Row.First;
            Db_Row_Title_Cursor     := Db.String_Vectors.Next (Db_Row_Id_Cursor);
            Db_Row_Thumbnail_Cursor := Db.String_Vectors.Next (Db_Row_Title_Cursor);
            Db_Row_Provider_Cursor  := Db.String_Vectors.Next (Db_Row_Thumbnail_Cursor);

            Song_List.Append
            (Song.Initialize
               (Id             => Db.String_Vectors.Element (Db_Row_Id_Cursor),
                Title          => Db.String_Vectors.Element (Db_Row_Title_Cursor),
                Thumbnail_Link => Db.String_Vectors.Element (Db_Row_Thumbnail_Cursor),
                Provider       =>
                  Api.T_Api_Provider'Value (Db.String_Vectors.Element (Db_Row_Provider_Cursor))));
         end loop;

         Db.Sqlite.End_Select (Db_Iterator);
      end if;

      return Song_List;
   end Get_Room_Likes;

   -------------------------------------------------------------------------------------------------
   -- Is_Room_Song_Liked
   -------------------------------------------------------------------------------------------------
   function Is_Room_Song_Liked
     (This          : in T_Database;
      Room_Name     : in String;
      Song_To_Check : in Song.T_Song) return Boolean
   is
      Db_Iterator : Db.Sqlite.Iterator;
      Liked       : Boolean := False;
   begin
      if This.Open and then This.Rooms.Contains (To_Unbounded_String (Room_Name)) then
         Db.Sqlite.Prepare_Select
           (This.Db_Handle,
            Db_Iterator,
            "SELECT * FROM likes INDEXED BY room_index_on_likes WHERE room_name = '" &
            Room_Name &
            "'AND song_id = '" &
            Song_To_Check.Get_Id &
            "';");

         Liked := Db.Sqlite.More (Db_Iterator);

         Db.Sqlite.End_Select (Db_Iterator);
      end if;

      return Liked;
   end Is_Room_Song_Liked;

   -------------------------------------------------------------------------------------------------
   -- Read_Rooms_In_Db
   -------------------------------------------------------------------------------------------------
   procedure Read_Rooms_In_Db (This : in out T_Database) is
      Db_Iterator : Db.Sqlite.Iterator;
      Db_Row      : Db.String_Vectors.Vector;
   begin
      Db.Sqlite.Prepare_Select (This.Db_Handle, Db_Iterator, "SELECT * FROM rooms;");

      while Db.Sqlite.More (Db_Iterator) loop
         Db.Sqlite.Get_Line (Db_Iterator, Db_Row);

         This.Rooms.Append (To_Unbounded_String (Db_Row.First_Element));
      end loop;

      Db.Sqlite.End_Select (Db_Iterator);
   end Read_Rooms_In_Db;

end Database;
