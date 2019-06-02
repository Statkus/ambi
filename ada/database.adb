with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Database is

   -------------------------------------------------------------------------------------------------
   -- Open
   -------------------------------------------------------------------------------------------------
   procedure Open (This : in out T_Database) is
   begin
      DB.SQLite.Connect (This.DB_Handle, "ambi.sqlite3");

      -- Create the rooms table if it does not exist yet
      DB.SQLite.Execute (This.DB_Handle,
        "CREATE TABLE IF NOT EXISTS rooms (room_name TEXT NOT NULL PRIMARY KEY);");

      -- Create the historic table if it does not exist yet
      DB.SQLite.Execute (This.DB_Handle,
        "CREATE TABLE IF NOT EXISTS historic (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, video_id TEXT NOT NULL, video_title TEXT NOT NULL, video_thumbnail TEXT NOT NULL, room_name TEXT NOT NULL);");

      -- Add an index on room_name for historic table
      DB.SQLite.Execute (This.DB_Handle,
        "CREATE INDEX IF NOT EXISTS room_index_on_historic ON historic (room_name);");

      -- Create the likes table if it does not exist yet
      DB.SQLite.Execute (This.DB_Handle,
        "CREATE TABLE IF NOT EXISTS likes (video_id TEXT NOT NULL PRIMARY KEY, video_title TEXT NOT NULL, video_thumbnail TEXT NOT NULL, room_name TEXT NOT NULL);");

      -- Add an index on room_name for likes table
      DB.SQLite.Execute (This.DB_Handle,
        "CREATE INDEX IF NOT EXISTS room_index_on_likes ON likes (room_name);");

      This.Read_Rooms_In_DB;
   end Open;

   -------------------------------------------------------------------------------------------------
   -- Close
   -------------------------------------------------------------------------------------------------
   procedure Close (This : in out T_Database) is
   begin
      DB.SQLite.Close (This.DB_Handle);
   end Close;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Rooms
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Rooms (This : in out T_Database; Room_Name : in String) is
      DB_Iterator        : DB.SQLite.Iterator;
      Room_Already_Exist : Boolean := False;
   begin
      DB.SQLite.Prepare_Select
        (This.DB_Handle, DB_Iterator, "SELECT * FROM rooms WHERE room_name = '" & Room_Name & "';");

      Room_Already_Exist := DB.SQLite.More (DB_Iterator);

      DB.SQLite.End_Select (DB_Iterator);

      if not Room_Already_Exist then
         DB.SQLite.Execute
           (This.DB_Handle, "INSERT INTO rooms (room_name) VALUES ('" & Room_Name & "');");

         This.Rooms.Append (To_Unbounded_String (Room_Name));
      end if;
   end Add_To_Rooms;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Room_Historic
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Room_Historic
     (This : in out T_Database; Room_Name : in String; Video : in T_Video) is
      Title       : constant String := To_String (Video.Video_Title);
      Video_Title : Unbounded_String;
   begin
      for C of Title loop
         if Character'Pos (C) = 39 then
            Append (Video_Title, "''");
         else
            Append (Video_Title, C);
         end if;
      end loop;

      DB.SQLite.Execute (This.DB_Handle,
        "INSERT INTO historic (video_id, video_title, video_thumbnail, room_name) VALUES ('"
        & To_String (Video.Video_ID) & "', '"
        & To_String (Video_Title) & "', '"
        & To_String (Video.Video_Thumbnail) & "', '"
        & Room_Name & "');");
   end Add_To_Room_Historic;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Room_Likes
     (This : in out T_Database; Room_Name : in String; Video : in T_Video) is
      Title       : constant String := To_String (Video.Video_Title);
      Video_Title : Unbounded_String;
   begin
      if not This.Is_Room_Video_Liked (Room_Name, Video) then
         for C of Title loop
            if Character'Pos (C) = 39 then
               Append (Video_Title, "''");
            else
               Append (Video_Title, C);
            end if;
         end loop;

         DB.SQLite.Execute (This.DB_Handle,
           "INSERT INTO likes (video_id, video_title, video_thumbnail, room_name) VALUES ('"
           & To_String (Video.Video_ID) & "', '"
           & To_String (Video_Title) & "', '"
           & To_String (Video.Video_Thumbnail) & "', '"
           & Room_Name & "');");
      end if;
   end Add_To_Room_Likes;

   -------------------------------------------------------------------------------------------------
   -- Remove_From_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Remove_From_Room_Likes
     (This : in out T_Database; Room_Name : in String; Video : in T_Video) is
   begin
      if This.Is_Room_Video_Liked (Room_Name, Video) then
         DB.SQLite.Execute (This.DB_Handle,
           "DELETE FROM likes WHERE room_name = '" & Room_Name & "' AND video_id = '"
           & To_String (Video.Video_ID) & "';");
      end if;
   end Remove_From_Room_Likes;

   -------------------------------------------------------------------------------------------------
   -- Get_Rooms
   -------------------------------------------------------------------------------------------------
   function Get_Rooms (This : in T_Database) return Room_Name_Vectors.Vector is (This.Rooms);

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Historic
   -------------------------------------------------------------------------------------------------
   function Get_Room_Historic (This : in T_Database; Room_Name : in String)
     return Video_Vectors.Vector is (This.Get_Room_Last_Videos (Room_Name, MAX_HISTORIC_VIDEOS));

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Last_Videos
   -------------------------------------------------------------------------------------------------
   function Get_Room_Last_Videos
     (This : in T_Database; Room_Name : in String; Number_Of_Videos : in Natural)
     return Video_Vectors.Vector is
      DB_Iterator   : DB.SQLite.Iterator;
      DB_Row        : DB.String_Vectors.Vector;
      DB_Row_Cursor : DB.String_Vectors.Cursor;

      Video      : T_Video;
      Video_List : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
   begin
      DB.SQLite.Prepare_Select (This.DB_Handle, DB_Iterator,
        "SELECT * FROM historic INDEXED BY room_index_on_historic WHERE room_name = '" & Room_Name
        & "' ORDER BY id DESC;");

      while DB.SQLite.More (DB_Iterator)
        and Natural (Video_List.Length) < Number_Of_Videos loop
         DB.SQLite.Get_Line (DB_Iterator, DB_Row);

         DB_Row_Cursor := DB.String_Vectors.Next (DB_Row.First);
         Video.Video_ID := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));

         DB.String_Vectors.Next (DB_Row_Cursor);
         Video.Video_Title := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));

         DB.String_Vectors.Next (DB_Row_Cursor);
         Video.Video_Thumbnail := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));

         Video_List.Prepend (Video);
      end loop;

      DB.SQLite.End_Select (DB_Iterator);

      return Video_List;
   end Get_Room_Last_Videos;

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Likes
   -------------------------------------------------------------------------------------------------
   function Get_Room_Likes (This : in T_Database; Room_Name : in String)
     return Video_Vectors.Vector is
      DB_Iterator   : DB.SQLite.Iterator;
      DB_Row        : DB.String_Vectors.Vector;
      DB_Row_Cursor : DB.String_Vectors.Cursor;

      Video      : T_Video;
      Video_List : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
   begin
      DB.SQLite.Prepare_Select (This.DB_Handle, DB_Iterator,
        "SELECT * FROM likes INDEXED BY room_index_on_likes WHERE room_name = '" & Room_Name
        & "';");

      while DB.SQLite.More (DB_Iterator) loop
         DB.SQLite.Get_Line (DB_Iterator, DB_Row);

         DB_Row_Cursor := DB_Row.First;
         Video.Video_ID := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));

         DB.String_Vectors.Next (DB_Row_Cursor);
         Video.Video_Title := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));

         DB.String_Vectors.Next (DB_Row_Cursor);
         Video.Video_Thumbnail := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));

         Video_List.Append (Video);
      end loop;

      DB.SQLite.End_Select (DB_Iterator);

      return Video_List;
   end Get_Room_Likes;

   -------------------------------------------------------------------------------------------------
   -- Is_Room_Video_Liked
   -------------------------------------------------------------------------------------------------
   function Is_Room_Video_Liked (This : in T_Database; Room_Name : in String; Video : in T_Video)
     return Boolean is
      DB_Iterator : DB.SQLite.Iterator;
      Liked       : Boolean := False;
   begin
      DB.SQLite.Prepare_Select (This.DB_Handle, DB_Iterator,
        "SELECT * FROM likes INDEXED BY room_index_on_likes WHERE room_name = '" & Room_Name
        & "'AND video_id = '" & To_String (Video.Video_ID) & "';");

      Liked := DB.SQLite.More (DB_Iterator);

      DB.SQLite.End_Select (DB_Iterator);

      return Liked;
   end Is_Room_Video_Liked;

   -------------------------------------------------------------------------------------------------
   -- Read_Rooms_In_DB
   -------------------------------------------------------------------------------------------------
   procedure Read_Rooms_In_DB (This : in out T_Database) is
      DB_Iterator   : DB.SQLite.Iterator;
      DB_Row        : DB.String_Vectors.Vector;
   begin
      DB.SQLite.Prepare_Select (This.DB_Handle, DB_Iterator, "SELECT * FROM rooms;");

      while DB.SQLite.More (DB_Iterator) loop
         DB.SQLite.Get_Line (DB_Iterator, DB_Row);

         This.Rooms.Append (To_Unbounded_String (DB_Row.First_Element));
      end loop;

      DB.SQLite.End_Select (DB_Iterator);
   end Read_Rooms_In_DB;

end Database;
