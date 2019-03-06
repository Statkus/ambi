with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Database is

   -------------------------------------------------------------------------------------------------
   -- Open
   -------------------------------------------------------------------------------------------------
   procedure Open (This : in out T_Database) is
   begin
      DB.SQLite.Connect (This.DB_Handle, "ambi.sqlite3");

      -- Create the historic table if it does not exist yet
      DB.SQLite.Execute (This.DB_Handle, "CREATE TABLE IF NOT EXISTS historic (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, video_id TEXT NOT NULL, video_title TEXT NOT NULL, video_thumbnail TEXT NOT NULL);");

      -- Create the likes table if it does not exist yet
      DB.SQLite.Execute (This.DB_Handle, "CREATE TABLE IF NOT EXISTS likes (video_id TEXT NOT NULL PRIMARY KEY, video_title TEXT NOT NULL, video_thumbnail TEXT NOT NULL);");

      This.Read_Historic_In_DB;
      This.Read_Likes_In_DB;
   end Open;

   -------------------------------------------------------------------------------------------------
   -- Close
   -------------------------------------------------------------------------------------------------
   procedure Close (This : in out T_Database) is
   begin
      DB.SQLite.Close (This.DB_Handle);
   end Close;

   -------------------------------------------------------------------------------------------------
   -- Get_Historic
   -------------------------------------------------------------------------------------------------
   function Get_Historic (This : in T_Database) return Playlist.Video_Vectors.Vector is
     (This.Historic);

   -------------------------------------------------------------------------------------------------
   -- Get_Likes
   -------------------------------------------------------------------------------------------------
   function Get_Likes (This : in T_Database) return Playlist.Video_Vectors.Vector is
     (This.Likes);

   -------------------------------------------------------------------------------------------------
   -- Add_To_Historic
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Historic (This : in out T_Database; Video : in YT_API.T_Video) is
   begin
      DB.SQLite.Execute (This.DB_Handle,
        "INSERT INTO historic (video_id, video_title, video_thumbnail) VALUES ('"
        & To_String (Video.Video_ID) & "', '"
        & To_String (Video.Video_Title) & "', '"
        & To_String (Video.Video_Thumbnail) & "');");

      This.Historic.Append (Video);
   end Add_To_Historic;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Likes
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Likes (This : in out T_Database; Video : in YT_API.T_Video) is
      Likes_Cursor        : Playlist.Video_Vectors.Cursor := This.Likes.First;
      Video_Already_Liked : Boolean := False;
   begin
      while Playlist.Video_Vectors.Has_Element (Likes_Cursor) loop
         if Playlist.Video_Vectors.Element (Likes_Cursor).Video_ID = Video.Video_ID then
            Video_Already_Liked := True;
            exit;
         end if;

         Likes_Cursor := Playlist.Video_Vectors.Next (Likes_Cursor);
      end loop;

      if not Video_Already_Liked then
         DB.SQLite.Execute (This.DB_Handle,
           "INSERT INTO likes (video_id, video_title, video_thumbnail) VALUES ('"
           & To_String (Video.Video_ID) & "', '"
           & To_String (Video.Video_Title) & "', '"
           & To_String (Video.Video_Thumbnail) & "');");

         This.Likes.Append (Video);
      end if;
   end Add_To_Likes;

   -------------------------------------------------------------------------------------------------
   -- Remove_From_Likes
   -------------------------------------------------------------------------------------------------
   procedure Remove_From_Likes (This : in out T_Database; Video : in YT_API.T_Video) is
   begin
      null;
   end Remove_From_Likes;

   -------------------------------------------------------------------------------------------------
   -- Read_Historic_In_DB
   -------------------------------------------------------------------------------------------------
   procedure Read_Historic_In_DB (This : in out T_Database) is
      DB_Iterator   : DB.SQLite.Iterator;
      DB_Row        : DB.String_Vectors.Vector;
      DB_Row_Cursor : DB.String_Vectors.Cursor;

      Video : YT_API.T_Video;
   begin
      DB.SQLite.Prepare_Select (This.DB_Handle, DB_Iterator, "SELECT * FROM historic");

      while DB.SQLite.More (DB_Iterator) loop
         DB.SQLite.Get_Line (DB_Iterator, DB_Row);
         DB_Row_Cursor := DB_Row.First;
         DB.String_Vectors.Next (DB_Row_Cursor);

         Video.Video_ID := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));
         DB.String_Vectors.Next (DB_Row_Cursor);

         Video.Video_Title := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));
         DB.String_Vectors.Next (DB_Row_Cursor);

         Video.Video_Thumbnail := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));

         This.Historic.Append (Video);
      end loop;

      DB.SQLite.End_Select (DB_Iterator);
   end Read_Historic_In_DB;

   -------------------------------------------------------------------------------------------------
   -- Read_Likes_In_DB
   -------------------------------------------------------------------------------------------------
   procedure Read_Likes_In_DB (This : in out T_Database) is
      DB_Iterator   : DB.SQLite.Iterator;
      DB_Row        : DB.String_Vectors.Vector;
      DB_Row_Cursor : DB.String_Vectors.Cursor;

      Video : YT_API.T_Video;
   begin
      DB.SQLite.Prepare_Select (This.DB_Handle, DB_Iterator, "SELECT * FROM likes");

      while DB.SQLite.More (DB_Iterator) loop
         DB.SQLite.Get_Line (DB_Iterator, DB_Row);
         DB_Row_Cursor := DB_Row.First;

         Video.Video_ID := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));
         DB.String_Vectors.Next (DB_Row_Cursor);

         Video.Video_Title := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));
         DB.String_Vectors.Next (DB_Row_Cursor);

         Video.Video_Thumbnail := To_Unbounded_String (DB.String_Vectors.Element (DB_Row_Cursor));

         This.Likes.Append (Video);
      end loop;

      DB.SQLite.End_Select (DB_Iterator);
   end Read_Likes_In_DB;

end Database;
