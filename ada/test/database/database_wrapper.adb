with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Api;
with Song;

package body Database_Wrapper is

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize (File_Name : in String) return T_Database_Wrapper_Access is
      Test_Db : constant T_Database_Wrapper_Access :=
        new T_Database_Wrapper'
          (File_Name_Length => File_Name'Length, File_Name => File_Name, others => <>);
   begin
      Db.Sqlite.Connect (Test_Db.Db_Handle, File_Name);

      return Test_Db;
   end New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Close
   -------------------------------------------------------------------------------------------------
   procedure Close (This : in out T_Database_Wrapper) is
   begin
      Db.Sqlite.Close (This.Db_Handle);
   end Close;

   -------------------------------------------------------------------------------------------------
   -- Delete
   -------------------------------------------------------------------------------------------------
   procedure Delete (File_Name : in String) is
   begin
      if Ada.Directories.Exists (File_Name) then
         Ada.Directories.Delete_File (File_Name);
      end if;
   end Delete;

   -------------------------------------------------------------------------------------------------
   -- Is_Table_Exist
   -------------------------------------------------------------------------------------------------
   function Is_Table_Exist (This : in T_Database_Wrapper; Table_Name : in String) return Boolean is
      Db_Iterator : Db.Sqlite.Iterator;
      Table_Exist : Boolean := False;
   begin
      Db.Sqlite.Prepare_Select
        (This.Db_Handle,
         Db_Iterator,
         "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '" & Table_Name & "';");

      Table_Exist := Db.Sqlite.More (Db_Iterator);

      Db.Sqlite.End_Select (Db_Iterator);

      return Table_Exist;
   end Is_Table_Exist;

   -------------------------------------------------------------------------------------------------
   -- Is_Column_Of_Table_Exist
   -------------------------------------------------------------------------------------------------
   function Is_Column_Of_Table_Exist
     (This        : in T_Database_Wrapper;
      Table_Name  : in String;
      Column_Name : in String) return Boolean
   is
      Db_Iterator  : Db.Sqlite.Iterator;
      Column_Exist : Boolean := False;
   begin
      Db.Sqlite.Prepare_Select
        (This.Db_Handle,
         Db_Iterator,
         "SELECT name FROM pragma_table_info('" &
         Table_Name &
         "') WHERE name='" &
         Column_Name &
         "';");

      Column_Exist := Db.Sqlite.More (Db_Iterator);

      Db.Sqlite.End_Select (Db_Iterator);

      return Column_Exist;
   end Is_Column_Of_Table_Exist;

   -------------------------------------------------------------------------------------------------
   -- Get_Rooms
   -------------------------------------------------------------------------------------------------
   function Get_Rooms (This : in T_Database_Wrapper) return Room_Name_List.T_Room_Name_List is
      Db_Iterator : Db.Sqlite.Iterator;
      Db_Row      : Db.String_Vectors.Vector;

      Rooms : Room_Name_List.T_Room_Name_List := Room_Name_List.Initialize;
   begin
      Db.Sqlite.Prepare_Select (This.Db_Handle, Db_Iterator, "SELECT * FROM rooms;");

      while Db.Sqlite.More (Db_Iterator) loop
         Db.Sqlite.Get_Line (Db_Iterator, Db_Row);

         Rooms.Append (To_Unbounded_String (Db_Row.First_Element));
      end loop;

      Db.Sqlite.End_Select (Db_Iterator);

      return Rooms;
   end Get_Rooms;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Table
   -------------------------------------------------------------------------------------------------
   function Get_Song_Table
     (This       : in T_Database_Wrapper;
      Room_Name  : in String;
      Table_Name : in String) return Song.List.T_Song_List
   is
      Db_Iterator             : Db.Sqlite.Iterator;
      Db_Row                  : Db.String_Vectors.Vector;
      Db_Row_Id_Cursor        : Db.String_Vectors.Cursor;
      Db_Row_Title_Cursor     : Db.String_Vectors.Cursor;
      Db_Row_Thumbnail_Cursor : Db.String_Vectors.Cursor;
      Db_Row_Provider_Cursor  : Db.String_Vectors.Cursor;

      Song_List : Song.List.T_Song_List := Song.List.Initialize;
   begin
      Db.Sqlite.Prepare_Select
        (This.Db_Handle,
         Db_Iterator,
         "SELECT * FROM " &
         Table_Name &
         " INDEXED BY room_index_on_" &
         Table_Name &
         " WHERE room_name = '" &
         Room_Name &
         "';");

      while Db.Sqlite.More (Db_Iterator) loop
         Db.Sqlite.Get_Line (Db_Iterator, Db_Row);

         if Table_Name = "history" then
            Db_Row_Id_Cursor := Db.String_Vectors.Next (Db_Row.First);
         else
            Db_Row_Id_Cursor := Db_Row.First;
         end if;
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

      return Song_List;
   end Get_Song_Table;

end Database_Wrapper;
