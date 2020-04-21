with Db.Sqlite;

with Room_Name_Vector;
with Song_Vector;

package Database_Wrapper is

   type T_Database_Wrapper (File_Name_Length : Natural) is tagged limited private;
   type T_Database_Wrapper_Access is access all T_Database_Wrapper;

   package Constructors is

      ----------------------------------------------------------------------------------------------
      -- New_And_Initialize
      ----------------------------------------------------------------------------------------------
      function New_And_Initialize (File_Name : in String) return T_Database_Wrapper_Access;

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Close
   -------------------------------------------------------------------------------------------------
   procedure Close (This : in out T_Database_Wrapper);

   -------------------------------------------------------------------------------------------------
   -- Delete
   -------------------------------------------------------------------------------------------------
   procedure Delete (File_Name : in String);

   -------------------------------------------------------------------------------------------------
   -- Is_Table_Exist
   -------------------------------------------------------------------------------------------------
   function Is_Table_Exist (This : in T_Database_Wrapper; Table_Name : in String) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Is_Column_Of_Table_Exist
   -------------------------------------------------------------------------------------------------
   function Is_Column_Of_Table_Exist
     (This        : in T_Database_Wrapper;
      Table_Name  : in String;
      Column_Name : in String) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Get_Rooms
   -------------------------------------------------------------------------------------------------
   function Get_Rooms (This : in T_Database_Wrapper) return Room_Name_Vector.T_Room_Name_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Table
   -------------------------------------------------------------------------------------------------
   function Get_Song_Table
     (This       : in T_Database_Wrapper;
      Room_Name  : in String;
      Table_Name : in String) return Song_Vector.T_Song_Vector;

private

   type T_Database_Wrapper (File_Name_Length : Natural) is tagged limited record
      File_Name : String (1 .. File_Name_Length);
      Db_Handle : Db.Sqlite.Handle;
   end record;

end Database_Wrapper;
