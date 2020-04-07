with Db;
with Db.Sqlite;

with Room_Name_Vector; use Room_Name_Vector;
with Song;             use Song;
with Song_Vector;      use Song_Vector;

package Database is

   type T_Database is tagged limited private;
   type T_Database_Class_Access is access all T_Database'Class;

   -------------------------------------------------------------------------------------------------
   -- Open
   -------------------------------------------------------------------------------------------------
   procedure Open (This : in out T_Database);

   -------------------------------------------------------------------------------------------------
   -- Close
   -------------------------------------------------------------------------------------------------
   procedure Close (This : in out T_Database);

   -------------------------------------------------------------------------------------------------
   -- Add_To_Rooms
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Rooms (This : in out T_Database; Room_Name : in String);

   -------------------------------------------------------------------------------------------------
   -- Add_To_Room_Historic
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Room_Historic
     (This      : in out T_Database;
      Room_Name : in     String;
      New_Song  : in     T_Song);

   -------------------------------------------------------------------------------------------------
   -- Add_To_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Room_Likes
     (This      : in out T_Database;
      Room_Name : in     String;
      New_Song  : in     T_Song);

   -------------------------------------------------------------------------------------------------
   -- Remove_From_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Remove_From_Room_Likes
     (This      : in out T_Database;
      Room_Name : in     String;
      Old_Song  : in     T_Song);

   -------------------------------------------------------------------------------------------------
   -- Get_Rooms
   -------------------------------------------------------------------------------------------------
   function Get_Rooms (This : in T_Database) return T_Room_Name_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Historic
   -------------------------------------------------------------------------------------------------
   function Get_Room_Historic (This : in T_Database; Room_Name : in String) return T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Last_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Room_Last_Songs
     (This            : in T_Database;
      Room_Name       : in String;
      Number_Of_Songs : in Natural) return T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Likes
   -------------------------------------------------------------------------------------------------
   function Get_Room_Likes (This : in T_Database; Room_Name : in String) return T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Is_Room_Song_Liked
   -------------------------------------------------------------------------------------------------
   function Is_Room_Song_Liked
     (This          : in T_Database;
      Room_Name     : in String;
      Song_To_Check : in T_Song) return Boolean;

private

   -------------------------------------------------------------------------------------------------
   -- Get_Rooms
   -------------------------------------------------------------------------------------------------
   procedure Read_Rooms_In_Db (This : in out T_Database);

   Max_Historic_Songs : constant := 500;

   type T_Database is tagged limited record
      Db_Handle : Db.Sqlite.Handle;
      Rooms     : T_Room_Name_Vector := Room_Name_Vector.Constructors.Initialize;
   end record;

end Database;
