with Db.Sqlite;

with Room_Name_Vector;
with Song;
with Song_Vector;

package Database is

   type T_Database is tagged limited private;
   type T_Database_Access is access all T_Database;

   package Constructors is

      ----------------------------------------------------------------------------------------------
      -- New_And_Initialize
      ----------------------------------------------------------------------------------------------
      function New_And_Initialize (File_Name : in String) return T_Database_Access;

   end Constructors;

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
      New_Song  : in     Song.T_Song);

   -------------------------------------------------------------------------------------------------
   -- Add_To_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Room_Likes
     (This      : in out T_Database;
      Room_Name : in     String;
      New_Song  : in     Song.T_Song);

   -------------------------------------------------------------------------------------------------
   -- Remove_From_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Remove_From_Room_Likes
     (This      : in out T_Database;
      Room_Name : in     String;
      Old_Song  : in     Song.T_Song);

   -------------------------------------------------------------------------------------------------
   -- Get_Rooms
   -------------------------------------------------------------------------------------------------
   function Get_Rooms (This : in T_Database) return Room_Name_Vector.T_Room_Name_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Historic
   -------------------------------------------------------------------------------------------------
   function Get_Room_Historic
     (This      : in T_Database;
      Room_Name : in String) return Song_Vector.T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Last_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Room_Last_Songs
     (This            : in T_Database;
      Room_Name       : in String;
      Number_Of_Songs : in Natural) return Song_Vector.T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Room_Likes
   -------------------------------------------------------------------------------------------------
   function Get_Room_Likes
     (This      : in T_Database;
      Room_Name : in String) return Song_Vector.T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Is_Room_Song_Liked
   -------------------------------------------------------------------------------------------------
   function Is_Room_Song_Liked
     (This          : in T_Database;
      Room_Name     : in String;
      Song_To_Check : in Song.T_Song) return Boolean;

private

   -------------------------------------------------------------------------------------------------
   -- Read_Rooms_In_Db
   -------------------------------------------------------------------------------------------------
   procedure Read_Rooms_In_Db (This : in out T_Database);

   Historic_Length : constant := 50;

   type T_Database is tagged limited record
      Db_Handle : Db.Sqlite.Handle;
      Open      : Boolean;
      Rooms     : Room_Name_Vector.T_Room_Name_Vector;
   end record;

end Database;
