with DB;
with DB.SQLite;

with List; use List;

package Database is

   type T_Database is tagged limited private;

   type T_Database_Class_Access is access all T_Database'Class;

   procedure Open (This : in out T_Database);
   procedure Close (This : in out T_Database);

   procedure Add_To_Rooms (This : in out T_Database; Room_Name : in String);
   procedure Add_To_Room_Historic
     (This : in out T_Database; Room_Name : in String; Video : in T_Video);
   procedure Add_To_Room_Likes
     (This : in out T_Database; Room_Name : in String; Video : in T_Video);
   procedure Remove_From_Room_Likes
     (This : in out T_Database; Room_Name : in String; Video : in T_Video);

   function Get_Rooms (This : in T_Database) return Room_Name_Vectors.Vector;
   function Get_Room_Historic (This : in T_Database; Room_Name : in String)
     return Video_Vectors.Vector;
   function Get_Room_Likes (This : in T_Database; Room_Name : in String) return Video_Vectors.Vector;

   function Is_Room_Video_Liked (This : in T_Database; Room_Name : in String; Video : in T_Video)
     return Boolean;

private

   MAX_HISTORIC_VIDEOS : constant := 500;

   procedure Read_Rooms_In_DB (This : in out T_Database);

   type T_Database is tagged limited record
      DB_Handle : DB.SQLite.Handle;
      Rooms     : Room_Name_Vectors.Vector := Room_Name_Vectors.Empty_Vector;
   end record;

end Database;
