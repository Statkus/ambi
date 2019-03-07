with DB;
with DB.SQLite;

with Video_List; use Video_List;
with YT_API;

package Database is

   type T_Database is tagged limited private;

   type T_Database_Class_Access is access all T_Database'Class;

   procedure Open (This : in out T_Database);
   procedure Close (This : in out T_Database);

   function Get_Historic (This : in T_Database) return Video_Vectors.Vector;
   function Get_Likes (This : in T_Database) return Video_Vectors.Vector;

   procedure Add_To_Historic (This : in out T_Database; Video : in YT_API.T_Video);
   procedure Add_To_Likes (This : in out T_Database; Video : in YT_API.T_Video);

   procedure Remove_From_Likes (This : in out T_Database; Video : in YT_API.T_Video);

private

   procedure Read_Historic_In_DB (This : in out T_Database);
   procedure Read_Likes_In_DB (This : in out T_Database);

   type T_Database is tagged limited record
      DB_Handle : DB.SQLite.Handle;
      Historic  : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
      Likes     : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
   end record;

end Database;
