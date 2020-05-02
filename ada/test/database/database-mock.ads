with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Database.Mock is

   type T_Database_Mock is new T_Database with private;
   type T_Database_Mock_Access is access all T_Database_Mock;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize return T_Database_Mock_Access;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Room_History
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Room_History
     (This      : in out T_Database_Mock;
      Room_Name : in     String;
      New_Song  : in     Song.T_Song);

   -------------------------------------------------------------------------------------------------
   -- Get_Last_Room_Name
   -------------------------------------------------------------------------------------------------
   function Get_Last_Room_Name (This : in out T_Database_Mock) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_History
   -------------------------------------------------------------------------------------------------
   function Get_History (This : in out T_Database_Mock) return Song.List.T_Song_List;

private

   type T_Database_Mock is new T_Database with record
      Last_Room_Name : Unbounded_String;
      History        : Song.List.T_Song_List;
   end record;

end Database.Mock;
