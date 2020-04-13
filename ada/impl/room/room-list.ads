with Ada.Containers.Vectors;

package Room.List is

   package Room_Vectors is new Ada.Containers.Vectors (Natural, T_Room_Access);

   type T_Room_List is new Room_Vectors.Vector with null record;

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   function Initialize return T_Room_List;

   -------------------------------------------------------------------------------------------------
   -- Get_Room
   -------------------------------------------------------------------------------------------------
   function Get_Room (This : in T_Room_List; Room_Name : in String) return T_Room_Access;

end Room.List;
