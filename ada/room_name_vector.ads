with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Room_Name_Vector is

   package Room_Name_Vectors is new Ada.Containers.Vectors (Natural, Unbounded_String);

   type T_Room_Name_Vector is new Room_Name_Vectors.Vector with null record;

   package Constructors is

      ----------------------------------------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------------------------------------
      function Initialize return T_Room_Name_Vector;

   end Constructors;

end Room_Name_Vector;
