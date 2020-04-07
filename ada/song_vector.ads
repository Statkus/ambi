with Ada.Containers.Vectors;

with Song;

package Song_Vector is

   use type Song.T_Song;

   package Song_Vectors is new Ada.Containers.Vectors (Natural, Song.T_Song);

   type T_Song_Vector is new Song_Vectors.Vector with null record;

   package Constructors is

      ----------------------------------------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------------------------------------
      function Initialize return T_Song_Vector;

   end Constructors;

end Song_Vector;
