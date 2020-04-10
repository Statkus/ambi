package body Song_Vector is

   package body Constructors is

      ----------------------------------------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------------------------------------
      function Initialize return T_Song_Vector is
        (T_Song_Vector'(Song_Vectors.Vector with null record));

   end Constructors;

end Song_Vector;
