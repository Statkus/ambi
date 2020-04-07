package body Room_Name_Vector is

   package body Constructors is

      ----------------------------------------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------------------------------------
      function Initialize return T_Room_Name_Vector is
        (T_Room_Name_Vector'(Room_Name_Vectors.Vector with null record));

   end Constructors;

end Room_Name_Vector;
