package body Room_Name_List is

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   function Initialize return T_Room_Name_List is
     (T_Room_Name_List'(Room_Name_Vectors.Vector with null record));

end Room_Name_List;
