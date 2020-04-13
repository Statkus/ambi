package body Room.List is

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   function Initialize return T_Room_List is (T_Room_List'(Room_Vectors.Vector with null record));

   -------------------------------------------------------------------------------------------------
   -- Get_Room
   -------------------------------------------------------------------------------------------------
   function Get_Room (This : in T_Room_List; Room_Name : in String) return T_Room_Access is
      Room_List_Cursor : Room_Vectors.Cursor := This.First;
      Requested_Room   : T_Room_Access       := null;
   begin
      while Room_Vectors.Has_Element (Room_List_Cursor) and Requested_Room = null loop
         if Room_Vectors.Element (Room_List_Cursor).Get_Name = Room_Name then
            Requested_Room := Room_Vectors.Element (Room_List_Cursor);
         end if;

         Room_Vectors.Next (Room_List_Cursor);
      end loop;

      return Requested_Room;
   end Get_Room;

end Room.List;
