package body Room.List.Thread_Safe is

   protected body T_Thread_Safe_Room_List is

      ----------------------------------------------------------------------------------------------
      -- Append
      ----------------------------------------------------------------------------------------------
      procedure Append (New_Room : in T_Room_Access) is
      begin
         Thread_Safe_Room_List.Append (New_Room);
      end Append;

      ----------------------------------------------------------------------------------------------
      -- Last_Element
      ----------------------------------------------------------------------------------------------
      function Last_Element return T_Room_Access is (Thread_Safe_Room_List.Last_Element);

      ----------------------------------------------------------------------------------------------
      -- Length
      ----------------------------------------------------------------------------------------------
      function Length return Natural is (Natural (Thread_Safe_Room_List.Length));

      ----------------------------------------------------------------------------------------------
      -- Get_Room
      ----------------------------------------------------------------------------------------------
      function Get_Room
        (Room_Name : in String) return T_Room_Access is
        (Thread_Safe_Room_List.Get_Room (Room_Name));

   end T_Thread_Safe_Room_List;

end Room.List.Thread_Safe;
