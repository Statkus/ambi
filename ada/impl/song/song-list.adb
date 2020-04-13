package body Song.List is

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   function Initialize return T_Song_List is (T_Song_List'(Song_Vectors.Vector with null record));

   -------------------------------------------------------------------------------------------------
   -- Select_First_Song_Not_In_Exclusion_List
   -------------------------------------------------------------------------------------------------
   function Select_First_Song_Not_In_Exclusion_List
     (This           : in T_Song_List;
      Exclusion_List : in T_Song_List) return T_Song
   is
      use type Song_Vectors.Cursor;

      Song_List_Cursor    : Song_Vectors.Cursor := This.First;
      Selected_Song       : T_Song              := Initialize;
      Selected_Song_Found : Boolean             := False;
   begin
      while not Selected_Song_Found and Song_Vectors.Has_Element (Song_List_Cursor) loop
         if Exclusion_List.Find (Song_Vectors.Element (Song_List_Cursor)) =
           Song_Vectors.No_Element
         then
            Selected_Song       := Song_Vectors.Element (Song_List_Cursor);
            Selected_Song_Found := True;
         end if;

         Song_Vectors.Next (Song_List_Cursor);
      end loop;

      if not Selected_Song_Found and not This.Is_Empty then
         Selected_Song := Song_Vectors.Element (This.First);
      end if;

      return Selected_Song;
   end Select_First_Song_Not_In_Exclusion_List;

   -------------------------------------------------------------------------------------------------
   -- Iterate
   -------------------------------------------------------------------------------------------------
   procedure Iterate
     (This    : in T_Song_List;
      Process :    not null access procedure (Element : in T_Song))
   is
      ----------------------------------------------------------------------------------------------
      -- Query_Element
      ----------------------------------------------------------------------------------------------
      procedure Query_Element (Position : in Song_Vectors.Cursor) is
      begin
         Song_Vectors.Query_Element (Position, Process);
      end Query_Element;
   begin
      This.Iterate (Query_Element'Access);
   end Iterate;

   -------------------------------------------------------------------------------------------------
   -- Reverse_Iterate
   -------------------------------------------------------------------------------------------------
   procedure Reverse_Iterate
     (This    : in T_Song_List;
      Process :    not null access procedure (Element : in T_Song))
   is
      ----------------------------------------------------------------------------------------------
      -- Query_Element
      ----------------------------------------------------------------------------------------------
      procedure Query_Element (Position : in Song_Vectors.Cursor) is
      begin
         Song_Vectors.Query_Element (Position, Process);
      end Query_Element;
   begin
      This.Reverse_Iterate (Query_Element'Access);
   end Reverse_Iterate;

end Song.List;
