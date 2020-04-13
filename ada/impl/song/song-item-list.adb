package body Song.Item.List is

   protected body T_Item_List is

      ----------------------------------------------------------------------------------------------
      -- Set
      ----------------------------------------------------------------------------------------------
      procedure Set (Item_Vector : in Item_Vectors.Vector) is
      begin
         Item_List := Item_Vector;
      end Set;

      ----------------------------------------------------------------------------------------------
      -- Append
      ----------------------------------------------------------------------------------------------
      procedure Append (New_Item : in T_Item) is
      begin
         Item_List.Append (New_Item);
      end Append;

      ----------------------------------------------------------------------------------------------
      -- Delete_First
      ----------------------------------------------------------------------------------------------
      procedure Delete_First is
      begin
         Item_List.Delete_First;
      end Delete_First;

      ----------------------------------------------------------------------------------------------
      -- Delete
      ----------------------------------------------------------------------------------------------
      procedure Delete (Item_Id : in T_Item_Id) is
         Item_Cursor : Item_Vectors.Cursor := Item_List.First;
      begin
         while Item_Vectors.Has_Element (Item_Cursor) loop
            if Item_Vectors.Element (Item_Cursor).Get_Id = Item_Id then
               Item_List.Delete (Item_Cursor);
            end if;

            Item_Vectors.Next (Item_Cursor);
         end loop;
      end Delete;

      ----------------------------------------------------------------------------------------------
      -- Up_Vote
      ----------------------------------------------------------------------------------------------
      procedure Up_Vote (Item_Id : in T_Item_Id) is
         use type Item_Vectors.Cursor;

         Up_Voted_Item : T_Item;
         Item_Cursor   : Item_Vectors.Cursor := Item_List.First;
      begin
         while Item_Vectors.Has_Element (Item_Cursor)
           and then Item_Vectors.Element (Item_Cursor).Get_Id /= Item_Id
         loop
            Item_Vectors.Next (Item_Cursor);
         end loop;

         if Item_Vectors.Has_Element (Item_Cursor) then
            Up_Voted_Item := Item_Vectors.Element (Item_Cursor);
            Up_Voted_Item.Up_Vote;

            Item_List.Delete (Item_Cursor);

            if Item_List.Is_Empty then
               Item_List.Append (Up_Voted_Item);
            else
               Item_Cursor := Item_List.First;

               while Item_Vectors.Has_Element (Item_Cursor) loop
                  if Item_Vectors.Element (Item_Cursor).Get_Up_Votes <
                    Up_Voted_Item.Get_Up_Votes
                  then
                     Item_List.Insert (Item_Cursor, Up_Voted_Item);
                     Item_Cursor := Item_Vectors.No_Element;
                  elsif Item_Cursor = Item_List.Last then
                     Item_List.Append (Up_Voted_Item);
                     Item_Cursor := Item_Vectors.No_Element;
                  end if;

                  Item_Vectors.Next (Item_Cursor);
               end loop;
            end if;
         end if;
      end Up_Vote;

      ----------------------------------------------------------------------------------------------
      -- Get
      ----------------------------------------------------------------------------------------------
      function Get return Item_Vectors.Vector is (Item_List);

      ----------------------------------------------------------------------------------------------
      -- Iterate
      ----------------------------------------------------------------------------------------------
      procedure Iterate (Process : not null access procedure (Element : in T_Item)) is
         -------------------------------------------------------------------------------------------
         -- Query_Element
         -------------------------------------------------------------------------------------------
         procedure Query_Element (Position : in Item_Vectors.Cursor) is
         begin
            Item_Vectors.Query_Element (Position, Process);
         end Query_Element;
      begin
         Item_List.Iterate (Query_Element'Access);
      end Iterate;

      ----------------------------------------------------------------------------------------------
      -- First_Element
      ----------------------------------------------------------------------------------------------
      function First_Element return T_Item is (Item_List.First_Element);

      ----------------------------------------------------------------------------------------------
      -- Is_Empty
      ----------------------------------------------------------------------------------------------
      function Is_Empty return Boolean is (Item_List.Is_Empty);

   end T_Item_List;

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   function Initialize return T_Item_List is
   begin
      return Item_List : T_Item_List do
         null;
      end return;
   end Initialize;

end Song.Item.List;
