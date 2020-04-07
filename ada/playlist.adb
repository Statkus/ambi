package body Playlist is

   package body Constructors is

      ----------------------------------------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------------------------------------
      function Initialize return T_Playlist is
        (T_Playlist'(Playlist_Item_Vectors.Vector with null record));

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote_Playlist_Item
     (This    : in out T_Playlist;
      Item_Id : in     Playlist_Item.T_Playlist_Item_Id)
   is
      use type Playlist_Item.T_Playlist_Item_Id;

      Item        : Playlist_Item.T_Playlist_Item;
      Item_Cursor : Playlist_Item_Vectors.Cursor := This.First;
   begin
      while Playlist_Item_Vectors.Has_Element (Item_Cursor)
        and then Playlist_Item_Vectors.Element (Item_Cursor).Get_Id /= Item_Id
      loop
         Playlist_Item_Vectors.Next (Item_Cursor);
      end loop;

      if Playlist_Item_Vectors.Has_Element (Item_Cursor) then
         Item := Playlist_Item_Vectors.Element (Item_Cursor);
         Item.Up_Vote;

         This.Delete (Item_Cursor);

         if This.Is_Empty then
            This.Append (Item);
         else
            Item_Cursor := This.First;

            while Playlist_Item_Vectors.Has_Element (Item_Cursor) loop
               if Playlist_Item_Vectors.Element (Item_Cursor).Get_Up_Votes < Item.Get_Up_Votes then
                  This.Insert (Item_Cursor, Item);

                  Item_Cursor := This.Last;
               elsif Playlist_Item_Vectors.To_Index (Item_Cursor) =
                 Playlist_Item_Vectors.To_Index (This.Last)
               then
                  This.Append (Item);

                  Item_Cursor := This.Last;
               end if;

               Playlist_Item_Vectors.Next (Item_Cursor);
            end loop;
         end if;
      end if;
   end Up_Vote_Playlist_Item;

end Playlist;
