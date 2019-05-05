package body List is

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote_Playlist_Item
     (Playlist : in out Playlist_Vectors.Vector; Item_ID : in T_Playlist_Item_ID) is
      Item        : T_Playlist_Item;
      Item_Cursor : Playlist_Vectors.Cursor := Playlist.First;
   begin
      while Playlist_Vectors.Has_Element (Item_Cursor)
        and then Playlist_Vectors.Element (Item_Cursor).ID /= Item_ID loop
         Playlist_Vectors.Next (Item_Cursor);
      end loop;

      if Playlist_Vectors.Has_Element (Item_Cursor) then
         Item := Playlist_Vectors.Element (Item_Cursor);
         Item.Up_Votes := Item.Up_Votes + 1;

         Playlist.Delete (Item_Cursor);

         if Playlist.Is_Empty then
            Playlist.Append (Item);
         else
            Item_Cursor := Playlist.First;

            while Playlist_Vectors.Has_Element (Item_Cursor) loop
               if Playlist_Vectors.Element (Item_Cursor).Up_Votes < Item.Up_Votes then
                  Playlist.Insert (Item_Cursor, Item);

                  Item_Cursor := Playlist.Last;
               elsif Playlist_Vectors.To_Index (Item_Cursor)
                 = Playlist_Vectors.To_Index (Playlist.Last) then
                  Playlist.Append (Item);

                  Item_Cursor := Playlist.Last;
               end if;

               Playlist_Vectors.Next (Item_Cursor);
            end loop;
         end if;
      end if;
   end Up_Vote_Playlist_Item;

end List;
