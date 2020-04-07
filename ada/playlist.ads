with Ada.Containers.Vectors;

with Playlist_Item;

package Playlist is

   use type Playlist_Item.T_Playlist_Item;

   package Playlist_Item_Vectors is new Ada.Containers.Vectors
     (Natural,
      Playlist_Item.T_Playlist_Item);

   type T_Playlist is new Playlist_Item_Vectors.Vector with null record;

   package Constructors is

      ----------------------------------------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------------------------------------
      function Initialize return T_Playlist;

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote_Playlist_Item
     (This    : in out T_Playlist;
      Item_Id : in     Playlist_Item.T_Playlist_Item_Id);

end Playlist;
