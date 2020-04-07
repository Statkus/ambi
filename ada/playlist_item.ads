with Aws.Session;

with Song;

package Playlist_Item is

   type T_Playlist_Item is tagged private;

   type T_Playlist_Item_Id is mod 2**32;

   package Constructors is

      ----------------------------------------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------------------------------------
      function Initialize
        (Item_Song : in Song.T_Song;
         Id        : in T_Playlist_Item_Id;
         Client_Id : in Aws.Session.Id;
         Up_Votes  : in Natural := Natural'First) return T_Playlist_Item;

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Equality operator
   -------------------------------------------------------------------------------------------------
   function "=" (Left, Right : in T_Playlist_Item) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Get_Song
   -------------------------------------------------------------------------------------------------
   function Get_Song (This : in T_Playlist_Item) return Song.T_Song;

   -------------------------------------------------------------------------------------------------
   -- Get_Id
   -------------------------------------------------------------------------------------------------
   function Get_Id (This : in T_Playlist_Item) return T_Playlist_Item_Id;

   -------------------------------------------------------------------------------------------------
   -- Get_Client_Id
   -------------------------------------------------------------------------------------------------
   function Get_Client_Id (This : in T_Playlist_Item) return Aws.Session.Id;

   -------------------------------------------------------------------------------------------------
   -- Get_Up_Votes
   -------------------------------------------------------------------------------------------------
   function Get_Up_Votes (This : in T_Playlist_Item) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote (This : in out T_Playlist_Item);

private

   type T_Playlist_Item is tagged record
      Item_Song : Song.T_Song;
      Id        : T_Playlist_Item_Id;
      Client_Id : Aws.Session.Id;
      Up_Votes  : Natural := Natural'First;
   end record;

end Playlist_Item;
