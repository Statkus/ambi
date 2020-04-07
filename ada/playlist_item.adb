package body Playlist_Item is

   package body Constructors is

      ----------------------------------------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------------------------------------
      function Initialize
        (Item_Song : in Song.T_Song;
         Id        : in T_Playlist_Item_Id;
         Client_Id : in Aws.Session.Id;
         Up_Votes  : in Natural := Natural'First) return T_Playlist_Item is
        (T_Playlist_Item'
           (Item_Song => Item_Song, Id => Id, Client_Id => Client_Id, Up_Votes => Up_Votes));

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Equality operator
   -------------------------------------------------------------------------------------------------
   function "=" (Left, Right : in T_Playlist_Item) return Boolean is (Left.Id = Right.Id);

   -------------------------------------------------------------------------------------------------
   -- Get_Song
   -------------------------------------------------------------------------------------------------
   function Get_Song (This : in T_Playlist_Item) return Song.T_Song is (This.Item_Song);

   -------------------------------------------------------------------------------------------------
   -- Get_Id
   -------------------------------------------------------------------------------------------------
   function Get_Id (This : in T_Playlist_Item) return T_Playlist_Item_Id is (This.Id);

   -------------------------------------------------------------------------------------------------
   -- Get_Client_Id
   -------------------------------------------------------------------------------------------------
   function Get_Client_Id (This : in T_Playlist_Item) return Aws.Session.Id is (This.Client_Id);

   -------------------------------------------------------------------------------------------------
   -- Get_Up_Votes
   -------------------------------------------------------------------------------------------------
   function Get_Up_Votes (This : in T_Playlist_Item) return Natural is (This.Up_Votes);

   -------------------------------------------------------------------------------------------------
   -- Up_Vote
   -------------------------------------------------------------------------------------------------
   procedure Up_Vote (This : in out T_Playlist_Item) is
   begin
      This.Up_Votes := This.Up_Votes + 1;
   end Up_Vote;

end Playlist_Item;
