package body Song is

   package body Constructors is

      ----------------------------------------------------------------------------------------------
      -- Initialize (default constructor)
      ----------------------------------------------------------------------------------------------
      function Initialize return T_Song is
        (T_Song'
           (Id             => Null_Unbounded_String,
            Title          => To_Unbounded_String ("no song played"),
            Thumbnail_Link => Null_Unbounded_String,
            Provider       => Api.No_Provider_Api));

      ----------------------------------------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------------------------------------
      function Initialize
        (Id             : in String;
         Title          : in String;
         Thumbnail_Link : in String;
         Provider       : in Api.T_Api_Provider) return T_Song is
        (T_Song'
           (Id             => To_Unbounded_String (Id),
            Title          => To_Unbounded_String (Title),
            Thumbnail_Link => To_Unbounded_String (Thumbnail_Link),
            Provider       => Provider));

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Equality operator
   -------------------------------------------------------------------------------------------------
   function "=" (Left, Right : in T_Song) return Boolean is (Left.Id = Right.Id);

   -------------------------------------------------------------------------------------------------
   -- Get_Id
   -------------------------------------------------------------------------------------------------
   function Get_Id (This : in T_Song) return String is (To_String (This.Id));

   -------------------------------------------------------------------------------------------------
   -- Get_Title
   -------------------------------------------------------------------------------------------------
   function Get_Title (This : in T_Song) return String is (To_String (This.Title));

   -------------------------------------------------------------------------------------------------
   -- Get_Thumbnail_Link
   -------------------------------------------------------------------------------------------------
   function Get_Thumbnail_Link
     (This : in T_Song) return String is
     (To_String (This.Thumbnail_Link));

   -------------------------------------------------------------------------------------------------
   -- Get_Provider
   -------------------------------------------------------------------------------------------------
   function Get_Provider (This : in T_Song) return Api.T_Api_Provider is (This.Provider);

end Song;
