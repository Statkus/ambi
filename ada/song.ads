with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Api_Provider;

package Song is

   type T_Song is tagged private;

   package Constructors is

      ----------------------------------------------------------------------------------------------
      -- Initialize (default constructor)
      ----------------------------------------------------------------------------------------------
      function Initialize return T_Song;

      ----------------------------------------------------------------------------------------------
      -- Initialize
      ----------------------------------------------------------------------------------------------
      function Initialize
        (Id             : in String;
         Title          : in String;
         Thumbnail_Link : in String;
         Provider       : in Api_Provider.T_Api_Provider) return T_Song;

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Equality operator
   -------------------------------------------------------------------------------------------------
   function "=" (Left, Right : in T_Song) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Get_Id
   -------------------------------------------------------------------------------------------------
   function Get_Id (This : in T_Song) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_Title
   -------------------------------------------------------------------------------------------------
   function Get_Title (This : in T_Song) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_Thumbnail_Link
   -------------------------------------------------------------------------------------------------
   function Get_Thumbnail_Link (This : in T_Song) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_Provider
   -------------------------------------------------------------------------------------------------
   function Get_Provider (This : in T_Song) return Api_Provider.T_Api_Provider;

private

   type T_Song is tagged record
      Id             : Unbounded_String;
      Title          : Unbounded_String;
      Thumbnail_Link : Unbounded_String;
      Provider       : Api_Provider.T_Api_Provider;
   end record;

end Song;
