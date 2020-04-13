with Ada.Containers.Vectors;

package Song.List is

   package Song_Vectors is new Ada.Containers.Vectors (Natural, Song.T_Song);

   type T_Song_List is new Song_Vectors.Vector with null record;

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   function Initialize return T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Select_First_Song_Not_In_Exclusion_List
   -------------------------------------------------------------------------------------------------
   function Select_First_Song_Not_In_Exclusion_List
     (This           : in T_Song_List;
      Exclusion_List : in T_Song_List) return T_Song;

   -------------------------------------------------------------------------------------------------
   -- Iterate
   -------------------------------------------------------------------------------------------------
   procedure Iterate
     (This    : in T_Song_List;
      Process :    not null access procedure (Element : in T_Song));

   -------------------------------------------------------------------------------------------------
   -- Reverse_Iterate
   -------------------------------------------------------------------------------------------------
   procedure Reverse_Iterate
     (This    : in T_Song_List;
      Process :    not null access procedure (Element : in T_Song));

end Song.List;
