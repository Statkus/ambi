with Aunit; use Aunit;
with Aunit.Test_Cases;

package Song.List.Test is

   type T_Song_List_Test_Case is new Aunit.Test_Cases.Test_Case with null record;
   type T_Song_List_Test_Case_Access is access all T_Song_List_Test_Case;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Song_List_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Song_List_Test_Case) return Message_String;

   -------------------------------------------------------------------------------------------------
   -- Test_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Initialize (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Select_First_Songs_Not_In_Exclusion_List
   -------------------------------------------------------------------------------------------------
   procedure Test_Select_First_Songs_Not_In_Exclusion_List
     (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Iterate
   -------------------------------------------------------------------------------------------------
   procedure Test_Iterate (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Reverse_Iterate
   -------------------------------------------------------------------------------------------------
   procedure Test_Reverse_Iterate (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

end Song.List.Test;
