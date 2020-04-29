with Aunit; use Aunit;
with Aunit.Test_Cases;

package Song.Item.Test is

   type T_Song_Item_Test_Case is new Aunit.Test_Cases.Test_Case with null record;
   type T_Song_Item_Test_Case_Access is access all T_Song_Item_Test_Case;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Song_Item_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Song_Item_Test_Case) return Message_String;

   -------------------------------------------------------------------------------------------------
   -- Test_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Initialize (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Equality_Operator
   -------------------------------------------------------------------------------------------------
   procedure Test_Equality_Operator (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Getter
   -------------------------------------------------------------------------------------------------
   procedure Test_Getter (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Up_Vote
   -------------------------------------------------------------------------------------------------
   procedure Test_Up_Vote (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Next
   -------------------------------------------------------------------------------------------------
   procedure Test_Next (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

end Song.Item.Test;
