with Aunit; use Aunit;
with Aunit.Test_Cases;

package Api.Dispatcher.Test is

   type T_Dispatcher_Test_Case is new Aunit.Test_Cases.Test_Case with null record;
   type T_Dispatcher_Test_Case_Access is access all T_Dispatcher_Test_Case;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Dispatcher_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Dispatcher_Test_Case) return Message_String;

   -------------------------------------------------------------------------------------------------
   -- Test_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_New_And_Initialize (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Song_Search_Results (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Song_Duration (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Related_Songs (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

end Api.Dispatcher.Test;
