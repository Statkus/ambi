with Aunit; use Aunit;
with Aunit.Test_Cases;

package Room_Name_List.Test is

   type T_Room_Name_List_Test_Case is new Aunit.Test_Cases.Test_Case with null record;
   type T_Room_Name_List_Test_Case_Access is access all T_Room_Name_List_Test_Case;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Room_Name_List_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Room_Name_List_Test_Case) return Message_String;

   -------------------------------------------------------------------------------------------------
   -- Test_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Initialize (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

end Room_Name_List.Test;
