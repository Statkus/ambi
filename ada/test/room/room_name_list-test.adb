with Aunit.Assertions; use Aunit.Assertions;

package body Room_Name_List.Test is

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Room_Name_List_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine (This, Test_Initialize'Access, "Test Initialize");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Room_Name_List_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Room_Name_List tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      New_Room_Name_List : constant T_Room_Name_List := Initialize;
   begin
      Assert (Natural (New_Room_Name_List.Length) = 0, "Wrong room name list length.");
      Assert (New_Room_Name_List.Is_Empty, "Room name list not empty.");
   end Test_Initialize;

end Room_Name_List.Test;
