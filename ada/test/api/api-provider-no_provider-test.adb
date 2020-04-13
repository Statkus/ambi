with Aunit.Assertions; use Aunit.Assertions;

package body Api.Provider.No_Provider.Test is

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_No_Provider_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine (This, Test_New_And_Initialize'Access, "Test New_And_Initialize");
      Register_Routine (This, Test_Get_Song_Search_Results'Access, "Test Get_Song_Search_Results");
      Register_Routine (This, Test_Get_Song_Duration'Access, "Test Get_Song_Duration");
      Register_Routine (This, Test_Get_Related_Songs'Access, "Test Get_Related_Songs");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_No_Provider_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Api.Provider.No_Provider tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_New_And_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);
   begin
      Assert (New_And_Initialize /= null, "Null provider given.");
   end Test_New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Song_Search_Results (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      No_Api      : constant T_No_Provider_Access := New_And_Initialize;
      Search_Type : T_Search_Type;
   begin
      Assert
        (Natural (No_Api.Get_Song_Search_Results ("test", Search_Type).Length) = 0,
         "Wrong number of songs returned.");
   end Test_Get_Song_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Song_Duration (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      No_Api      : constant T_No_Provider_Access := New_And_Initialize;
      Source_Song : constant Song.T_Song          := Song.Initialize;
   begin
      Assert (No_Api.Get_Song_Duration (Source_Song) = 0, "Wrong song duration returned.");
   end Test_Get_Song_Duration;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Related_Songs (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      No_Api      : constant T_No_Provider_Access := New_And_Initialize;
      Source_Song : constant Song.T_Song          := Song.Initialize;
   begin
      Assert
        (Natural (No_Api.Get_Related_Songs (Source_Song).Length) = 0,
         "Wrong number of songs returned.");
   end Test_Get_Related_Songs;

end Api.Provider.No_Provider.Test;
