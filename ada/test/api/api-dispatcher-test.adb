with Aunit.Assertions; use Aunit.Assertions;

with Api.Provider.Mock;

package body Api.Dispatcher.Test is

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Dispatcher_Test_Case) is
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
   function Name (This : in T_Dispatcher_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Api.Dispatcher tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_New_And_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);
   begin
      Assert (Api.Dispatcher.New_And_Initialize /= null, "Null dispatcher returned.");
   end Test_New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Song_Search_Results (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Api_Dispatcher : constant T_Dispatcher_Access := new T_Dispatcher;

      Provider_Mock_No_Provider : constant Api.Provider.Mock.T_Mock_Access :=
        new Api.Provider.Mock.T_Mock;
      Provider_Mock_Youtube : constant Api.Provider.Mock.T_Mock_Access :=
        new Api.Provider.Mock.T_Mock;

      Search_Type : T_Search_Type;
   begin
      Api_Dispatcher.Provider_List :=
        (No_Provider_Api => Api.Provider.T_Provider_Class_Access (Provider_Mock_No_Provider),
         Youtube_Api     => Api.Provider.T_Provider_Class_Access (Provider_Mock_Youtube));

      Provider_Mock_No_Provider.Reset_Calls;
      Provider_Mock_Youtube.Reset_Calls;

      Assert
        (Natural
           (Api_Dispatcher.Get_Song_Search_Results (No_Provider_Api, "test", Search_Type).Length) =
         0,
         "Wrong number of songs returned.");

      Assert (Provider_Mock_No_Provider.Is_Get_Song_Search_Results_Called, "Wrong API called.");
      Assert (not Provider_Mock_Youtube.Is_Get_Song_Search_Results_Called, "Wrong API called.");

      Provider_Mock_No_Provider.Reset_Calls;
      Provider_Mock_Youtube.Reset_Calls;

      Assert
        (Natural
           (Api_Dispatcher.Get_Song_Search_Results (Youtube_Api, "test", Search_Type).Length) =
         0,
         "Wrong number of songs returned.");

      Assert (not Provider_Mock_No_Provider.Is_Get_Song_Search_Results_Called, "Wrong API called.");
      Assert (Provider_Mock_Youtube.Is_Get_Song_Search_Results_Called, "Wrong API called.");
   end Test_Get_Song_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Song_Duration (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Api_Dispatcher : constant T_Dispatcher_Access := new T_Dispatcher;

      Provider_Mock_No_Provider : constant Api.Provider.Mock.T_Mock_Access :=
        new Api.Provider.Mock.T_Mock;
      Provider_Mock_Youtube : constant Api.Provider.Mock.T_Mock_Access :=
        new Api.Provider.Mock.T_Mock;

      Source_Song : Song.T_Song :=
        Song.Initialize
          (Id             => "test",
           Title          => "test",
           Thumbnail_Link => "test",
           Provider       => No_Provider_Api);
   begin
      Api_Dispatcher.Provider_List :=
        (No_Provider_Api => Api.Provider.T_Provider_Class_Access (Provider_Mock_No_Provider),
         Youtube_Api     => Api.Provider.T_Provider_Class_Access (Provider_Mock_Youtube));

      Provider_Mock_No_Provider.Reset_Calls;
      Provider_Mock_Youtube.Reset_Calls;

      Assert (Api_Dispatcher.Get_Song_Duration (Source_Song) = 0, "Wrong song duration returned.");

      Assert (Provider_Mock_No_Provider.Is_Get_Song_Duration_Called, "Wrong API called.");
      Assert (not Provider_Mock_Youtube.Is_Get_Song_Duration_Called, "Wrong API called.");

      Provider_Mock_No_Provider.Reset_Calls;
      Provider_Mock_Youtube.Reset_Calls;

      Source_Song :=
        Song.Initialize
          (Id             => "test",
           Title          => "test",
           Thumbnail_Link => "test",
           Provider       => Youtube_Api);

      Assert (Api_Dispatcher.Get_Song_Duration (Source_Song) = 0, "Wrong song duration returned.");

      Assert (not Provider_Mock_No_Provider.Is_Get_Song_Duration_Called, "Wrong API called.");
      Assert (Provider_Mock_Youtube.Is_Get_Song_Duration_Called, "Wrong API called.");
   end Test_Get_Song_Duration;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Related_Songs (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Api_Dispatcher : constant T_Dispatcher_Access := new T_Dispatcher;

      Provider_Mock_No_Provider : constant Api.Provider.Mock.T_Mock_Access :=
        new Api.Provider.Mock.T_Mock;
      Provider_Mock_Youtube : constant Api.Provider.Mock.T_Mock_Access :=
        new Api.Provider.Mock.T_Mock;

      Source_Song : Song.T_Song :=
        Song.Initialize
          (Id             => "test",
           Title          => "test",
           Thumbnail_Link => "test",
           Provider       => No_Provider_Api);
   begin
      Api_Dispatcher.Provider_List :=
        (No_Provider_Api => Api.Provider.T_Provider_Class_Access (Provider_Mock_No_Provider),
         Youtube_Api     => Api.Provider.T_Provider_Class_Access (Provider_Mock_Youtube));

      Provider_Mock_No_Provider.Reset_Calls;
      Provider_Mock_Youtube.Reset_Calls;

      Assert
        (Natural (Api_Dispatcher.Get_Related_Songs (Source_Song).Length) = 0,
         "Wrong number of songs returned.");

      Assert (Provider_Mock_No_Provider.Is_Get_Related_Songs_Called, "Wrong API called.");
      Assert (not Provider_Mock_Youtube.Is_Get_Related_Songs_Called, "Wrong API called.");

      Provider_Mock_No_Provider.Reset_Calls;
      Provider_Mock_Youtube.Reset_Calls;

      Source_Song :=
        Song.Initialize
          (Id             => "test",
           Title          => "test",
           Thumbnail_Link => "test",
           Provider       => Youtube_Api);

      Assert
        (Natural (Api_Dispatcher.Get_Related_Songs (Source_Song).Length) = 0,
         "Wrong number of songs returned.");

      Assert (not Provider_Mock_No_Provider.Is_Get_Related_Songs_Called, "Wrong API called.");
      Assert (Provider_Mock_Youtube.Is_Get_Related_Songs_Called, "Wrong API called.");
   end Test_Get_Related_Songs;

end Api.Dispatcher.Test;
