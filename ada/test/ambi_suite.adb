with Api.Dispatcher.Test;
with Api.Provider.No_Provider.Test;
with Api.Provider.Youtube.Test;

package body Ambi_Suite is

   -------------------------------------------------------------------------------------------------
   -- Suite
   -------------------------------------------------------------------------------------------------
   function Suite return Aunit.Test_Suites.Access_Test_Suite is
      Result : constant Aunit.Test_Suites.Access_Test_Suite := Aunit.Test_Suites.New_Suite;

      Api_Dispatcher_Test : constant Api.Dispatcher.Test.T_Dispatcher_Test_Case_Access :=
        new Api.Dispatcher.Test.T_Dispatcher_Test_Case;

      Api_Provider_No_Provider_Test : constant Api.Provider.No_Provider.Test
        .T_No_Provider_Test_Case_Access :=
        new Api.Provider.No_Provider.Test.T_No_Provider_Test_Case;

      Api_Provider_Youtube_Test : constant Api.Provider.Youtube.Test.T_Youtube_Test_Case_Access :=
        new Api.Provider.Youtube.Test.T_Youtube_Test_Case;
   begin
      Result.Add_Test (Api_Dispatcher_Test);
      Result.Add_Test (Api_Provider_No_Provider_Test);
      Result.Add_Test (Api_Provider_Youtube_Test);

      return Result;
   end Suite;

end Ambi_Suite;
