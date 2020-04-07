with Yt_Api.Test;

package body Ambi_Suite is

   --  Statically allocate test cases:
   --Test_1 : aliased Test_Case_1.T_Test_Case_1;

   function Suite return Aunit.Test_Suites.Access_Test_Suite is
      Result : constant Aunit.Test_Suites.Access_Test_Suite := Aunit.Test_Suites.New_Suite;

      Yt_Api_Test : constant Yt_Api.Test.T_Yt_Api_Test_Case_Access := new Yt_Api.Test.T_Yt_Api_Test_Case;
   begin
      Result.Add_Test (Yt_Api_Test);

      return Result;
   end Suite;

end Ambi_Suite;
