with Api.Dispatcher.Test;
with Api.Provider.No_Provider.Test;
with Api.Provider.Youtube.Test;
with Client.Test;
with Client.List.Test;
with Database.Test;
with Song.Test;
with Song.List.Test;
with Song.Item.Test;
with Song.Item.List.Test;

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

      Client_Test : constant Client.Test.T_Client_Test_Case_Access :=
        new Client.Test.T_Client_Test_Case;

      Client_List_Test : constant Client.List.Test.T_Client_List_Test_Case_Access :=
        new Client.List.Test.T_Client_List_Test_Case;

      Database_Test : constant Database.Test.T_Database_Test_Case_Access :=
        new Database.Test.T_Database_Test_Case;

      Song_Test : constant Song.Test.T_Song_Test_Case_Access := new Song.Test.T_Song_Test_Case;

      Song_List_Test : constant Song.List.Test.T_Song_List_Test_Case_Access :=
        new Song.List.Test.T_Song_List_Test_Case;

      Song_Item_Test : constant Song.Item.Test.T_Song_Item_Test_Case_Access :=
        new Song.Item.Test.T_Song_Item_Test_Case;

      Song_Item_List_Test : constant Song.Item.List.Test.T_Song_Item_List_Test_Case_Access :=
        new Song.Item.List.Test.T_Song_Item_List_Test_Case;
   begin
      Result.Add_Test (Api_Dispatcher_Test);
      Result.Add_Test (Api_Provider_No_Provider_Test);
      Result.Add_Test (Api_Provider_Youtube_Test);
      Result.Add_Test (Client_Test);
      Result.Add_Test (Client_List_Test);
      Result.Add_Test (Database_Test);
      Result.Add_Test (Song_Test);
      Result.Add_Test (Song_List_Test);
      Result.Add_Test (Song_Item_Test);
      Result.Add_Test (Song_Item_List_Test);

      return Result;
   end Suite;

end Ambi_Suite;
