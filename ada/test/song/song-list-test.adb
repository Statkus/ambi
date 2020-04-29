with Aunit.Assertions; use Aunit.Assertions;

package body Song.List.Test is

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Song_List_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine (This, Test_Initialize'Access, "Test Initialize");
      Register_Routine
        (This,
         Test_Select_First_Song_Not_In_Exclusion_List'Access,
         "Test Select_First_Song_Not_In_Exclusion_List");
      Register_Routine (This, Test_Iterate'Access, "Test Iterate");
      Register_Routine (This, Test_Reverse_Iterate'Access, "Test Reverse_Iterate");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Song_List_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Song.List tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Song_List : constant T_Song_List := Initialize;
   begin
      Assert (Natural (Song_List.Length) = 0, "Wrong song list length.");
      Assert (Song_List.Is_Empty, "Song list not empty.");
   end Test_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Select_First_Song_Not_In_Exclusion_List
   -------------------------------------------------------------------------------------------------
   procedure Test_Select_First_Song_Not_In_Exclusion_List
     (Test_Case : in out Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Test_Case);

      Song_List      : T_Song_List := Initialize;
      Exclusion_List : T_Song_List := Initialize;
   begin
      Assert
        (Song_List.Select_First_Song_Not_In_Exclusion_List (Exclusion_List) = Initialize,
         "Default song not returned.");

      Song_List.Append
      (Initialize
         (Id             => "test_1",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));

      Song_List.Append
      (Initialize
         (Id             => "test_2",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));

      Song_List.Append
      (Initialize
         (Id             => "test_3",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));

      Assert
        (Song_List.Select_First_Song_Not_In_Exclusion_List (Exclusion_List).Get_Id = "test_1",
         "Wrong song selected.");

      Exclusion_List.Append
      (Initialize
         (Id             => "test_1",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));

      Assert
        (Song_List.Select_First_Song_Not_In_Exclusion_List (Exclusion_List).Get_Id = "test_2",
         "Wrong song selected.");

      Exclusion_List.Append
      (Initialize
         (Id             => "test_2",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));

      Assert
        (Song_List.Select_First_Song_Not_In_Exclusion_List (Exclusion_List).Get_Id = "test_3",
         "Wrong song selected.");

      Exclusion_List.Append
      (Initialize
         (Id             => "test_3",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));

      Assert
        (Song_List.Select_First_Song_Not_In_Exclusion_List (Exclusion_List).Get_Id = "test_1",
         "Wrong song selected.");
   end Test_Select_First_Song_Not_In_Exclusion_List;

   -------------------------------------------------------------------------------------------------
   -- Test_Iterate
   -------------------------------------------------------------------------------------------------
   procedure Test_Iterate (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Id_List_String : Unbounded_String := Null_Unbounded_String;

      ----------------------------------------------------------------------------------------------
      -- Test_Iterate_Procedure
      ----------------------------------------------------------------------------------------------
      procedure Test_Iterate_Procedure (Element : in T_Song) is
      begin
         Append (Id_List_String, Element.Get_Id);
      end Test_Iterate_Procedure;

      Song_List : T_Song_List := Initialize;
   begin
      Song_List.Iterate (Test_Iterate_Procedure'Access);

      Assert (Id_List_String = Null_Unbounded_String, "Wrong ID list string.");

      Song_List.Append
      (Initialize
         (Id             => "test_1",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));
      Song_List.Append
      (Initialize
         (Id             => "test_2",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));
      Song_List.Append
      (Initialize
         (Id             => "test_3",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));
      Assert (Natural (Song_List.Length) = 3, "Wrong song list length.");

      Song_List.Iterate (Test_Iterate_Procedure'Access);

      Assert (To_String (Id_List_String) = "test_1test_2test_3", "Wrong ID list string.");
   end Test_Iterate;

   -------------------------------------------------------------------------------------------------
   -- Test_Reverse_Iterate
   -------------------------------------------------------------------------------------------------
   procedure Test_Reverse_Iterate (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Id_List_String : Unbounded_String := Null_Unbounded_String;

      ----------------------------------------------------------------------------------------------
      -- Test_Reverse_Iterate_Procedure
      ----------------------------------------------------------------------------------------------
      procedure Test_Reverse_Iterate_Procedure (Element : in T_Song) is
      begin
         Append (Id_List_String, Element.Get_Id);
      end Test_Reverse_Iterate_Procedure;

      Song_List : T_Song_List := Initialize;
   begin
      Song_List.Reverse_Iterate (Test_Reverse_Iterate_Procedure'Access);

      Assert (Id_List_String = Null_Unbounded_String, "Wrong ID list string.");

      Song_List.Append
      (Initialize
         (Id             => "test_1",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));
      Song_List.Append
      (Initialize
         (Id             => "test_2",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));
      Song_List.Append
      (Initialize
         (Id             => "test_3",
          Title          => "",
          Thumbnail_Link => "",
          Provider       => Api.No_Provider_Api));
      Assert (Natural (Song_List.Length) = 3, "Wrong song list length.");

      Song_List.Reverse_Iterate (Test_Reverse_Iterate_Procedure'Access);

      Assert (To_String (Id_List_String) = "test_3test_2test_1", "Wrong ID list string.");
   end Test_Reverse_Iterate;

end Song.List.Test;
