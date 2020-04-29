with Aunit.Assertions; use Aunit.Assertions;

package body Song.Item.List.Test is

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Song_Item_List_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine (This, Test_Initialize'Access, "Test Initialize");
      Register_Routine (This, Test_Set'Access, "Test Set");
      Register_Routine (This, Test_Append'Access, "Test Append");
      Register_Routine (This, Test_Delete_First'Access, "Test Delete_First");
      Register_Routine (This, Test_Delete'Access, "Test Delete");
      Register_Routine (This, Test_Up_Vote'Access, "Test Up_Vote");
      Register_Routine (This, Test_Get'Access, "Test Get");
      Register_Routine (This, Test_Iterate'Access, "Test Iterate");
      Register_Routine (This, Test_First_Element'Access, "Test First_Element");
      Register_Routine (This, Test_Is_Empty'Access, "Test Is_Empty");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Song_Item_List_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Song.Item.List tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_List : constant T_Item_List := Initialize;
   begin
      Assert (Item_List.Is_Empty, "Item list not empty.");
   end Test_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Set
   -------------------------------------------------------------------------------------------------
   procedure Test_Set (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_List : T_Item_List := Initialize;

      Item_Vector : Item_Vectors.Vector := Item_Vectors.Empty_Vector;
      Item_1      : constant T_Item     :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Item_Vector.Append (Item_1);
      Item_List.Set (Item_Vector);

      Assert (not Item_List.Is_Empty, "Item list empty.");
      Assert (Item_List.First_Element = Item_1, "Wrong first item.");
   end Test_Set;

   -------------------------------------------------------------------------------------------------
   -- Test_Append
   -------------------------------------------------------------------------------------------------
   procedure Test_Append (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_List : T_Item_List := Initialize;

      Item_1 : constant T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Item_List.Append (Item_1);

      Assert (not Item_List.Is_Empty, "Item list empty.");
      Assert (Item_List.First_Element = Item_1, "Wrong first item.");
   end Test_Append;

   -------------------------------------------------------------------------------------------------
   -- Test_Delete_First
   -------------------------------------------------------------------------------------------------
   procedure Test_Delete_First (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_List : T_Item_List := Initialize;

      Item_1 : constant T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
      Item_2 : constant T_Item :=
        Initialize (Id => 2, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Item_List.Delete_First;
      Assert (Item_List.Is_Empty, "Item list not empty.");

      Item_List.Append (Item_1);
      Item_List.Append (Item_2);
      Assert (Natural (Item_List.Get.Length) = 2, "Wrong item list length.");

      Item_List.Delete_First;

      Assert (Natural (Item_List.Get.Length) = 1, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 2, "Wrong first element ID.");

      Item_List.Delete_First;

      Assert (Natural (Item_List.Get.Length) = 0, "Wrong item list length.");
      Assert (Item_List.Is_Empty, "Item list not empty.");
   end Test_Delete_First;

   -------------------------------------------------------------------------------------------------
   -- Test_Delete
   -------------------------------------------------------------------------------------------------
   procedure Test_Delete (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_List : T_Item_List := Initialize;

      Item_1 : constant T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
      Item_2 : constant T_Item :=
        Initialize (Id => 2, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
      Item_3 : constant T_Item :=
        Initialize (Id => 3, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Item_List.Delete (T_Item_Id'First);
      Assert (Item_List.Is_Empty, "Item list not empty.");

      Item_List.Append (Item_1);
      Item_List.Append (Item_2);
      Item_List.Append (Item_3);
      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");

      Item_List.Delete (T_Item_Id'First);

      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 1, "Wrong first element ID.");
      Assert (Item_List.Get.Last_Element.Get_Id = 3, "Wrong last element ID.");

      Item_List.Delete (T_Item_Id (2));

      Assert (Natural (Item_List.Get.Length) = 2, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 1, "Wrong first element ID.");
      Assert (Item_List.Get.Last_Element.Get_Id = 3, "Wrong last element ID.");

      Item_List.Delete (T_Item_Id (3));

      Assert (Natural (Item_List.Get.Length) = 1, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 1, "Wrong first element ID.");

      Item_List.Delete (T_Item_Id (1));

      Assert (Natural (Item_List.Get.Length) = 0, "Wrong item list length.");
      Assert (Item_List.Is_Empty, "Item list not empty.");
   end Test_Delete;

   -------------------------------------------------------------------------------------------------
   -- Test_Up_Vote
   -------------------------------------------------------------------------------------------------
   procedure Test_Up_Vote (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_List : T_Item_List := Initialize;

      Item_1 : constant T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
      Item_2 : constant T_Item :=
        Initialize (Id => 2, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
      Item_3 : constant T_Item :=
        Initialize (Id => 3, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Item_List.Up_Vote (T_Item_Id'First);
      Assert (Item_List.Is_Empty, "Item list not empty.");

      Item_List.Append (Item_1);
      Item_List.Append (Item_2);
      Item_List.Append (Item_3);
      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");

      Item_List.Up_Vote (T_Item_Id'First);

      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 1, "Wrong first element ID.");
      Assert (Item_List.Get.Last_Element.Get_Id = 3, "Wrong last element ID.");
      Assert (Item_List.First_Element.Get_Up_Votes = 0, "Wrong first element up votes.");
      Assert (Item_List.Get.Last_Element.Get_Up_Votes = 0, "Wrong first element up votes.");

      Item_List.Up_Vote (1);

      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 1, "Wrong first element ID.");
      Assert (Item_List.Get.Last_Element.Get_Id = 3, "Wrong last element ID.");
      Assert (Item_List.First_Element.Get_Up_Votes = 1, "Wrong first element up votes.");
      Assert (Item_List.Get.Last_Element.Get_Up_Votes = 0, "Wrong first element up votes.");

      Item_List.Up_Vote (2);

      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 1, "Wrong first element ID.");
      Assert (Item_List.Get.Last_Element.Get_Id = 3, "Wrong last element ID.");
      Assert (Item_List.First_Element.Get_Up_Votes = 1, "Wrong first element up votes.");
      Assert (Item_List.Get.Last_Element.Get_Up_Votes = 0, "Wrong first element up votes.");

      Item_List.Up_Vote (3);

      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 1, "Wrong first element ID.");
      Assert (Item_List.Get.Last_Element.Get_Id = 3, "Wrong last element ID.");
      Assert (Item_List.First_Element.Get_Up_Votes = 1, "Wrong first element up votes.");
      Assert (Item_List.Get.Last_Element.Get_Up_Votes = 1, "Wrong first element up votes.");

      Item_List.Up_Vote (2);

      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 2, "Wrong first element ID.");
      Assert (Item_List.Get.Last_Element.Get_Id = 3, "Wrong last element ID.");
      Assert (Item_List.First_Element.Get_Up_Votes = 2, "Wrong first element up votes.");
      Assert (Item_List.Get.Last_Element.Get_Up_Votes = 1, "Wrong first element up votes.");

      Item_List.Up_Vote (3);

      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 2, "Wrong first element ID.");
      Assert (Item_List.Get.Last_Element.Get_Id = 1, "Wrong last element ID.");
      Assert (Item_List.First_Element.Get_Up_Votes = 2, "Wrong first element up votes.");
      Assert (Item_List.Get.Last_Element.Get_Up_Votes = 1, "Wrong first element up votes.");

      Item_List.Up_Vote (1);

      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 2, "Wrong first element ID.");
      Assert (Item_List.Get.Last_Element.Get_Id = 1, "Wrong last element ID.");
      Assert (Item_List.First_Element.Get_Up_Votes = 2, "Wrong first element up votes.");
      Assert (Item_List.Get.Last_Element.Get_Up_Votes = 2, "Wrong first element up votes.");

      Item_List.Up_Vote (1);

      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");
      Assert (Item_List.First_Element.Get_Id = 1, "Wrong first element ID.");
      Assert (Item_List.Get.Last_Element.Get_Id = 3, "Wrong last element ID.");
      Assert (Item_List.First_Element.Get_Up_Votes = 3, "Wrong first element up votes.");
      Assert (Item_List.Get.Last_Element.Get_Up_Votes = 2, "Wrong first element up votes.");
   end Test_Up_Vote;

   -------------------------------------------------------------------------------------------------
   -- Test_Get
   -------------------------------------------------------------------------------------------------
   procedure Test_Get (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_List : T_Item_List := Initialize;

      Item_1 : constant T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Item_List.Append (Item_1);
      Assert (Natural (Item_List.Get.Length) = 1, "Wrong item list length.");
      Assert (Item_List.Get.First_Element = Item_1, "Wrong first item.");
   end Test_Get;

   -------------------------------------------------------------------------------------------------
   -- Test_Iterate
   -------------------------------------------------------------------------------------------------
   procedure Test_Iterate (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Id_Sum : Natural := Natural'First;

      ----------------------------------------------------------------------------------------------
      -- Test_Iterate_Procedure
      ----------------------------------------------------------------------------------------------
      procedure Test_Iterate_Procedure (Element : in T_Item) is
      begin
         Id_Sum := Id_Sum + Natural (Element.Get_Id);
      end Test_Iterate_Procedure;

      Item_List : T_Item_List := Initialize;

      Item_1 : constant T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
      Item_2 : constant T_Item :=
        Initialize (Id => 2, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
      Item_3 : constant T_Item :=
        Initialize (Id => 3, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Item_List.Iterate (Test_Iterate_Procedure'Access);

      Assert (Id_Sum = 0, "Wrong ID sum.");

      Item_List.Append (Item_1);
      Item_List.Append (Item_2);
      Item_List.Append (Item_3);
      Assert (Natural (Item_List.Get.Length) = 3, "Wrong item list length.");

      Item_List.Iterate (Test_Iterate_Procedure'Access);

      Assert (Id_Sum = 6, "Wrong ID sum.");
   end Test_Iterate;

   -------------------------------------------------------------------------------------------------
   -- Test_First_Element
   -------------------------------------------------------------------------------------------------
   procedure Test_First_Element (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_List : T_Item_List := Initialize;

      Item_1 : constant T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Item_List.Append (Item_1);
      Assert (Natural (Item_List.Get.Length) = 1, "Wrong item list length.");
      Assert (Item_List.First_Element = Item_1, "Wrong first item.");
   end Test_First_Element;

   -------------------------------------------------------------------------------------------------
   -- Test_Is_Empty
   -------------------------------------------------------------------------------------------------
   procedure Test_Is_Empty (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_List : T_Item_List := Initialize;

      Item_1 : constant T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Assert (Item_List.Is_Empty, "Item list not empty.");
      Item_List.Append (Item_1);
      Assert (Natural (Item_List.Get.Length) = 1, "Wrong item list length.");
      Assert (not Item_List.Is_Empty, "Item list empty.");
   end Test_Is_Empty;

end Song.Item.List.Test;
