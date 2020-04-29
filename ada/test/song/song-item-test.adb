with Aunit.Assertions; use Aunit.Assertions;

package body Song.Item.Test is

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Song_Item_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine (This, Test_Initialize'Access, "Test Initialize");
      Register_Routine (This, Test_Equality_Operator'Access, "Test equality operator");
      Register_Routine (This, Test_Getter'Access, "Test getter");
      Register_Routine (This, Test_Up_Vote'Access, "Test Up_Vote");
      Register_Routine (This, Test_Next'Access, "Test Next");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Song_Item_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Song.Item tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use type Aws.Session.Id;

      New_Item : T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Assert (New_Item.Id = 1, "Wrong ID.");
      Assert (New_Item.Item_Song = Initialize, "Wrong song.");
      Assert (New_Item.Client_Id = Aws.Session.No_Session, "Wrong client ID.");
      Assert (New_Item.Up_Votes = 0, "Wrong up votes.");

      New_Item :=
        Initialize
          (Id        => 2,
           Item_Song => Initialize,
           Client_Id => Aws.Session.No_Session,
           Up_Votes  => 3);

      Assert (New_Item.Id = 2, "Wrong ID.");
      Assert (New_Item.Item_Song = Initialize, "Wrong song.");
      Assert (New_Item.Client_Id = Aws.Session.No_Session, "Wrong client ID.");
      Assert (New_Item.Up_Votes = 3, "Wrong up votes.");
   end Test_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Equality_Operator
   -------------------------------------------------------------------------------------------------
   procedure Test_Equality_Operator (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_1 : constant T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
      Item_2 : T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Assert (Item_1 = Item_2, "Wrong equality operator.");

      Item_2.Id := 2;

      Assert (Item_1 /= Item_2, "Wrong equality operator.");
   end Test_Equality_Operator;

   -------------------------------------------------------------------------------------------------
   -- Test_Getter
   -------------------------------------------------------------------------------------------------
   procedure Test_Getter (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      use type Aws.Session.Id;

      New_Item : constant T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      Assert (New_Item.Get_Id = 1, "Wrong ID.");
      Assert (New_Item.Get_Song = Initialize, "Wrong song.");
      Assert (New_Item.Get_Client_Id = Aws.Session.No_Session, "Wrong client ID.");
      Assert (New_Item.Get_Up_Votes = 0, "Wrong up votes.");
   end Test_Getter;

   -------------------------------------------------------------------------------------------------
   -- Test_Up_Vote
   -------------------------------------------------------------------------------------------------
   procedure Test_Up_Vote (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      New_Item : T_Item :=
        Initialize (Id => 1, Item_Song => Initialize, Client_Id => Aws.Session.No_Session);
   begin
      New_Item.Up_Vote;
      Assert (New_Item.Up_Votes = 1, "Wrong up votes.");
      New_Item.Up_Vote;
      Assert (New_Item.Up_Votes = 2, "Wrong up votes.");
   end Test_Up_Vote;

   -------------------------------------------------------------------------------------------------
   -- Test_Next
   -------------------------------------------------------------------------------------------------
   procedure Test_Next (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Item_Id : T_Item_Id := T_Item_Id'First;
   begin
      Next (Item_Id);
      Assert (Item_Id = 1, "Wrong item ID.");
      Next (Item_Id);
      Assert (Item_Id = 2, "Wrong item ID.");
      Item_Id := T_Item_Id'Last;
      Next (Item_Id);
      Assert (Item_Id = T_Item_Id'First, "Wrong item ID.");
   end Test_Next;

end Song.Item.Test;
