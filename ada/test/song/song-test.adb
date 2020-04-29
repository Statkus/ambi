with Aunit.Assertions; use Aunit.Assertions;

package body Song.Test is

   use type Api.T_Api_Provider;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Song_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine (This, Test_Default_Initialize'Access, "Test default Initialize");
      Register_Routine (This, Test_Initialize'Access, "Test Initialize");
      Register_Routine (This, Test_Equality_Operator'Access, "Test equality operator");
      Register_Routine (This, Test_Getter'Access, "Test getter");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Song_Test_Case) return Test_String is
      pragma Unreferenced (This);
   begin
      return Format ("Song tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_Default_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Default_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      New_Song : constant T_Song := Initialize;
   begin
      Assert (New_Song.Id = Null_Unbounded_String, "Wrong default ID.");
      Assert (New_Song.Title = To_Unbounded_String ("no song played"), "Wrong default title.");
      Assert (New_Song.Thumbnail_Link = Null_Unbounded_String, "Wrong default thumbnail link.");
      Assert (New_Song.Provider = Api.No_Provider_Api, "Wrong default API porvider.");
   end Test_Default_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Id             : constant String             := "test id";
      Title          : constant String             := "test title";
      Thumbnail_Link : constant String             := "test thumbnail link";
      Provider       : constant Api.T_Api_Provider := Api.Youtube_Api;

      New_Song : constant T_Song :=
        Initialize
          (Id             => Id,
           Title          => Title,
           Thumbnail_Link => Thumbnail_Link,
           Provider       => Provider);
   begin
      Assert (New_Song.Id = Id, "Wrong ID.");
      Assert (New_Song.Title = Title, "Wrong title.");
      Assert (New_Song.Thumbnail_Link = Thumbnail_Link, "Wrong thumbnail link.");
      Assert (New_Song.Provider = Provider, "Wrong API porvider.");
   end Test_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Equality_Operator
   -------------------------------------------------------------------------------------------------
   procedure Test_Equality_Operator (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Song_1 : constant T_Song := Initialize;
      Song_2 : T_Song          := Initialize;
   begin
      Assert (Song_1 = Song_2, "Wrong equality operator.");

      Song_2.Id := To_Unbounded_String ("test");

      Assert (Song_1 /= Song_2, "Wrong equality operator.");
   end Test_Equality_Operator;

   -------------------------------------------------------------------------------------------------
   -- Test_Getter
   -------------------------------------------------------------------------------------------------
   procedure Test_Getter (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Id             : constant String             := "test id";
      Title          : constant String             := "test title";
      Thumbnail_Link : constant String             := "test thumbnail link";
      Provider       : constant Api.T_Api_Provider := Api.Youtube_Api;

      New_Song : constant T_Song :=
        Initialize
          (Id             => Id,
           Title          => Title,
           Thumbnail_Link => Thumbnail_Link,
           Provider       => Provider);
   begin
      Assert (New_Song.Get_Id = Id, "Wrong ID.");
      Assert (New_Song.Get_Title = Title, "Wrong title.");
      Assert (New_Song.Get_Thumbnail_Link = Thumbnail_Link, "Wrong thumbnail link.");
      Assert (New_Song.Get_Provider = Provider, "Wrong API porvider.");
   end Test_Getter;

end Song.Test;
