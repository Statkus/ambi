with Aunit; use Aunit;
with Aunit.Test_Cases;

package Database.Test is

   type T_Database_Test_Case is new Aunit.Test_Cases.Test_Case with null record;
   type T_Database_Test_Case_Access is access all T_Database_Test_Case;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Database_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Database_Test_Case) return Message_String;

   -------------------------------------------------------------------------------------------------
   -- Test_Constructors_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Constructors_New_And_Initialize
     (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Close
   -------------------------------------------------------------------------------------------------
   procedure Test_Close (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Add_To_Rooms
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_To_Rooms (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Add_To_Room_Historic
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_To_Room_Historic (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Add_To_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_To_Room_Likes (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Remove_From_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Test_Remove_From_Room_Likes (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Rooms
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Rooms (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Room_Historic
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Room_Historic (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Room_Last_Songs
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Room_Last_Songs (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Room_Likes
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Room_Likes (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Is_Room_Song_Liked
   -------------------------------------------------------------------------------------------------
   procedure Test_Is_Room_Song_Liked (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Read_Rooms_In_Db
   -------------------------------------------------------------------------------------------------
   procedure Test_Read_Rooms_In_Db (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

end Database.Test;
