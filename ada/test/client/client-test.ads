with Aunit; use Aunit;
with Aunit.Test_Cases;

package Client.Test is

   type T_Client_Test_Case is new Aunit.Test_Cases.Test_Case with null record;
   type T_Client_Test_Case_Access is access all T_Client_Test_Case;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Client_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Client_Test_Case) return Message_String;

   -------------------------------------------------------------------------------------------------
   -- Test_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_New_And_Initialize (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Equality_Operator
   -------------------------------------------------------------------------------------------------
   procedure Test_Equality_Operator (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Display_Player
   -------------------------------------------------------------------------------------------------
   procedure Test_Display_Player (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Sync_With_Room
   -------------------------------------------------------------------------------------------------
   procedure Test_Sync_With_Room (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Update_Last_Request_Time
   -------------------------------------------------------------------------------------------------
   procedure Test_Update_Last_Request_Time (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Next_Song
   -------------------------------------------------------------------------------------------------
   procedure Test_Next_Song (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Add_Item_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_Item_To_Playlist (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Remove_Item_From_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Test_Remove_Item_From_Playlist (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Up_Vote_Playlist_Item
   -------------------------------------------------------------------------------------------------
   procedure Test_Up_Vote_Playlist_Item (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Getter
   -------------------------------------------------------------------------------------------------
   procedure Test_Getter (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

end Client.Test;
