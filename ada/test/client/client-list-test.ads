with Aunit; use Aunit;
with Aunit.Test_Cases;

package Client.List.Test is

   type T_Client_List_Test_Case is new Aunit.Test_Cases.Test_Case with null record;
   type T_Client_List_Test_Case_Access is access all T_Client_List_Test_Case;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Client_List_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Client_List_Test_Case) return Message_String;

   -------------------------------------------------------------------------------------------------
   -- Test_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_Initialize (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Append
   -------------------------------------------------------------------------------------------------
   procedure Test_Append (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Remove_Disconnected_Client
   -------------------------------------------------------------------------------------------------
   procedure Test_Remove_Disconnected_Client (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

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
   -- Test_Length
   -------------------------------------------------------------------------------------------------
   procedure Test_Length (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Client
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Client (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Is_Auto_Playback_Requested
   -------------------------------------------------------------------------------------------------
   procedure Test_Is_Auto_Playback_Requested (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

end Client.List.Test;
