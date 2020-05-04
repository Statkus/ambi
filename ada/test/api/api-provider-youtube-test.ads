with Aunit; use Aunit;
with Aunit.Test_Cases;

package Api.Provider.Youtube.Test is

   type T_Youtube_Test_Case is new Aunit.Test_Cases.Test_Case with null record;
   type T_Youtube_Test_Case_Access is access all T_Youtube_Test_Case;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Youtube_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Youtube_Test_Case) return Message_String;

   -------------------------------------------------------------------------------------------------
   -- Test_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_New_And_Initialize (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Song_Search_Results (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Song_Duration (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Related_Songs (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Key
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Key (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Playlist (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Format_Requests
   -------------------------------------------------------------------------------------------------
   procedure Test_Format_Requests (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Request_Response
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Request_Response (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Parse_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   procedure Test_Parse_Video_Search_Results (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Parse_Playlist_Item_Results
   -------------------------------------------------------------------------------------------------
   procedure Test_Parse_Playlist_Item_Results (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Parse_Video_Duration_Result
   -------------------------------------------------------------------------------------------------
   procedure Test_Parse_Video_Duration_Result (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Convert_Iso_8601_Duration_To_Seconds
   -------------------------------------------------------------------------------------------------
   procedure Test_Convert_Iso_8601_Duration_To_Seconds
     (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

end Api.Provider.Youtube.Test;
