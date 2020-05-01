with Aunit; use Aunit;
with Aunit.Test_Cases;

with Database.Mock;
with Web_Methods.Websocket.Mock;

package Room.Test is

   type T_Room_Test_Case is new Aunit.Test_Cases.Test_Case with private;
   type T_Room_Test_Case_Access is access all T_Room_Test_Case;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Room_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Room_Test_Case) return Message_String;

   -------------------------------------------------------------------------------------------------
   -- Set_Up
   -------------------------------------------------------------------------------------------------
   procedure Set_Up (This : in out T_Room_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Tear_Down
   -------------------------------------------------------------------------------------------------
   procedure Tear_Down (This : in out T_Room_Test_Case);

   -------------------------------------------------------------------------------------------------
   -- Test_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_New_And_Initialize (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Equality_Operator
   -------------------------------------------------------------------------------------------------
   procedure Test_Equality_Operator (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Add_Client
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_Client (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

   -------------------------------------------------------------------------------------------------
   -- Test_Add_Song_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Test_Add_Song_To_Playlist (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

private

   type T_Room_Test_Case is new Aunit.Test_Cases.Test_Case with record
      Db         : Database.Mock.T_Database_Mock_Access;
      Dispatcher : Api.Dispatcher.T_Dispatcher_Access;
      Websocket  : Web_Methods.Websocket.Mock.T_Websocket_Mock_Access;
   end record;

end Room.Test;
