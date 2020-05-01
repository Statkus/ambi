package Web_Methods.Websocket.Mock is

   type T_Websocket_Mock is new T_Websocket with private;
   type T_Websocket_Mock_Access is access all T_Websocket_Mock;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize return T_Websocket_Mock_Access;

   -------------------------------------------------------------------------------------------------
   -- Send_Room_Request
   -------------------------------------------------------------------------------------------------
   procedure Send_Room_Request
     (This      : in out T_Websocket_Mock;
      Room_Name : in     String;
      Request   : in     T_Room_Request);

   -------------------------------------------------------------------------------------------------
   -- Reset_Room_Request_Called
   -------------------------------------------------------------------------------------------------
   procedure Reset_Room_Request_Called (This : in out T_Websocket_Mock);

   -------------------------------------------------------------------------------------------------
   -- Is_Room_Request_Called
   -------------------------------------------------------------------------------------------------
   function Is_Room_Request_Called
     (This    : in out T_Websocket_Mock;
      Request : in     T_Room_Request) return Boolean;

private

   type T_Room_Request_Called is array (T_Room_Request) of Boolean;

   type T_Websocket_Mock is new T_Websocket with record
      Room_Request_Called : T_Room_Request_Called;
   end record;

end Web_Methods.Websocket.Mock;
