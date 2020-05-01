package body Web_Methods.Websocket.Mock is

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize return T_Websocket_Mock_Access is
     (new T_Websocket_Mock'(Room_Request_Called => (others => False)));

   -------------------------------------------------------------------------------------------------
   -- Send_Room_Request
   -------------------------------------------------------------------------------------------------
   procedure Send_Room_Request
     (This      : in out T_Websocket_Mock;
      Room_Name : in     String;
      Request   : in     T_Room_Request)
   is
      pragma Unreferenced (Room_Name);
   begin
      This.Room_Request_Called (Request) := True;
   end Send_Room_Request;

   -------------------------------------------------------------------------------------------------
   -- Reset_Room_Request_Called
   -------------------------------------------------------------------------------------------------
   procedure Reset_Room_Request_Called (This : in out T_Websocket_Mock) is
   begin
      This.Room_Request_Called := (others => False);
   end Reset_Room_Request_Called;

   -------------------------------------------------------------------------------------------------
   -- Is_Room_Request_Called
   -------------------------------------------------------------------------------------------------
   function Is_Room_Request_Called
     (This    : in out T_Websocket_Mock;
      Request : in     T_Room_Request) return Boolean is
     (This.Room_Request_Called (Request));

end Web_Methods.Websocket.Mock;
