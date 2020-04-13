with Ada.Characters.Handling;

with Aws.Net.Websocket.Registry;

package body Web_Methods.Websocket is

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize return T_Websocket_Class_Access is (new T_Websocket);

   -------------------------------------------------------------------------------------------------
   -- Send_Room_Request
   -------------------------------------------------------------------------------------------------
   procedure Send_Room_Request
     (This      : in out T_Websocket;
      Room_Name : in     String;
      Request   : in     T_Room_Request)
   is
      pragma Unreferenced (This);

      Rcp : constant Aws.Net.Websocket.Registry.Recipient :=
        Aws.Net.Websocket.Registry.Create (Uri => "/" & Room_Name & "Socket");
   begin
      Aws.Net.Websocket.Registry.Send (Rcp, Ada.Characters.Handling.To_Lower (Request'Img));

   exception
      when others =>
         null;
   end Send_Room_Request;

end Web_Methods.Websocket;
