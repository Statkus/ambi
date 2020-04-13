with Aws.Client;
with Aws.Response;

package body Web_Methods.Http is

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize return T_Http_Class_Access is (new T_Http);

   -------------------------------------------------------------------------------------------------
   -- Get
   -------------------------------------------------------------------------------------------------
   function Get (This : in out T_Http; Url : in String) return String is
      pragma Unreferenced (This);
   begin
      return Aws.Response.Message_Body (Aws.Client.Get (Url => Url));

   exception
      when others =>
         return "GET request error.";
   end Get;

end Web_Methods.Http;
