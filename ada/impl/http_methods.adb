with Aws.Client;
with Aws.Response;

package body Http_Methods is

   function Get (This : in out T_Http_Methods; Url : in String) return String is
      pragma Unreferenced (This);
   begin
      return Aws.Response.Message_Body (Aws.Client.Get (Url => Url));

   exception
      when others =>
         return "GET request error.";
   end Get;

end Http_Methods;
