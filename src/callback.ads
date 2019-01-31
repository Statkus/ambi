with AWS.Response;
with AWS.Status;

package Callback is

   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data;

end Callback;
