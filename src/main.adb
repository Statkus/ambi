with AWS; use AWS;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   function CB (Request : Status.Data) return Response.Data is
      pragma Unreferenced (Request);
   begin
      return Response.Build ("text/html", "icocou Hello world!");
   end CB;
   TheServer : Server.HTTP;
   ch : Character;
begin
   Server.Start (TheServer, "Rosettacode",
      Callback => CB'Unrestricted_Access, Port => 80);
   Put_Line ("Press any key to quit."); Get_Immediate (ch);
   Server.Shutdown (TheServer);
end Main;
