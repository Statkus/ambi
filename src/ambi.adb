with AWS.Server;

with Callback;

with Ada.Text_IO; use Ada.Text_IO;

with Templates_Parser;

procedure Ambi is
   Ambi_Server : AWS.Server.HTTP;
   Ch : Character;
begin
   AWS.Server.Start
     (Web_Server => Ambi_Server,
      Name       => "Ambi",
      Callback   => Callback.Ambi_Callback'Access,
      Port       => 8080);

   Put_Line ("Press any key to quit.");
   Get_Immediate (ch);

   AWS.Server.Shutdown (Ambi_Server);
end Ambi;
