with GNAT.Exception_Traces;

with Ada.Text_IO; use Ada.Text_IO;

with Aws.Net.Websocket.Registry.Control;
with Aws.Server;
with Aws.Session;

with Callback;
with Database;

----------------------------------------------------------------------------------------------------
-- Ambi
----------------------------------------------------------------------------------------------------
procedure Ambi is
   Ambi_Database : constant Database.T_Database_Class_Access :=
     Database.New_And_Initialize ("ambi.sqlite3");

   Ambi_Server : Aws.Server.Http;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);

   Callback.Initialize (Ambi_Database);

   -- Launch the server on port 80
   Aws.Server.Start
     (Web_Server     => Ambi_Server,
      Name           => "Ambi",
      Callback       => Callback.Callback'Access,
      Port           => 80,
      Max_Connection => 100,
      Session        => True);

   -- Launch Websocket server
   Aws.Net.Websocket.Registry.Control.Start;

   -- Set sessions lifetime to 2 hours
   Aws.Session.Set_Lifetime (7200.0);

   Put_Line ("Press Q key to quit.");
   Aws.Server.Wait (Aws.Server.Q_Key_Pressed);

   Aws.Net.Websocket.Registry.Control.Shutdown;
   Aws.Server.Shutdown (Ambi_Server);

   Ambi_Database.Close;
end Ambi;
