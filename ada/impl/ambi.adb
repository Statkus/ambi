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
   Config_File   : File_Type;
   Ambi_Database : constant Database.T_Database_Access :=
     Database.Constructors.New_And_Initialize ("ambi.sqlite3");
   Ambi_Server : Aws.Server.Http;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);

   -- Read server address
   Open (File => Config_File, Mode => In_File, Name => "server_address.txt");
   Callback.Set_Server_Address (Get_Line (Config_File));
   Close (Config_File);

   Callback.Set_Database (Ambi_Database);

   -- Launch the server on port 80
   Aws.Server.Start
     (Web_Server     => Ambi_Server,
      Name           => "Ambi",
      Callback       => Callback.Ambi_Callback'Access,
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
