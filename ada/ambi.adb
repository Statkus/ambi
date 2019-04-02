with GNAT.Exception_Traces;

with Ada.Text_IO; use Ada.Text_IO;

with AWS.Net.Websocket.Registry.Control;
with AWS.Server;
with AWS.Session;

with Callback;
with Database;
with YT_API;

----------------------------------------------------------------------------------------------------
-- Ambi
----------------------------------------------------------------------------------------------------
procedure Ambi is
   Config_File   : File_Type;
   Ambi_Database : constant Database.T_Database_Class_Access := new Database.T_Database;
   Ambi_Server   : AWS.Server.HTTP;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);

   -- Read server address
   Open (File => Config_File, Mode => In_File, Name => "server_address.txt");
   Callback.Set_Server_Address (Get_Line (Config_File));
   Close (Config_File);

   -- Read Youtube API key
   Open (File => Config_File, Mode => In_File, Name => "yt_api_key.txt");
   YT_API.Set_YT_API_Key (Get_Line (Config_File));
   Close (Config_File);

   -- Open or create the ambi database
   Ambi_Database.Open;
   Callback.Set_Database (Ambi_Database);

   -- Launch the server on port 80
   AWS.Server.Start
     (Web_Server     => Ambi_Server,
      Name           => "Ambi",
      Callback       => Callback.Ambi_Callback'Access,
      Port           => 80,
      Max_Connection => 100,
      Session        => True);

   -- Launch Websocket server
   AWS.Net.WebSocket.Registry.Control.Start;

   -- Set sessions lifetime to 2 hours
   AWS.Session.Set_Lifetime (7200.0);

   Put_Line ("Press Q key to quit.");
   AWS.Server.Wait (AWS.Server.Q_Key_Pressed);

   AWS.Net.WebSocket.Registry.Control.Shutdown;
   AWS.Server.Shutdown (Ambi_Server);

   Ambi_Database.Close;
end Ambi;
