with AWS.Net.Websocket.Registry.Control;
with AWS.Server;

with Callback;
with YT_API;

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Exception_Traces;

with Templates_Parser;

----------------------------------------------------------------------------------------------------
-- Ambi
----------------------------------------------------------------------------------------------------
procedure Ambi is
   Config_File : File_Type;

   Ambi_Server : AWS.Server.HTTP;
   Ch : Character;
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

   Put_Line ("Press any key to quit.");
   Get_Immediate (Ch);

   AWS.Server.Shutdown (Ambi_Server);
end Ambi;
