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
   YT_API_Key_File : File_Type;

   Ambi_Server : AWS.Server.HTTP;
   Ch : Character;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);

   -- Read Youtube API key
   Open (File => YT_API_Key_File, Mode => In_File, Name => "yt_api_key");
   YT_API.Set_YT_API_Key (Get_Line (YT_API_Key_File));
   Close (YT_API_Key_File);

   -- Launch the server on port 80
   AWS.Server.Start
     (Web_Server => Ambi_Server,
      Name       => "Ambi",
      Callback   => Callback.Ambi_Callback'Access,
      Port       => 80);

   Put_Line ("Press any key to quit.");
   Get_Immediate (Ch);

   AWS.Server.Shutdown (Ambi_Server);
end Ambi;
