with AWS.MIME;
with AWS.Parameters;

with Templates_Parser;

with GNAT.Regpat; use GNAT.Regpat;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Text_IO; use Ada.Text_IO;

package body Callback is

   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      URI        : constant String := AWS.Status.URI (Request);
      Parameters : constant AWS.Parameters.List := AWS.Status.Parameters (Request);

      Video_URL : constant String := AWS.Parameters.Get (Parameters, "video_url");
      Video_ID : String := "00000000000";

      Translations : Templates_Parser.Translate_Table :=
        (1 => Templates_Parser.Assoc ("VIDEO_ID", Video_ID));

      Web_Page : Unbounded_String;

      Video_ID_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile ("v=([a-zA-Z0-9_-]+)");

      Result : GNAT.Regpat.Match_Array (0 .. 1);
   begin
      Put_Line ("NEW REQUEST ----------------------------------------------------------------");

      GNAT.Regpat.Match (Video_ID_Pattern, Video_URL, Result);

      if Result (1) /= GNAT.Regpat.No_Match then
         Video_ID (Video_ID'First .. Video_ID'Last) := Video_URL (Result (1).First .. Result (1).Last);
      end if;

      Translations (1) := Templates_Parser.Assoc ("VIDEO_ID", Video_ID);


      Web_Page := To_Unbounded_String (Templates_Parser.Parse ("html/ambi.thtml", Translations));

      Put_Line ("Video URL: " & Video_URL);
      Put_Line ("Video ID: " & Video_ID);
      --Put_Line (To_String (Web_Page));

      return AWS.Response.Build (AWS.MIME.Text_HTML, To_String (Web_Page));
   end Ambi_Callback;

end Callback;
