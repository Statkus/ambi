with Aunit.Run;
with Aunit.Reporter.Text;

with Ambi_Suite;

----------------------------------------------------------------------------------------------------
-- Ambi_Test
----------------------------------------------------------------------------------------------------
procedure Ambi_Test is
   procedure Run is new Aunit.Run.Test_Runner (Ambi_Suite.Suite);

   Reporter : Aunit.Reporter.Text.Text_Reporter;
begin
   Aunit.Reporter.Text.Set_Use_Ansi_Colors (Reporter, True);

   Run (Reporter);
end Ambi_Test;
