with Aunit; use Aunit;
with Aunit.Test_Cases;

package Yt_Api.Test is

   type T_Yt_Api_Test_Case is new Aunit.Test_Cases.Test_Case with null record;
   type T_Yt_Api_Test_Case_Access is access all T_Yt_Api_Test_Case;

   procedure Register_Tests (This : in out T_Yt_Api_Test_Case);

   function Name (This : in T_Yt_Api_Test_Case) return Message_String;

   procedure Test_Convert_Iso_8601_Duration_To_Seconds
     (Test_Case : in out Aunit.Test_Cases.Test_Case'Class);

end Yt_Api.Test;
