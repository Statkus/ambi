with Aunit.Assertions; use Aunit.Assertions;

package body Yt_Api.Test is

   procedure Register_Tests (This : in out T_Yt_Api_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine
        (This,
         Test_Convert_Iso_8601_Duration_To_Seconds'Access,
         "Test Convert_Iso_8601_Duration_To_Seconds");
   end Register_Tests;

   function Name (This : in T_Yt_Api_Test_Case) return Test_String is
      pragma Unreferenced (This);

   begin
      return Format ("Yt_Api tests");
   end Name;

   procedure Test_Convert_Iso_8601_Duration_To_Seconds
     (Test_Case : in out Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Test_Case);

   begin
      Assert
        (Yt_Api.Convert_Iso_8601_Duration_To_Seconds ("PT0H0M0S") = 0,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Yt_Api.Convert_Iso_8601_Duration_To_Seconds ("PT21S") = 21,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Yt_Api.Convert_Iso_8601_Duration_To_Seconds ("PT12M") = 720,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Yt_Api.Convert_Iso_8601_Duration_To_Seconds ("PT3H") = 10800,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Yt_Api.Convert_Iso_8601_Duration_To_Seconds ("PT59S") = 59,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Yt_Api.Convert_Iso_8601_Duration_To_Seconds ("PT1M0S") = 60,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Yt_Api.Convert_Iso_8601_Duration_To_Seconds ("PT59M59S") = 3599,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Yt_Api.Convert_Iso_8601_Duration_To_Seconds ("PT1H0M0S") = 3600,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Yt_Api.Convert_Iso_8601_Duration_To_Seconds ("PT23H59M59S") = 86399,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Yt_Api.Convert_Iso_8601_Duration_To_Seconds ("PT15H37M4S") = 56224,
         "Wrong Conversion from ISO 8601 to seconds.");
   end Test_Convert_Iso_8601_Duration_To_Seconds;

end Yt_Api.Test;
