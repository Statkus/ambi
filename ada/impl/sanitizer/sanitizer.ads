package Sanitizer is

   -------------------------------------------------------------------------------------------------
   -- Sanitize_Chat_Message
   -------------------------------------------------------------------------------------------------
   function Sanitize_Chat_Message (Unsafe_Chat_Message : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Is_Room_Name_Sanitized
   -------------------------------------------------------------------------------------------------
   function Is_Room_Name_Sanitized (Room_Name : in String) return Boolean;

end Sanitizer;
