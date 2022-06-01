with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Sanitizer is

   -------------------------------------------------------------------------------------------------
   -- Sanitize_Room_Name
   -------------------------------------------------------------------------------------------------
   function Sanitize_Chat_Message (Unsafe_Chat_Message : in String) return String is
      Safe_Chat_Message : Unbounded_String;
   begin
      for Char of Unsafe_Chat_Message loop
         case Char is
            when '<' =>
               Append (Source => Safe_Chat_Message, New_Item => "&lt");

            when '>' =>
               Append (Source => Safe_Chat_Message, New_Item => "&gt");

            when '"' =>
               Append (Source => Safe_Chat_Message, New_Item => "&quot");

            when ''' =>
               Append (Source => Safe_Chat_Message, New_Item => "&#39");

            when '&' =>
               Append (Source => Safe_Chat_Message, New_Item => "&amp");

            when others =>
               Append (Source => Safe_Chat_Message, New_Item => Char);
         end case;
      end loop;

      return To_String (Safe_Chat_Message);
   end Sanitize_Chat_Message;

   -------------------------------------------------------------------------------------------------
   -- Is_Room_Name_Sanitized
   -------------------------------------------------------------------------------------------------
   function Is_Room_Name_Sanitized (Room_Name : in String) return Boolean is
      Is_Sanitized : Boolean := True;
   begin
      for Char of Room_Name loop
         if Char not in 'a' .. 'z' and Char not in '0' .. '9' and Char /= ' ' then
            Is_Sanitized := False;
            exit;
         end if;
      end loop;

      return Is_Sanitized;
   end Is_Room_Name_Sanitized;

end Sanitizer;
