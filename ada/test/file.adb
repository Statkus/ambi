with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

package body File is

   -------------------------------------------------------------------------------------------------
   -- Read_File
   -------------------------------------------------------------------------------------------------
   function Read_File (File_Name : in String) return String is
      File_Ref    : File_Type;
      File_String : Unbounded_String;
   begin
      Open (File => File_Ref, Mode => In_File, Name => File_Name);

      loop
         Append (File_String, Get_Line (File_Ref));
      end loop;

   exception
      when End_Error =>
         Close (File_Ref);
         return To_String (File_String);
   end Read_File;

end File;
