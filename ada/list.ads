with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package List is

   type T_Video is record
      Video_ID        : Unbounded_String;
      Video_Title     : Unbounded_String;
      Video_Thumbnail : Unbounded_String;
   end record;

   -- Dummy function to instantiate a vector, for now comparing T_Video records is useless
   function Video_Compare (Left, Right : in T_Video) return Boolean is (False);

   package Video_Vectors is new Ada.Containers.Vectors (Natural, T_Video, Video_Compare);

   package Room_Name_Vectors is new Ada.Containers.Vectors (Natural, Unbounded_String);

end List;
