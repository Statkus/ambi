with Ada.Containers.Vectors;

with YT_API;

package Playlist is

   -- Dummy function to instantiate a vector, for now comparing YT_API.T_Video records is useless
   function Video_Compare (Left, Right : YT_API.T_Video) return Boolean;

   package Video_Vectors is new Ada.Containers.Vectors (Natural, YT_API.T_Video, Video_Compare);

end Playlist;
