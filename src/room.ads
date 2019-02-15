with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with YT_API;

package Room is

   type T_Room is tagged private;

   -- Dummy function to instantiate a vector, for now comparing YT_API.T_Video records is useless
   function Video_Compare (Left, Right : YT_API.T_Video) return Boolean;

   package Video_Vectors is new Ada.Containers.Vectors (Natural, YT_API.T_Video, Video_Compare);

   procedure Add_Video_To_Playlist (This : in out T_Room; Video : in YT_API.T_Video);

   procedure Set_Current_Video
     (This : in out T_Room; Current_Video_Index : in Integer);

   procedure Set_Video_Search_Results
     (This : in out T_Room; Video_Search_Results : in YT_API.T_Video_Search_Results);

   function Get_Current_Video (This : in T_Room) return YT_API.T_Video;

   function Get_Video_Search_Results (This : in T_Room) return YT_API.T_Video_Search_Results;

   function Get_Playlist (This : in T_Room) return Video_Vectors.Vector;

private

   type T_Room is tagged record
      Current_Video        : YT_API.T_Video;
      Video_Search_Results : YT_API.T_Video_Search_Results;
      Playlist             : Video_Vectors.Vector := Video_Vectors.Empty_Vector;
   end record;

end Room;
