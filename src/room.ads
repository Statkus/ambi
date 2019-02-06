with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with YT_API;

package Room is

   type T_Room is tagged private;

   procedure Set_Current_Video
     (This : in out T_Room; Current_Video_Index : in Integer);

   procedure Set_Video_Search_Results
     (This : in out T_Room; Video_Search_Results : in YT_API.T_Video_Search_Results);

   function Get_Current_Video (This : in T_Room) return YT_API.T_Video_Search_Result;

   function Get_Video_Search_Results (This : in T_Room) return YT_API.T_Video_Search_Results;

private

   type T_Room is tagged record
      Current_Video        : YT_API.T_Video_Search_Result;
      Video_Search_Results : YT_API.T_Video_Search_Results;
   end record;

end Room;
