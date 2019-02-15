package body Room is

   -------------------------------------------------------------------------------------------------
   -- Video_Compare
   -------------------------------------------------------------------------------------------------
   function Video_Compare (Left, Right : YT_API.T_Video) return Boolean is (False);

   -------------------------------------------------------------------------------------------------
   -- Add_Video_To_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Add_Video_To_Playlist (This : in out T_Room; Video : in YT_API.T_Video) is
   begin
      This.Playlist.Append (Video);
   end Add_Video_To_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Set_Current_Video
   -------------------------------------------------------------------------------------------------
   procedure Set_Current_Video
     (This : in out T_Room; Current_Video_Index : in Integer) is
   begin
      This.Current_Video := This.Video_Search_Results (Current_Video_Index);
   end Set_Current_Video;

   -------------------------------------------------------------------------------------------------
   -- Set_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   procedure Set_Video_Search_Results
     (This : in out T_Room; Video_Search_Results : in YT_API.T_Video_Search_Results) is
   begin
      This.Video_Search_Results := Video_Search_Results;
   end Set_Video_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Video
   -------------------------------------------------------------------------------------------------
   function Get_Current_Video (This : in T_Room) return YT_API.T_Video is (This.Current_Video);

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Video_Search_Results (This : in T_Room) return YT_API.T_Video_Search_Results is
     (This.Video_Search_Results);

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (This : in T_Room) return Video_Vectors.Vector is (This.Playlist);

end Room;
