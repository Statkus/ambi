with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Api_Provider;
with Song;        use Song;
with Song_Vector; use Song_Vector;

package Yt_Api is

   -------------------------------------------------------------------------------------------------
   -- Set_Api_Key
   -------------------------------------------------------------------------------------------------
   procedure Set_Api_Key (Key : in String);

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Video_Search_Results
     (Search_Input : in     String;
      Search_Type  :    out Api_Provider.T_Search_Type) return T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Video_Duration (Video : in T_Song) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Videos
   -------------------------------------------------------------------------------------------------
   function Get_Related_Videos (Video : in T_Song) return T_Song_Vector;

private

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist (Playlist_Id : in String) return T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Request_Response
   -------------------------------------------------------------------------------------------------
   function Get_Request_Response (Url_Request : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_Search_Request
   -------------------------------------------------------------------------------------------------
   function Get_Search_Request (Search_Input : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_Video_Request
   -------------------------------------------------------------------------------------------------
   function Get_Video_Request (Video_Id : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_Videos_Related_Request
   -------------------------------------------------------------------------------------------------
   function Get_Videos_Related_Request (Video_Id : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist_Items_Request
   -------------------------------------------------------------------------------------------------
   function Get_Playlist_Items_Request
     (Playlist_Id : in String;
      Page_Token  : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Search_Results (Search_Results : in String) return T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Parse_Playlist_Item_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Playlist_Item_Results
     (Search_Results     : in     String;
      Total_Results      :    out Natural;
      Next_Page_Token    :    out Unbounded_String;
      Unavailable_Videos : in out Natural) return T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Duration_Result
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Duration_Result (Search_Result : in String) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Convert_Iso_8601_Duration_To_Seconds
   -------------------------------------------------------------------------------------------------
   function Convert_Iso_8601_Duration_To_Seconds
     (Iso_8601_Duration_String : in String) return Natural;

   Api_Url : constant String := "https://www.googleapis.com/youtube/v3/";
   Api_Key : Unbounded_String;

   Max_Video_Search_Results    : constant String := "10";
   Max_Number_Of_Request_Retry : constant        := 10;

end Yt_Api;
