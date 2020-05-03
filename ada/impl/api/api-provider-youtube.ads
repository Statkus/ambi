with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Web_Methods.Http;

package Api.Provider.Youtube is

   type T_Youtube (Api_Key_Length : Positive) is new T_Provider with private;
   type T_Youtube_Access is access all T_Youtube;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize
     (Api_Key       : in String;
      Http_Accessor : in not null Web_Methods.Http.T_Http_Class_Access) return T_Youtube_Access;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in out T_Youtube;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song.List.T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration
     (This        : in out T_Youtube;
      Source_Song : in     Song.T_Song) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in out T_Youtube;
      Source_Song : in     Song.T_Song) return Song.List.T_Song_List;

private

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist
     (This        : in T_Youtube;
      Playlist_Id : in String) return Song.List.T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Format_Search_Request
   -------------------------------------------------------------------------------------------------
   function Format_Search_Request (This : in T_Youtube; Search_Input : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Format_Video_Request
   -------------------------------------------------------------------------------------------------
   function Format_Video_Request (This : in T_Youtube; Video_Id : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Format_Videos_Related_Request
   -------------------------------------------------------------------------------------------------
   function Format_Videos_Related_Request (This : in T_Youtube; Video_Id : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Format_Playlist_Items_Request
   -------------------------------------------------------------------------------------------------
   function Format_Playlist_Items_Request
     (This        : in T_Youtube;
      Playlist_Id : in String;
      Page_Token  : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_Request_Response
   -------------------------------------------------------------------------------------------------
   function Get_Request_Response (This : in T_Youtube; Url_Request : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Search_Results (Search_Results : in String) return Song.List.T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Parse_Playlist_Item_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Playlist_Item_Results
     (Search_Results     : in     String;
      Total_Results      :    out Natural;
      Next_Page_Token    :    out Unbounded_String;
      Unavailable_Videos : in out Natural) return Song.List.T_Song_List;

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

   Max_Number_Of_Request_Retry : constant := 10;

   type T_Youtube (Api_Key_Length : Positive) is new T_Provider with record
      Api_Key       : String (Positive'First .. Api_Key_Length);
      Http_Accessor : Web_Methods.Http.T_Http_Class_Access;
   end record;

end Api.Provider.Youtube;
