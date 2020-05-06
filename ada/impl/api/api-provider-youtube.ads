with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Web_Methods.Http;

package Api.Provider.Youtube is

   type T_Youtube is new T_Provider with private;
   type T_Youtube_Access is access all T_Youtube;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize
     (Http_Accessor     : in not null Web_Methods.Http.T_Http_Class_Access;
      Api_Key_File_Name : in String) return T_Youtube_Access;

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
   -- Get_Key
   -------------------------------------------------------------------------------------------------
   function Get_Key (This : in out T_Youtube; Query_Value : in Natural) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_Playlist
   -------------------------------------------------------------------------------------------------
   function Get_Playlist
     (This        : in out T_Youtube;
      Playlist_Id : in     String) return Song.List.T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Format_Search_Request
   -------------------------------------------------------------------------------------------------
   function Format_Search_Request (This : in out T_Youtube; Search_Input : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Format_Video_Request
   -------------------------------------------------------------------------------------------------
   function Format_Video_Request (This : in out T_Youtube; Video_Id : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Format_Videos_Related_Request
   -------------------------------------------------------------------------------------------------
   function Format_Videos_Related_Request
     (This     : in out T_Youtube;
      Video_Id : in     String) return String;

   -------------------------------------------------------------------------------------------------
   -- Format_Playlist_Items_Request
   -------------------------------------------------------------------------------------------------
   function Format_Playlist_Items_Request
     (This        : in out T_Youtube;
      Playlist_Id : in     String;
      Page_Token  : in     String) return String;

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

   package Api_Key_Vectors is new Ada.Containers.Vectors (Natural, Unbounded_String);

   Api_Url : constant String := "https://www.googleapis.com/youtube/v3/";

   Max_Number_Of_Request_Retry : constant := 10;

   Search_Query_Value         : constant := 105;
   Videos_Query_Value         : constant := 5;
   Playlist_Items_Query_Value : constant := 5;

   Quota : constant := 8000;

   type T_Youtube is new T_Provider with record
      Api_Keys       : Api_Key_Vectors.Vector;
      Api_Key_Cursor : Api_Key_Vectors.Cursor;
      Api_Key_Quota  : Natural;
      Http_Accessor  : Web_Methods.Http.T_Http_Class_Access;
   end record;

end Api.Provider.Youtube;
