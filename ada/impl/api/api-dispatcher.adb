with Api.Provider.No_Provider;
with Api.Provider.Youtube;
with Web_Methods.Http;

package body Api.Dispatcher is

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize
     (Youtube_Api_Key_File_Name : in String := "yt_api_key.txt") return T_Dispatcher_Access
   is
      Http_Accessor : constant Web_Methods.Http.T_Http_Class_Access :=
        Web_Methods.Http.New_And_Initialize;
   begin
      return new T_Dispatcher'
          (Provider_List =>
             (No_Provider_Api =>
                Api.Provider.T_Provider_Class_Access (Api.Provider.No_Provider.New_And_Initialize),
              Youtube_Api =>
                Api.Provider.T_Provider_Class_Access
                  (Api.Provider.Youtube.New_And_Initialize
                     (Http_Accessor     => Http_Accessor,
                      Api_Key_File_Name => Youtube_Api_Key_File_Name))));
   end New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in     T_Dispatcher;
      Api_Provider : in     T_Api_Provider;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song.List.T_Song_List is
     (This.Provider_List (Api_Provider).Get_Song_Search_Results (Search_Input, Search_Type));

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration
     (This        : in T_Dispatcher;
      Source_Song : in Song.T_Song) return Natural is
     (This.Provider_List (Source_Song.Get_Provider).Get_Song_Duration (Source_Song));

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in T_Dispatcher;
      Source_Song : in Song.T_Song) return Song.List.T_Song_List is
     (This.Provider_List (Source_Song.Get_Provider).Get_Related_Songs (Source_Song));

end Api.Dispatcher;
