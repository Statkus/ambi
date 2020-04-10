with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Api.Provider.No_Provider;
with Api.Provider.Youtube;
with Http_Methods;

package body Api.Dispatcher is

   package body Constructors is

      ----------------------------------------------------------------------------------------------
      -- New_And_Initialize
      ----------------------------------------------------------------------------------------------
      function New_And_Initialize return T_Dispatcher_Access is
         Config_File : File_Type;
         Yt_Api_Key  : Unbounded_String;
      begin
         -- Read Youtube API key
         Open (File => Config_File, Mode => In_File, Name => "yt_api_key.txt");
         Yt_Api_Key := To_Unbounded_String (Get_Line (Config_File));
         Close (Config_File);

         return new T_Dispatcher'
             (Provider_List =>
                (No_Provider_Api =>
                   Api.Provider.T_Provider_Class_Access
                     (Api.Provider.No_Provider.Constructors.New_And_Initialize),
                 Youtube_Api =>
                   Api.Provider.T_Provider_Class_Access
                     (Api.Provider.Youtube.Constructors.New_And_Initialize
                        (To_String (Yt_Api_Key),
                         new Http_Methods.T_Http_Methods))));
      end New_And_Initialize;

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in     T_Dispatcher;
      Api_Provider : in     T_Api_Provider;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song_Vector.T_Song_Vector is
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
      Source_Song : in Song.T_Song) return Song_Vector.T_Song_Vector is
     (This.Provider_List (Source_Song.Get_Provider).Get_Related_Songs (Source_Song));

end Api.Dispatcher;
