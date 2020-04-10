with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

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
             (Youtube =>
                Api.Provider.T_Provider_Class_Access
                  (Api.Provider.Youtube.Constructors.New_And_Initialize
                     (To_String (Yt_Api_Key),
                      new Http_Methods.T_Http_Methods)));
      end New_And_Initialize;

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in     T_Dispatcher;
      Api_Provider : in     T_Api_Provider;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song_Vector.T_Song_Vector
   is
      Song_Search_Results : Song_Vector.T_Song_Vector := Song_Vector.Constructors.Initialize;
   begin
      case Api_Provider is
         when Api.Youtube_Api =>
            Song_Search_Results := This.Youtube.Get_Song_Search_Results (Search_Input, Search_Type);

         when Api.No_Provider =>
            null;
      end case;

      return Song_Search_Results;
   end Get_Song_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration
     (This        : in T_Dispatcher;
      Source_Song : in Song.T_Song) return Natural
   is
      Duration : Natural := Natural'First;
   begin
      case Source_Song.Get_Provider is
         when Api.Youtube_Api =>
            Duration := This.Youtube.Get_Song_Duration (Source_Song);

         when Api.No_Provider =>
            null;
      end case;

      return Duration;
   end Get_Song_Duration;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in T_Dispatcher;
      Source_Song : in Song.T_Song) return Song_Vector.T_Song_Vector
   is
      Related_Songs : Song_Vector.T_Song_Vector := Song_Vector.Constructors.Initialize;
   begin
      case Source_Song.Get_Provider is
         when Api.Youtube_Api =>
            Related_Songs := This.Youtube.Get_Related_Songs (Source_Song);

         when Api.No_Provider =>
            null;
      end case;

      return Related_Songs;
   end Get_Related_Songs;

end Api.Dispatcher;
