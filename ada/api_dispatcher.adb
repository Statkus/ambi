with Yt_Api;

package body Api_Dispatcher is

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (Provider     : in     Api_Provider.T_Api_Provider;
      Search_Input : in     String;
      Search_Type  :    out Api_Provider.T_Search_Type) return Song_Vector.T_Song_Vector
   is
      Song_Search_Results : Song_Vector.T_Song_Vector := Song_Vector.Constructors.Initialize;
   begin
      case Provider is
         when Api_Provider.Youtube =>
            Song_Search_Results := Yt_Api.Get_Video_Search_Results (Search_Input, Search_Type);

         when Api_Provider.No_Provider =>
            null;
      end case;

      return Song_Search_Results;
   end Get_Song_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration (Source_Song : in Song.T_Song) return Natural is
      Duration : Natural := Natural'First;
   begin
      case Source_Song.Get_Provider is
         when Api_Provider.Youtube =>
            Duration := Yt_Api.Get_Video_Duration (Source_Song);

         when Api_Provider.No_Provider =>
            null;
      end case;

      return Duration;
   end Get_Song_Duration;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs (Source_Song : in Song.T_Song) return Song_Vector.T_Song_Vector is
      Related_Songs : Song_Vector.T_Song_Vector := Song_Vector.Constructors.Initialize;
   begin
      case Source_Song.Get_Provider is
         when Api_Provider.Youtube =>
            Related_Songs := Yt_Api.Get_Related_Videos (Source_Song);

         when Api_Provider.No_Provider =>
            null;
      end case;

      return Related_Songs;
   end Get_Related_Songs;

end Api_Dispatcher;
