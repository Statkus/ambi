with Api_Provider;

with Song;
with Song_Vector;

package Api_Dispatcher is

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (Provider     : in     Api_Provider.T_Api_Provider;
      Search_Input : in     String;
      Search_Type  :    out Api_Provider.T_Search_Type) return Song_Vector.T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration (Source_Song : in Song.T_Song) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs (Source_Song : in Song.T_Song) return Song_Vector.T_Song_Vector;

end Api_Dispatcher;
