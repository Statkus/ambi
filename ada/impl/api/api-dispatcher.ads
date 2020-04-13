with Api.Provider;

with Song;
with Song.List;

package Api.Dispatcher is

   type T_Dispatcher is tagged limited private;
   type T_Dispatcher_Access is access all T_Dispatcher;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize return T_Dispatcher_Access;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in     T_Dispatcher;
      Api_Provider : in     T_Api_Provider;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song.List.T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration (This : in T_Dispatcher; Source_Song : in Song.T_Song) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in T_Dispatcher;
      Source_Song : in Song.T_Song) return Song.List.T_Song_List;

private

   type T_Provider_List is array (T_Api_Provider) of Api.Provider.T_Provider_Class_Access;

   type T_Dispatcher is tagged limited record
      Provider_List : T_Provider_List;
   end record;

end Api.Dispatcher;
