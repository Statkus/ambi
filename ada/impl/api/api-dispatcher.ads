with Api.Provider;

with Song;
with Song_Vector;

package Api.Dispatcher is

   type T_Dispatcher is tagged limited private;
   type T_Dispatcher_Access is access all T_Dispatcher;

   package Constructors is

      ----------------------------------------------------------------------------------------------
      -- New_And_Initialize
      ----------------------------------------------------------------------------------------------
      function New_And_Initialize return T_Dispatcher_Access;

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in     T_Dispatcher;
      Api_Provider : in     T_Api_Provider;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song_Vector.T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration (This : in T_Dispatcher; Source_Song : in Song.T_Song) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in T_Dispatcher;
      Source_Song : in Song.T_Song) return Song_Vector.T_Song_Vector;

private

   type T_Dispatcher is tagged limited record
      Youtube : Api.Provider.T_Provider_Class_Access;
   end record;

end Api.Dispatcher;
