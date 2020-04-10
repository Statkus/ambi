with Song;
with Song_Vector;

package Api.Provider is

   type T_Provider is abstract tagged limited private;
   type T_Provider_Class_Access is access all T_Provider'Class;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in out T_Provider;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song_Vector.T_Song_Vector is abstract;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration
     (This        : in out T_Provider;
      Source_Song : in     Song.T_Song) return Natural is abstract;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in out T_Provider;
      Source_Song : in     Song.T_Song) return Song_Vector.T_Song_Vector is abstract;

private

   type T_Provider is abstract tagged limited null record;

end Api.Provider;
