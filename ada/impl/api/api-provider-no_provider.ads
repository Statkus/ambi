package Api.Provider.No_Provider is

   type T_No_Provider is new T_Provider with private;
   type T_No_Provider_Access is access all T_No_Provider;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize return T_No_Provider_Access;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in out T_No_Provider;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song.List.T_Song_List;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration
     (This        : in out T_No_Provider;
      Source_Song : in     Song.T_Song) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in out T_No_Provider;
      Source_Song : in     Song.T_Song) return Song.List.T_Song_List;

private

   type T_No_Provider is new T_Provider with null record;

end Api.Provider.No_Provider;
