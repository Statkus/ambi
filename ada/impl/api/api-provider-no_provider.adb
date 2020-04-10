package body Api.Provider.No_Provider is

   package body Constructors is

      ----------------------------------------------------------------------------------------------
      -- New_And_Initialize
      ----------------------------------------------------------------------------------------------
      function New_And_Initialize return T_No_Provider_Access is (new T_No_Provider);

   end Constructors;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in out T_No_Provider;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song_Vector.T_Song_Vector is
     (Song_Vector.Constructors.Initialize);

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration
     (This        : in out T_No_Provider;
      Source_Song : in     Song.T_Song) return Natural is
     (Natural'First);

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in out T_No_Provider;
      Source_Song : in     Song.T_Song) return Song_Vector.T_Song_Vector is
     (Song_Vector.Constructors.Initialize);

end Api.Provider.No_Provider;
