package Api.Provider.Mock is

   type T_Mock is new T_Provider with private;
   type T_Mock_Access is access all T_Mock;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in out T_Mock;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song_Vector.T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration (This : in out T_Mock; Source_Song : in Song.T_Song) return Natural;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in out T_Mock;
      Source_Song : in     Song.T_Song) return Song_Vector.T_Song_Vector;

   -------------------------------------------------------------------------------------------------
   -- Is_Get_Song_Search_Results_Called
   -------------------------------------------------------------------------------------------------
   function Is_Get_Song_Search_Results_Called (This : in T_Mock) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Is_Get_Song_Duration_Called
   -------------------------------------------------------------------------------------------------
   function Is_Get_Song_Duration_Called (This : in T_Mock) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Is_Get_Related_Songs_Called
   -------------------------------------------------------------------------------------------------
   function Is_Get_Related_Songs_Called (This : in T_Mock) return Boolean;

   -------------------------------------------------------------------------------------------------
   -- Reset_Calls
   -------------------------------------------------------------------------------------------------
   procedure Reset_Calls (This : in out T_Mock);

private

   type T_Mock is new T_Provider with record
      Get_Song_Search_Results_Called : Boolean := False;
      Get_Song_Duration_Called       : Boolean := False;
      Get_Related_Songs_Called       : Boolean := False;
   end record;

end Api.Provider.Mock;
