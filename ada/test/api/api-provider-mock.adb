package body Api.Provider.Mock is

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   function Get_Song_Search_Results
     (This         : in out T_Mock;
      Search_Input : in     String;
      Search_Type  :    out T_Search_Type) return Song_Vector.T_Song_Vector
   is
      pragma Unreferenced (Search_Input);
      pragma Unreferenced (Search_Type);
   begin
      This.Get_Song_Search_Results_Called := True;

      return Song_Vector.Constructors.Initialize;
   end Get_Song_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   function Get_Song_Duration (This : in out T_Mock; Source_Song : in Song.T_Song) return Natural is
      pragma Unreferenced (Source_Song);
   begin
      This.Get_Song_Duration_Called := True;

      return Natural'First;
   end Get_Song_Duration;

   -------------------------------------------------------------------------------------------------
   -- Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   function Get_Related_Songs
     (This        : in out T_Mock;
      Source_Song : in     Song.T_Song) return Song_Vector.T_Song_Vector
   is
      pragma Unreferenced (Source_Song);
   begin
      This.Get_Related_Songs_Called := True;

      return Song_Vector.Constructors.Initialize;
   end Get_Related_Songs;

   -------------------------------------------------------------------------------------------------
   -- Is_Get_Song_Search_Results_Called
   -------------------------------------------------------------------------------------------------
   function Is_Get_Song_Search_Results_Called
     (This : in T_Mock) return Boolean is
     (This.Get_Song_Search_Results_Called);

   -------------------------------------------------------------------------------------------------
   -- Is_Get_Song_Duration_Called
   -------------------------------------------------------------------------------------------------
   function Is_Get_Song_Duration_Called
     (This : in T_Mock) return Boolean is
     (This.Get_Song_Duration_Called);

   -------------------------------------------------------------------------------------------------
   -- Is_Get_Related_Songs_Called
   -------------------------------------------------------------------------------------------------
   function Is_Get_Related_Songs_Called
     (This : in T_Mock) return Boolean is
     (This.Get_Related_Songs_Called);

   -------------------------------------------------------------------------------------------------
   -- Reset_Calls
   -------------------------------------------------------------------------------------------------
   procedure Reset_Calls (This : in out T_Mock) is
   begin
      This.Get_Song_Search_Results_Called := False;
      This.Get_Song_Duration_Called       := False;
      This.Get_Related_Songs_Called       := False;
   end Reset_Calls;

end Api.Provider.Mock;
