package body Database.Mock is

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize return T_Database_Mock_Access is
     (new T_Database_Mock'
        (T_Database with
         Last_Room_Name => Null_Unbounded_String,
         Historic       => Song.List.Initialize));

   -------------------------------------------------------------------------------------------------
   -- Add_To_Room_Historic
   -------------------------------------------------------------------------------------------------
   procedure Add_To_Room_Historic
     (This      : in out T_Database_Mock;
      Room_Name : in     String;
      New_Song  : in     Song.T_Song)
   is
   begin
      This.Last_Room_Name := To_Unbounded_String (Room_Name);
      This.Historic.Append (New_Song);
   end Add_To_Room_Historic;

   -------------------------------------------------------------------------------------------------
   -- Get_Last_Room_Name
   -------------------------------------------------------------------------------------------------
   function Get_Last_Room_Name
     (This : in out T_Database_Mock) return String is
     (To_String (This.Last_Room_Name));

   -------------------------------------------------------------------------------------------------
   -- Get_Historic
   -------------------------------------------------------------------------------------------------
   function Get_Historic
     (This : in out T_Database_Mock) return Song.List.T_Song_List is
     (This.Historic);

end Database.Mock;
