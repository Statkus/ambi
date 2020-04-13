package Web_Methods.Http is

   type T_Http is tagged limited null record;
   type T_Http_Class_Access is access all T_Http'Class;

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize return T_Http_Class_Access;

   -------------------------------------------------------------------------------------------------
   -- Get
   -------------------------------------------------------------------------------------------------
   function Get (This : in out T_Http; Url : in String) return String;

end Web_Methods.Http;
