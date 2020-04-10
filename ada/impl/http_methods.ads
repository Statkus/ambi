package Http_Methods is

   type T_Http_Methods is tagged limited private;
   type T_Http_Methods_Class_Access is access all T_Http_Methods'Class;

   function Get (This : in out T_Http_Methods; Url : in String) return String;

private

   type T_Http_Methods is tagged limited null record;

end Http_Methods;
