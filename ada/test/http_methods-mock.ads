with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Http_Methods.Mock is

   type T_Http_Methods_Mock is new T_Http_Methods with private;
   type T_Http_Methods_Mock_Access is access all T_Http_Methods_Mock;

   package Get_Response_Vectors is new Ada.Containers.Vectors (Natural, Unbounded_String);

   type T_Get_Response_Vector is new Get_Response_Vectors.Vector with null record;

   -------------------------------------------------------------------------------------------------
   -- Get
   -------------------------------------------------------------------------------------------------
   function Get (This : in out T_Http_Methods_Mock; Url : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- Set_Get_Response
   -------------------------------------------------------------------------------------------------
   procedure Set_Get_Response (This : in out T_Http_Methods_Mock; Response : in String);

   -------------------------------------------------------------------------------------------------
   -- Set_Multiple_Get_Response
   -------------------------------------------------------------------------------------------------
   procedure Set_Multiple_Get_Response
     (This      : in out T_Http_Methods_Mock'Class;
      Responses : in     T_Get_Response_Vector);

   -------------------------------------------------------------------------------------------------
   -- Reset_Number_Of_Get_Called
   -------------------------------------------------------------------------------------------------
   procedure Reset_Number_Of_Get_Called (This : in out T_Http_Methods_Mock);

   -------------------------------------------------------------------------------------------------
   -- Get_Get_Url
   -------------------------------------------------------------------------------------------------
   function Get_Get_Url (This : in T_Http_Methods_Mock) return String;

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Of_Get_Called
   -------------------------------------------------------------------------------------------------
   function Get_Number_Of_Get_Called (This : in T_Http_Methods_Mock) return Natural;

private

   type T_Http_Methods_Mock is new T_Http_Methods with record
      Get_Url                      : Unbounded_String;
      Number_Of_Get_Called         : Natural := Natural'First;
      Multiple_Get_Response_Used   : Boolean := False;
      Get_Response                 : Unbounded_String;
      Multiple_Get_Response        : T_Get_Response_Vector;
      Multiple_Get_Response_Cursor : Get_Response_Vectors.Cursor;
   end record;

end Http_Methods.Mock;
