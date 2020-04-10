package body Http_Methods.Mock is

   -------------------------------------------------------------------------------------------------
   -- Get
   -------------------------------------------------------------------------------------------------
   function Get (This : in out T_Http_Methods_Mock; Url : in String) return String is
      Response : Unbounded_String;
   begin
      This.Get_Url              := To_Unbounded_String (Url);
      This.Number_Of_Get_Called := This.Number_Of_Get_Called + 1;

      if not This.Multiple_Get_Response_Used then
         Response := This.Get_Response;
      else
         Response := Get_Response_Vectors.Element (This.Multiple_Get_Response_Cursor);
         Get_Response_Vectors.Next (This.Multiple_Get_Response_Cursor);
      end if;

      return To_String (Response);
   end Get;

   -------------------------------------------------------------------------------------------------
   -- Set_Get_Response
   -------------------------------------------------------------------------------------------------
   procedure Set_Get_Response (This : in out T_Http_Methods_Mock; Response : in String) is
   begin
      This.Multiple_Get_Response_Used := False;
      This.Get_Response               := To_Unbounded_String (Response);
   end Set_Get_Response;

   -------------------------------------------------------------------------------------------------
   -- Set_Multiple_Get_Response
   -------------------------------------------------------------------------------------------------
   procedure Set_Multiple_Get_Response
     (This      : in out T_Http_Methods_Mock'Class;
      Responses : in     T_Get_Response_Vector)
   is
   begin
      This.Multiple_Get_Response_Used   := True;
      This.Multiple_Get_Response        := Responses;
      This.Multiple_Get_Response_Cursor := This.Multiple_Get_Response.First;
   end Set_Multiple_Get_Response;

   -------------------------------------------------------------------------------------------------
   -- Reset_Number_Of_Get_Called
   -------------------------------------------------------------------------------------------------
   procedure Reset_Number_Of_Get_Called (This : in out T_Http_Methods_Mock) is
   begin
      This.Number_Of_Get_Called := Natural'First;
   end Reset_Number_Of_Get_Called;

   -------------------------------------------------------------------------------------------------
   -- Get_Get_Url
   -------------------------------------------------------------------------------------------------
   function Get_Get_Url (This : in T_Http_Methods_Mock) return String is (To_String (This.Get_Url));

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Of_Get_Called
   -------------------------------------------------------------------------------------------------
   function Get_Number_Of_Get_Called
     (This : in T_Http_Methods_Mock) return Natural is
     (This.Number_Of_Get_Called);

end Http_Methods.Mock;
