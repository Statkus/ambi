with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Aws.Mime;

with Callback_Room;
with Room_Name_Vector; use Room_Name_Vector;
with Room;             use Room;

package body Callback is

   -------------------------------------------------------------------------------------------------
   -- Set_Server_Address
   -------------------------------------------------------------------------------------------------
   procedure Set_Server_Address (Address : in String) is
   begin
      Callback_Room.Set_Server_Address (Address);
   end Set_Server_Address;

   -------------------------------------------------------------------------------------------------
   -- Set_Database
   -------------------------------------------------------------------------------------------------
   procedure Set_Database (Ambi_Database : in not null Database.T_Database_Access) is
   begin
      Db := Ambi_Database;
   end Set_Database;

   -------------------------------------------------------------------------------------------------
   -- Ambi_Callback
   -------------------------------------------------------------------------------------------------
   function Ambi_Callback (Request : Aws.Status.Data) return Aws.Response.Data is
      Uri : constant String := Aws.Status.Uri (Request);

      Current_Room : Room.T_Room_Class_Access := null;

      Response : Aws.Response.Data;
   begin
      if Uri = "/" then
         Put_Line ("Request: '" & Uri & "'");
         Response := Aws.Response.File (Aws.Mime.Text_Html, "html/index.html");
      elsif Index (Uri, "/javascripts/") > 0 then
         Put_Line ("Request: '" & Uri & "'");
         Response := Javascripts_Callback (Request);
      elsif Index (Uri, "/css/") > 0 then
         Put_Line ("Request: '" & Uri & "'");
         Response := Css_Callback (Request);
      elsif Index (Uri, "/icon/") > 0 then
         Put_Line ("Request: '" & Uri & "'");
         Response := Icon_Callback (Request);
      elsif Uri = "/get_rooms_list" then
         Put_Line ("Request: '" & Uri & "'");
         Response := Get_Rooms_List_Callback;
      else
         Current_Room := Select_Room_From_Uri (Uri);

         if Current_Room /= null then
            -- Room request
            Response := Callback_Room.Callback (Request, Current_Room);
         elsif Aws.Status.Parameter (Request, "roomName") = Uri (Uri'First + 1 .. Uri'Last) then
            -- Create room
            Rooms.Append
            (Room.New_And_Initialize (Uri (Uri'First + 1 .. Uri'Last), Db, Api_Dispatcher));
            Current_Room := Rooms.Last_Element;

            Put_Line
              ("New room: " & Current_Room.Get_Name & ", number of rooms:" & Rooms.Length'Img);

            Response := Callback_Room.Callback (Request, Current_Room);
         else
            Put_Line ("Not supported request: '" & Uri & "'");
            Response := Aws.Response.Build (Aws.Mime.Text_Html, "");
         end if;
      end if;

      return Response;
   end Ambi_Callback;

   -------------------------------------------------------------------------------------------------
   -- Javascripts_Callback
   -------------------------------------------------------------------------------------------------
   function Javascripts_Callback (Request : in Aws.Status.Data) return Aws.Response.Data is
      Uri : constant String := Aws.Status.Uri (Request);
   begin
      return Aws.Response.File (Aws.Mime.Text_Javascript, Uri (Uri'First + 1 .. Uri'Last));
   end Javascripts_Callback;

   -------------------------------------------------------------------------------------------------
   -- CSS_Callback
   -------------------------------------------------------------------------------------------------
   function Css_Callback (Request : in Aws.Status.Data) return Aws.Response.Data is
      Uri : constant String := Aws.Status.Uri (Request);
   begin
      return Aws.Response.File (Aws.Mime.Text_Css, Uri (Uri'First + 1 .. Uri'Last));
   end Css_Callback;

   -------------------------------------------------------------------------------------------------
   -- Icon_Callback
   -------------------------------------------------------------------------------------------------
   function Icon_Callback (Request : in Aws.Status.Data) return Aws.Response.Data is
      Uri : constant String := Aws.Status.Uri (Request);
   begin
      return Aws.Response.File (Aws.Mime.Image_Icon, Uri (Uri'First + 1 .. Uri'Last));
   end Icon_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Rooms_List_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Rooms_List_Callback return Aws.Response.Data is
      Response : Unbounded_String := To_Unbounded_String ("displayRoomNameAutocompletion([");
      Rooms_List        : constant T_Room_Name_Vector := Db.Get_Rooms;
      Rooms_List_Cursor : Room_Name_Vectors.Cursor    := Rooms_List.First;
   begin
      if Room_Name_Vectors.Has_Element (Rooms_List_Cursor) then
         Append (Response, """" & To_String (Room_Name_Vectors.Element (Rooms_List_Cursor)) & """");

         Room_Name_Vectors.Next (Rooms_List_Cursor);

         while Room_Name_Vectors.Has_Element (Rooms_List_Cursor) loop
            Append
              (Response,
               ",""" & To_String (Room_Name_Vectors.Element (Rooms_List_Cursor)) & """");

            Room_Name_Vectors.Next (Rooms_List_Cursor);
         end loop;
      end if;

      Append (Response, "])");

      return Aws.Response.Build (Aws.Mime.Text_Javascript, To_String (Response));
   end Get_Rooms_List_Callback;

   -------------------------------------------------------------------------------------------------
   -- Select_Room_From_URI
   -------------------------------------------------------------------------------------------------
   function Select_Room_From_Uri (Uri : in String) return Room.T_Room_Class_Access is
      Room_Name   : Unbounded_String := To_Unbounded_String (Uri (Uri'First + 1 .. Uri'Last));
      Slash_Index : constant Natural := Index (Uri (Uri'First + 1 .. Uri'Last), "/");

      Room_List_Cursor : Room_Vectors.Cursor      := Rooms.First;
      Room_Selected    : Room.T_Room_Class_Access := null;
   begin
      if Slash_Index > 0 then
         Room_Name := To_Unbounded_String (Slice (Room_Name, 1, Slash_Index - 2));
      end if;

      while Room_Vectors.Has_Element (Room_List_Cursor) and Room_Selected = null loop
         if Room_Vectors.Element (Room_List_Cursor).Get_Name = To_String (Room_Name) then
            Room_Selected := Room_Vectors.Element (Room_List_Cursor);
         end if;

         Room_Vectors.Next (Room_List_Cursor);
      end loop;

      return Room_Selected;
   end Select_Room_From_Uri;

end Callback;
