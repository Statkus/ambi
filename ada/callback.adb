with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with AWS.MIME;

with Callback_Room;
with List; use List;
with Room; use Room;

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
   procedure Set_Database (Ambi_Database : in not null Database.T_Database_Class_Access) is
   begin
      DB := Ambi_Database;
   end Set_Database;

   -------------------------------------------------------------------------------------------------
   -- Ambi_Callback
   -------------------------------------------------------------------------------------------------
   function Ambi_Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);

      Current_Room : Room.T_Room_Class_Access := null;

      Response : AWS.Response.Data;
   begin
      if URI = "/" then
         Put_Line ("Request: '" & URI & "'");
         Response := AWS.Response.File (AWS.MIME.Text_HTML, "html/index.html");
      elsif Index (URI, "/javascripts/") > 0 then
         Put_Line ("Request: '" & URI & "'");
         Response := Javascripts_Callback (Request);
      elsif Index (URI, "/css/") > 0 then
         Put_Line ("Request: '" & URI & "'");
         Response := CSS_Callback (Request);
      elsif Index (URI, "/icon/") > 0 then
         Put_Line ("Request: '" & URI & "'");
         Response := Icon_Callback (Request);
      elsif URI = "/get_rooms_list" then
         Put_Line ("Request: '" & URI & "'");
         Response := Get_Rooms_List_Callback;
      else
         Current_Room := Select_Room_From_URI (URI);

         if Current_Room /= null then
            -- Room request
            Response := Callback_Room.Callback (Request, Current_Room);
         elsif AWS.STATUS.Parameter (Request, "roomName") = URI (URI'First + 1 .. URI'Last) then
            -- Create room
            Rooms.Append (Room.New_And_Initialize (URI (URI'First + 1 .. URI'Last), DB));
            Current_Room := Rooms.Last_Element;

            Put_Line ("New room: " & Current_Room.Get_Name & ", number of rooms:"
              & Rooms.Length'Img);

            Response := Callback_Room.Callback (Request, Current_Room);
         else
            Put_Line ("Not supported request: '" & URI & "'");
            Response := AWS.Response.Build (AWS.MIME.Text_HTML, "");
         end if;
      end if;

      return Response;
   end Ambi_Callback;

   -------------------------------------------------------------------------------------------------
   -- Javascripts_Callback
   -------------------------------------------------------------------------------------------------
   function Javascripts_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      return AWS.Response.File (AWS.MIME.Text_Javascript, URI (URI'First + 1 .. URI'Last));
   end Javascripts_Callback;

   -------------------------------------------------------------------------------------------------
   -- CSS_Callback
   -------------------------------------------------------------------------------------------------
   function CSS_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      return AWS.Response.File (AWS.MIME.Text_CSS, URI (URI'First + 1 .. URI'Last));
   end CSS_Callback;

   -------------------------------------------------------------------------------------------------
   -- Icon_Callback
   -------------------------------------------------------------------------------------------------
   function Icon_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      return AWS.Response.File (AWS.MIME.Image_Icon, URI (URI'First + 1 .. URI'Last));
   end Icon_Callback;

   -------------------------------------------------------------------------------------------------
   -- Get_Rooms_List_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Rooms_List_Callback return AWS.Response.Data is
      Response : Unbounded_String :=
        To_Unbounded_String ("displayRoomNameAutocompletion([");
      Rooms_List        : constant Room_Name_Vectors.Vector := DB.Get_Rooms;
      Rooms_List_Cursor : Room_Name_Vectors.Cursor := Rooms_List.First;
   begin
      if Room_Name_Vectors.Has_Element (Rooms_List_Cursor) then
         Append
           (Response, """" & To_String (Room_Name_Vectors.Element (Rooms_List_Cursor)) & """");

         Room_Name_Vectors.Next (Rooms_List_Cursor);

         while Room_Name_Vectors.Has_Element (Rooms_List_Cursor) loop
            Append
              (Response, ",""" & To_String (Room_Name_Vectors.Element (Rooms_List_Cursor)) & """");

            Room_Name_Vectors.Next (Rooms_List_Cursor);
         end loop;
      end if;

      Append (Response, "])");

      return AWS.Response.Build (AWS.MIME.Text_Javascript, To_String (Response));
   end Get_Rooms_List_Callback;

   -------------------------------------------------------------------------------------------------
   -- Select_Room_From_URI
   -------------------------------------------------------------------------------------------------
   function Select_Room_From_URI (URI : in String) return Room.T_Room_Class_Access is
      Room_Name   : Unbounded_String := To_Unbounded_String (URI (URI'First + 1 .. URI'Last));
      Slash_Index : constant Natural := Index (URI (URI'First + 1 .. URI'Last), "/");

      Room_List_Cursor : Room_Vectors.Cursor := Rooms.First;
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
   end Select_Room_From_URI;

end Callback;
