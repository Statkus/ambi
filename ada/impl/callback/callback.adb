with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Aws.Mime;

with Callback_Room;
with Room_Name_List;
with Sanitizer;

package body Callback is

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   procedure Initialize (Ambi_Database : in not null Database.T_Database_Class_Access) is
   begin
      Cb.Db             := Ambi_Database;
      Cb.Api_Dispatcher := Api.Dispatcher.New_And_Initialize;
      Cb.Websocket      := Web_Methods.Websocket.New_And_Initialize;

      Callback_Room.Initialize;
   end Initialize;

   -------------------------------------------------------------------------------------------------
   -- Callback
   -------------------------------------------------------------------------------------------------
   function Callback (Request : Aws.Status.Data) return Aws.Response.Data is
      use type Room.T_Room_Access;

      Uri : constant String := Aws.Status.Uri (Request);

      Current_Room : Room.T_Room_Access := null;

      Response : Aws.Response.Data;
   begin
      if Uri = "/" then
         Put_Line ("Request: '" & Uri & "'");
         Response := Aws.Response.File (Aws.Mime.Text_Html, "html/index.html");
      elsif Index (Uri, "/js/") > 0 then
         Put_Line ("Request: '" & Uri & "'");
         Response := Js_Callback (Request);
      elsif Index (Uri, "/css/") > 0 then
         Put_Line ("Request: '" & Uri & "'");
         Response := Css_Callback (Request);
      elsif Index (Uri, "/icon/") > 0 then
         Put_Line ("Request: '" & Uri & "'");
         Response := Icon_Callback (Request);
      elsif Uri = "/get_room_list" then
         Put_Line ("Request: '" & Uri & "'");
         Response := Get_Room_List_Callback;
      else
         Current_Room := Select_Room_From_Uri (Uri);

         if Current_Room /= null then
            -- Room request
            Response := Callback_Room.Callback (Request, Current_Room);
         elsif Aws.Status.Parameter (Request, "roomName") = Uri (Uri'First + 1 .. Uri'Last) and
           Sanitizer.Is_Room_Name_Sanitized (Uri (Uri'First + 1 .. Uri'Last))
         then
            -- Create new room
            Cb.Room_List.Append
              (Room.New_And_Initialize
                 (Name           => Uri (Uri'First + 1 .. Uri'Last),
                  Ambi_Database  => Cb.Db,
                  Api_Dispatcher => Cb.Api_Dispatcher,
                  Websocket      => Cb.Websocket));
            Current_Room := Cb.Room_List.Last_Element;

            Put_Line
              ("New room: " &
               Current_Room.Get_Name &
               ", number of rooms:" &
               Cb.Room_List.Length'Img);

            Response := Callback_Room.Callback (Request, Current_Room);
         else
            Put_Line ("Not supported request: '" & Uri & "'");
            Response := Aws.Response.Build (Aws.Mime.Text_Html, "");
         end if;
      end if;

      return Response;
   end Callback;

   -------------------------------------------------------------------------------------------------
   -- Js_Callback
   -------------------------------------------------------------------------------------------------
   function Js_Callback (Request : in Aws.Status.Data) return Aws.Response.Data is
      Uri : constant String := Aws.Status.Uri (Request);
   begin
      return Aws.Response.File (Aws.Mime.Text_Javascript, Uri (Uri'First + 1 .. Uri'Last));
   end Js_Callback;

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
   -- Get_Room_List_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Room_List_Callback return Aws.Response.Data is
      use Room_Name_List;

      Response : Unbounded_String := To_Unbounded_String ("displayRoomNameAutocompletion([");

      Room_List        : constant T_Room_Name_List := Cb.Db.Get_Rooms;
      Room_List_Cursor : Room_Name_Vectors.Cursor  := Room_List.First;
   begin
      if Room_Name_Vectors.Has_Element (Room_List_Cursor) then
         Append (Response, """" & To_String (Room_Name_Vectors.Element (Room_List_Cursor)) & """");

         Room_Name_Vectors.Next (Room_List_Cursor);

         while Room_Name_Vectors.Has_Element (Room_List_Cursor) loop
            Append
              (Response,
               ",""" & To_String (Room_Name_Vectors.Element (Room_List_Cursor)) & """");

            Room_Name_Vectors.Next (Room_List_Cursor);
         end loop;
      end if;

      Append (Response, "])");

      return Aws.Response.Build (Aws.Mime.Text_Javascript, To_String (Response));
   end Get_Room_List_Callback;

   -------------------------------------------------------------------------------------------------
   -- Select_Room_From_URI
   -------------------------------------------------------------------------------------------------
   function Select_Room_From_Uri (Uri : in String) return Room.T_Room_Access is
      Room_Name   : Unbounded_String := To_Unbounded_String (Uri (Uri'First + 1 .. Uri'Last));
      Slash_Index : constant Natural := Index (Uri (Uri'First + 1 .. Uri'Last), "/");
   begin
      if Slash_Index > 0 then
         Room_Name := To_Unbounded_String (Slice (Room_Name, 1, Slash_Index - 2));
      end if;

      return Cb.Room_List.Get_Room (To_String (Room_Name));
   end Select_Room_From_Uri;

end Callback;
