with Aws.Response;
with Aws.Status;

with Api.Dispatcher;
with Database;
with Room.List.Thread_Safe;
with Web_Methods.Websocket;

package Callback is

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   procedure Initialize (Ambi_Database : in not null Database.T_Database_Class_Access);

   -------------------------------------------------------------------------------------------------
   -- Callback
   -------------------------------------------------------------------------------------------------
   function Callback (Request : in Aws.Status.Data) return Aws.Response.Data;

private

   -------------------------------------------------------------------------------------------------
   -- Js_Callback
   -------------------------------------------------------------------------------------------------
   function Js_Callback (Request : in Aws.Status.Data) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Css_Callback
   -------------------------------------------------------------------------------------------------
   function Css_Callback (Request : in Aws.Status.Data) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Icon_Callback
   -------------------------------------------------------------------------------------------------
   function Icon_Callback (Request : in Aws.Status.Data) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Get_Room_List_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Room_List_Callback return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Select_Room_From_Uri
   -------------------------------------------------------------------------------------------------
   function Select_Room_From_Uri (Uri : in String) return Room.T_Room_Access;

   type T_Callback is record
      Db             : Database.T_Database_Class_Access;
      Api_Dispatcher : Api.Dispatcher.T_Dispatcher_Access;
      Websocket      : Web_Methods.Websocket.T_Websocket_Class_Access;
      Room_List      : Room.List.Thread_Safe.T_Thread_Safe_Room_List;
   end record;

   Cb : T_Callback;

end Callback;
