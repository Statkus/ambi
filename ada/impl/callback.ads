with Ada.Containers.Vectors;

with Aws.Response;
with Aws.Status;

with Api.Dispatcher;
with Database;
with Room;

package Callback is

   procedure Set_Server_Address (Address : in String);

   procedure Set_Database (Ambi_Database : in not null Database.T_Database_Class_Access);

   function Ambi_Callback (Request : in Aws.Status.Data) return Aws.Response.Data;

private

   function Javascripts_Callback (Request : in Aws.Status.Data) return Aws.Response.Data;
   function Css_Callback (Request : in Aws.Status.Data) return Aws.Response.Data;
   function Icon_Callback (Request : in Aws.Status.Data) return Aws.Response.Data;
   function Get_Rooms_List_Callback return Aws.Response.Data;

   function Select_Room_From_Uri (Uri : in String) return Room.T_Room_Class_Access;

   -- Dummy function to instantiate a vector, for now comparing T_Room type is useless
   function Room_Compare (Left, Right : Room.T_Room_Class_Access) return Boolean is (False);

   package Room_Vectors is new Ada.Containers.Vectors
     (Natural,
      Room.T_Room_Class_Access,
      Room_Compare);

   Db : Database.T_Database_Class_Access := null;

   Rooms : Room_Vectors.Vector := Room_Vectors.Empty_Vector;

   Api_Dispatcher : Api.Dispatcher.T_Dispatcher_Access :=
     Api.Dispatcher.Constructors.New_And_Initialize;

end Callback;
