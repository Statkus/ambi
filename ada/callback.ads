with Ada.Containers.Vectors;

with AWS.Response;
with AWS.Status;

with Database;
with Room;

package Callback is

   procedure Set_Server_Address (Address : in String);

   procedure Set_Database (Ambi_Database : in not null Database.T_Database_Class_Access);

   function Ambi_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;

private

   function Javascripts_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function CSS_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Icon_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;

   function Select_Room_From_URI (URI : in String) return Room.T_Room_Class_Access;

   -- Dummy function to instantiate a vector, for now comparing T_Room type is useless
   function Room_Compare (Left, Right : Room.T_Room_Class_Access) return Boolean is (False);

   package Room_Vectors is new Ada.Containers.Vectors
     (Natural, Room.T_Room_Class_Access, Room_Compare);

   DB : Database.T_Database_Class_Access := null;

   Rooms : Room_Vectors.Vector := Room_Vectors.Empty_Vector;

end Callback;
