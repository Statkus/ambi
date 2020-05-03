package Web_Methods.Websocket is

   type T_Websocket is tagged limited null record;
   type T_Websocket_Class_Access is access all T_Websocket'Class;

   type T_Room_Request is
     (Update_Room_Current_Song,
      Update_Nb_Clients,
      Update_Playlist,
      Update_History,
      Update_Likes,
      Update_Suggestions,
      Clear_Search_Input,
      Force_Next_Song);

   -------------------------------------------------------------------------------------------------
   -- New_And_Initialize
   -------------------------------------------------------------------------------------------------
   function New_And_Initialize return T_Websocket_Class_Access;

   -------------------------------------------------------------------------------------------------
   -- Send_Room_Request
   -------------------------------------------------------------------------------------------------
   procedure Send_Room_Request
     (This      : in out T_Websocket;
      Room_Name : in     String;
      Request   : in     T_Room_Request);

end Web_Methods.Websocket;
