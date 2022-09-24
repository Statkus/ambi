with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aws.Response;
with Aws.Status;

with Client;
with Room;
with Song.List;

package Callback_Room is

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   procedure Initialize;

   -------------------------------------------------------------------------------------------------
   -- Callback
   -------------------------------------------------------------------------------------------------
   function Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

private

   type T_Song_List_Source is (Playlist, History, Likes, Suggestions);

   type T_Parameter is
     (Param_Search_Input,
      Param_Song_Id,
      Param_Song_Title,
      Param_Song_Thumbnail_Link,
      Param_Item_Id,
      Param_Liked,
      Param_Checked,
      Param_Source,
      Param_Message,
      Param_Player);

   type T_Placeholder is
     (Ph_Search_Results,
      Ph_Song_List,
      Ph_Suggestions_List,
      Ph_Current_Room_Song,
      Ph_Next_Room_Song_Votes,
      Ph_Nb_Clients,
      Ph_Chat_Log);

   type T_Thtml_Field is
     (Room_Name,
      Room_Song,
      Next_Room_Song,
      Next_Song_Votes,
      Nb_Clients,
      Player_State,
      Player_Checkbox,
      Room_Script,
      Player_Script,
      Song_List,
      Suggestions_List,
      Display_Next_Suggested_Song,
      Client_Sync,
      Server_Address,
      Song_Id,
      Song_Title,
      Song_Thumbnail_Link,
      Item_Id,
      Source_Client,
      Up_Votes,
      Liked,
      Chat_Log);

   type T_Xml_Field is (Placeholder, Value, Action_Fields);

   -------------------------------------------------------------------------------------------------
   -- Room_Page_Callback
   -------------------------------------------------------------------------------------------------
   function Room_Page_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Search_Button_Callback
   -------------------------------------------------------------------------------------------------
   function Search_Button_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Add_To_Playlist_Callback
   -------------------------------------------------------------------------------------------------
   function Add_To_Playlist_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Remove_From_Playlist_Callback
   -------------------------------------------------------------------------------------------------
   function Remove_From_Playlist_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Add_Remove_Like_Callback
   -------------------------------------------------------------------------------------------------
   function Add_Remove_Like_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Player_Display_Checkbox_Callback
   -------------------------------------------------------------------------------------------------
   function Player_Display_Checkbox_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Player_Sync_Checkbox_Callback
   -------------------------------------------------------------------------------------------------
   function Player_Sync_Checkbox_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Next_Room_Song_Callback
   -------------------------------------------------------------------------------------------------
   function Next_Room_Song_Callback
     (Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Next_Cient_Song_Callback
   -------------------------------------------------------------------------------------------------
   function Next_Client_Song_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Up_Vote_Callback
   -------------------------------------------------------------------------------------------------
   function Up_Vote_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Get_Song_List_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Song_List_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Get_Current_Room_Song_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Current_Room_Song_Callback
     (Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Get_Next_Song_Votes_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Next_Song_Votes_Callback
     (Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Get_Number_Of_Clients_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Number_Of_Clients_Callback
     (Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Get_Chat_Log_Callback
   -------------------------------------------------------------------------------------------------
   function Get_Chat_Log_Callback
     (Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Add_Chat_Message_Callback
   -------------------------------------------------------------------------------------------------
   function Add_Chat_Message_Callback
     (Request      : in Aws.Status.Data;
      Current_Room : in not null Room.T_Room_Access) return Aws.Response.Data;

   -------------------------------------------------------------------------------------------------
   -- Build_Search_Results
   -------------------------------------------------------------------------------------------------
   function Build_Search_Results (Song_Search_Results : in Song.List.T_Song_List) return String;

   -------------------------------------------------------------------------------------------------
   -- Build_Playlist
   -------------------------------------------------------------------------------------------------
   function Build_Playlist
     (Current_Room   : in not null Room.T_Room_Access;
      Current_Client : in not null Client.T_Client_Access) return String;

   -------------------------------------------------------------------------------------------------
   -- Build_Empty_Playlist
   -------------------------------------------------------------------------------------------------
   function Build_Empty_Playlist
     (Current_Room   : in not null Room.T_Room_Access;
      Current_Client : in not null Client.T_Client_Access) return String;

   -------------------------------------------------------------------------------------------------
   -- Build_Song_List
   -------------------------------------------------------------------------------------------------
   function Build_Song_List
     (Current_Room : in not null Room.T_Room_Access;
      Source       : in T_Song_List_Source) return String;

   -------------------------------------------------------------------------------------------------
   -- Pack_Ajax_Xml_Response
   -------------------------------------------------------------------------------------------------
   function Pack_Ajax_Xml_Response
     (Xml_Placeholder : in String;
      Xml_Value       : in String) return String;

   -------------------------------------------------------------------------------------------------
   -- To_Parameter_String
   -------------------------------------------------------------------------------------------------
   function To_Parameter_String (Parameter : in T_Parameter) return String;

   -------------------------------------------------------------------------------------------------
   -- To_Placeholder_String
   -------------------------------------------------------------------------------------------------
   function To_Placeholder_String (Xml_Placeholder : in T_Placeholder) return String;

   Server_Ip : Unbounded_String;

end Callback_Room;
