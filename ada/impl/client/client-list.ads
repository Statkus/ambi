with Ada.Containers.Vectors;

package Client.List is

   package Client_Vectors is new Ada.Containers.Vectors (Natural, T_Client_Access);

   protected type T_Client_List is

      ----------------------------------------------------------------------------------------------
      -- Append
      ----------------------------------------------------------------------------------------------
      procedure Append (New_Client : in T_Client_Access);

      ----------------------------------------------------------------------------------------------
      -- Remove_Disconnected_Client
      ----------------------------------------------------------------------------------------------
      procedure Remove_Disconnected_Client (Last_Request_Time : in Ada.Real_Time.Time);

      ----------------------------------------------------------------------------------------------
      -- Add_Item_To_Playlist
      ----------------------------------------------------------------------------------------------
      procedure Add_Item_To_Playlist (Item : in Song.Item.T_Item);

      ----------------------------------------------------------------------------------------------
      -- Remove_Item_From_Playlist
      ----------------------------------------------------------------------------------------------
      procedure Remove_Item_From_Playlist (Item_Id : in Song.Item.T_Item_Id);

      ----------------------------------------------------------------------------------------------
      -- Up_Vote_Playlist_Item
      ----------------------------------------------------------------------------------------------
      procedure Up_Vote_Playlist_Item (Item_Id : in Song.Item.T_Item_Id);

      ----------------------------------------------------------------------------------------------
      -- Length
      ----------------------------------------------------------------------------------------------
      function Length return Natural;

      ----------------------------------------------------------------------------------------------
      -- Get_Client
      ----------------------------------------------------------------------------------------------
      function Get_Client (Session_Id : in Aws.Session.Id) return T_Client_Access;

      ----------------------------------------------------------------------------------------------
      -- Is_Auto_Playback_Requested
      ----------------------------------------------------------------------------------------------
      function Is_Auto_Playback_Requested return Boolean;

   private

      Client_List : Client_Vectors.Vector := Client_Vectors.Empty_Vector;

   end T_Client_List;

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   function Initialize return T_Client_List;

private

   Client_Session_Timeout : constant Duration := 60.0;

end Client.List;
