with Ada.Unchecked_Deallocation;

package body Client.List is

   -------------------------------------------------------------------------------------------------
   -- Free_Client
   -------------------------------------------------------------------------------------------------
   procedure Free_Client is new Ada.Unchecked_Deallocation (T_Client, T_Client_Access);

   protected body T_Client_List is

      ----------------------------------------------------------------------------------------------
      -- Append
      ----------------------------------------------------------------------------------------------
      procedure Append (New_Client : in T_Client_Access) is
      begin
         Client_List.Append (New_Client);
      end Append;

      ----------------------------------------------------------------------------------------------
      -- Remove_Disconnected_Client
      ----------------------------------------------------------------------------------------------
      procedure Remove_Disconnected_Client (Last_Request_Time : in Ada.Real_Time.Time) is
         use type Ada.Real_Time.Time;
         use type Client_Vectors.Cursor;

         Client_List_Cursor : Client_Vectors.Cursor := Client_List.First;
         Client_To_Remove   : T_Client_Access       := null;
      begin
         while Client_Vectors.Has_Element (Client_List_Cursor) loop
            if not Aws.Session.Exist (Client_Vectors.Element (Client_List_Cursor).Get_Id)
              or else
              (Last_Request_Time > Client_Vectors.Element (Client_List_Cursor).Get_Last_Request_Time
               and then
                 Ada.Real_Time.To_Duration
                   (Last_Request_Time -
                    Client_Vectors.Element (Client_List_Cursor).Get_Last_Request_Time) >
                 Client_Session_Timeout)
            then
               Client_To_Remove := Client_Vectors.Element (Client_List_Cursor);
               Client_List.Delete (Client_List_Cursor);

               Free_Client (Client_To_Remove);
            end if;

            if Client_List_Cursor = Client_Vectors.No_Element then
               Client_List_Cursor := Client_List.First;
            else
               Client_Vectors.Next (Client_List_Cursor);
            end if;
         end loop;
      end Remove_Disconnected_Client;

      ----------------------------------------------------------------------------------------------
      -- Add_Item_To_Playlist
      ----------------------------------------------------------------------------------------------
      procedure Add_Item_To_Playlist (Item : in Song.Item.T_Item) is
         -------------------------------------------------------------------------------------------
         -- Add_Item_To_Playlist_At_Position
         -------------------------------------------------------------------------------------------
         procedure Add_Item_To_Playlist_At_Position (Position : in Client_Vectors.Cursor) is
         begin
            Client_Vectors.Element (Position).Add_Item_To_Playlist (Item);
         end Add_Item_To_Playlist_At_Position;
      begin
         Client_List.Iterate (Add_Item_To_Playlist_At_Position'Access);
      end Add_Item_To_Playlist;

      ----------------------------------------------------------------------------------------------
      -- Remove_Item_From_Playlist
      ----------------------------------------------------------------------------------------------
      procedure Remove_Item_From_Playlist (Item_Id : in Song.Item.T_Item_Id) is
         -------------------------------------------------------------------------------------------
         -- Remove_Item_From_Playlist_At_Position
         -------------------------------------------------------------------------------------------
         procedure Remove_Item_From_Playlist_At_Position (Position : in Client_Vectors.Cursor) is
         begin
            Client_Vectors.Element (Position).Remove_Item_From_Playlist (Item_Id);
         end Remove_Item_From_Playlist_At_Position;
      begin
         Client_List.Iterate (Remove_Item_From_Playlist_At_Position'Access);
      end Remove_Item_From_Playlist;

      ----------------------------------------------------------------------------------------------
      -- Up_Vote_Playlist_Item
      ----------------------------------------------------------------------------------------------
      procedure Up_Vote_Playlist_Item (Item_Id : in Song.Item.T_Item_Id) is
         -------------------------------------------------------------------------------------------
         -- Up_Vote_Playlist_Item_At_Position
         -------------------------------------------------------------------------------------------
         procedure Up_Vote_Playlist_Item_At_Position (Position : in Client_Vectors.Cursor) is
         begin
            Client_Vectors.Element (Position).Up_Vote_Playlist_Item (Item_Id);
         end Up_Vote_Playlist_Item_At_Position;
      begin
         Client_List.Iterate (Up_Vote_Playlist_Item_At_Position'Access);
      end Up_Vote_Playlist_Item;

      ----------------------------------------------------------------------------------------------
      -- Length
      ----------------------------------------------------------------------------------------------
      function Length return Natural is (Natural (Client_List.Length));

      ----------------------------------------------------------------------------------------------
      -- Get_Client
      ----------------------------------------------------------------------------------------------
      function Get_Client (Session_Id : in Aws.Session.Id) return T_Client_Access is
         use type Aws.Session.Id;

         Client_List_Cursor : Client_Vectors.Cursor  := Client_List.First;
         Requested_Client   : Client.T_Client_Access := null;
      begin
         while Client_Vectors.Has_Element (Client_List_Cursor) and Requested_Client = null loop
            if Client_Vectors.Element (Client_List_Cursor).Get_Id = Session_Id then
               Requested_Client := Client_Vectors.Element (Client_List_Cursor);
            end if;

            Client_Vectors.Next (Client_List_Cursor);
         end loop;

         return Requested_Client;
      end Get_Client;

      ----------------------------------------------------------------------------------------------
      -- Is_Auto_Playback_Requested
      ----------------------------------------------------------------------------------------------
      function Is_Auto_Playback_Requested return Boolean is
         Client_List_Cursor      : Client_Vectors.Cursor := Client_List.First;
         Auto_Playback_Requested : Boolean               := False;
      begin
         while Client_Vectors.Has_Element (Client_List_Cursor) and not Auto_Playback_Requested loop
            if Client_Vectors.Element (Client_List_Cursor).Is_Sync_With_Room and
              Client_Vectors.Element (Client_List_Cursor).Is_Player_Displayed
            then
               Auto_Playback_Requested := True;
            end if;

            Client_Vectors.Next (Client_List_Cursor);
         end loop;

         return Auto_Playback_Requested;
      end Is_Auto_Playback_Requested;

   end T_Client_List;

   -------------------------------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------------------------------
   function Initialize return T_Client_List is
   begin
      return Client_List : T_Client_List do
         null;
      end return;
   end Initialize;

end Client.List;
