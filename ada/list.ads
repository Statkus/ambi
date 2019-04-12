with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Session;

package List is

   type T_Video is record
      Video_ID        : Unbounded_String := Null_Unbounded_String;
      Video_Title     : Unbounded_String := Null_Unbounded_String;
      Video_Thumbnail : Unbounded_String := Null_Unbounded_String;
   end record;

   type T_Playlist_Item_ID is mod 2**32;

   type T_Playlist_Item is record
      Video     : T_Video;
      ID        : T_Playlist_Item_ID;
      Client_ID : AWS.Session.ID;
      Up_Votes  : Natural := 0;
   end record;

   -- Dummy function to instantiate a vector, for now comparing T_Video records is useless
   function Video_Compare (Left, Right : in T_Video) return Boolean is (False);

   -- Dummy function to instantiate a vector, for now comparing T_Playlist_Item records is useless
   function Playlist_Item_Compare (Left, Right : in T_Playlist_Item) return Boolean is (False);

   package Video_Vectors is new Ada.Containers.Vectors (Natural, T_Video, Video_Compare);

   package Playlist_Vectors is new Ada.Containers.Vectors (Natural, T_Playlist_Item, Playlist_Item_Compare);

   package Room_Name_Vectors is new Ada.Containers.Vectors (Natural, Unbounded_String);

   procedure Up_Vote_Playlist_Item
     (Playlist : in out Playlist_Vectors.Vector; Item_ID : in T_Playlist_Item_ID);

end List;
