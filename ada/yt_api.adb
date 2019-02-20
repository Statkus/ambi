with JSON.Streams;

with Ada.Text_IO; use Ada.Text_IO;

package body YT_API is

   -------------------------------------------------------------------------------------------------
   -- Set_YT_API_Key
   -------------------------------------------------------------------------------------------------
   procedure Set_YT_API_Key (Key : String) is
   begin
      YT_API_KEY := To_Unbounded_String (Key);
   end Set_YT_API_Key;

   -------------------------------------------------------------------------------------------------
   -- Get_Search_Request
   -------------------------------------------------------------------------------------------------
   function Get_Search_Request (Search_Input : in String) return String is
   begin
      return YT_API_URL & "search?key=" & To_String (YT_API_KEY)
        & "&part=snippet&q=" & Search_Input
        & "&videoDefinition=any&type=video&order=relevance&safeSearch=none&maxResult="
        & MAX_VIDEO_SEARCH_RESULTS'Img
        & "&videoEmbeddable=true&videoSyndicated=true";
   End Get_Search_Request;

   -------------------------------------------------------------------------------------------------
   -- Parse_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   function Parse_Video_Search_Results (Search_Results : in String) return T_Video_Search_Results is
      JSON_String_Reponse : aliased String := Search_Results;

      JSON_Stream    : JSON.Streams.Stream'Class :=
        JSON.Streams.Create_Stream (JSON_String_Reponse'Access);
      JSON_Allocator : Types.Memory_Allocator;
      JSON_Result    : constant Types.JSON_Value := Parsers.Parse (JSON_Stream, JSON_Allocator);

      Video_Search_Results       : T_Video_Search_Results;
      Video_Search_Results_Index : Integer := Video_Search_Results'First;
   begin
      for Item of JSON_Result.Get ("items") loop
         Video_Search_Results (Video_Search_Results_Index).Video_ID :=
           To_Unbounded_String (Item.Get ("id").Get ("videoId").Value);

         Video_Search_Results (Video_Search_Results_Index).Video_Title :=
           To_Unbounded_String (Item.Get ("snippet").Get ("title").Value);

         Video_Search_Results (Video_Search_Results_Index).Video_Image_URL :=
           To_Unbounded_String
             (Item.Get ("snippet").Get ("thumbnails").Get ("default").Get ("url").Value);

         Video_Search_Results_Index := Video_Search_Results_Index + 1;
      end loop;

      return Video_Search_Results;
   end Parse_Video_Search_Results;

end YT_API;
