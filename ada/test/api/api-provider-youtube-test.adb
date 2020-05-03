with Aunit.Assertions; use Aunit.Assertions;

with File; use File;

with Web_Methods.Http;
with Web_Methods.Http.Mock;

package body Api.Provider.Youtube.Test is

   use Song.List;

   -------------------------------------------------------------------------------------------------
   -- Register_Tests
   -------------------------------------------------------------------------------------------------
   procedure Register_Tests (This : in out T_Youtube_Test_Case) is
      use Aunit.Test_Cases.Registration;
   begin
      Register_Routine (This, Test_New_And_Initialize'Access, "Test New_And_Initialize");

      Register_Routine (This, Test_Get_Song_Search_Results'Access, "Test Get_Song_Search_Results");

      Register_Routine (This, Test_Get_Song_Duration'Access, "Test Get_Song_Duration");

      Register_Routine (This, Test_Get_Related_Songs'Access, "Test Get_Related_Songs");

      Register_Routine (This, Test_Get_Playlist'Access, "Test Get_Playlist");

      Register_Routine (This, Test_Format_Requests'Access, "Test Format_Requests");

      Register_Routine (This, Test_Get_Request_Response'Access, "Test Get_Request_Response");

      Register_Routine
        (This,
         Test_Parse_Video_Search_Results'Access,
         "Test Parse_Video_Search_Results");

      Register_Routine
        (This,
         Test_Parse_Playlist_Item_Results'Access,
         "Test Parse_Playlist_Item_Results");

      Register_Routine
        (This,
         Test_Parse_Video_Duration_Result'Access,
         "Test Parse_Video_Duration_Result");

      Register_Routine
        (This,
         Test_Convert_Iso_8601_Duration_To_Seconds'Access,
         "Test Convert_Iso_8601_Duration_To_Seconds");
   end Register_Tests;

   -------------------------------------------------------------------------------------------------
   -- Name
   -------------------------------------------------------------------------------------------------
   function Name (This : in T_Youtube_Test_Case) return Test_String is
      pragma Unreferenced (This);

   begin
      return Format ("Api.Provider.Youtube tests");
   end Name;

   -------------------------------------------------------------------------------------------------
   -- Test_New_And_Initialize
   -------------------------------------------------------------------------------------------------
   procedure Test_New_And_Initialize (Test_Case : in out Test_Cases.Test_Case'Class) is
      use type Web_Methods.Http.T_Http_Class_Access;

      pragma Unreferenced (Test_Case);

      Http_Accessor : constant Web_Methods.Http.T_Http_Class_Access :=
        Web_Methods.Http.New_And_Initialize;
      Yt_Api : constant T_Youtube_Access := New_And_Initialize ("test_key", Http_Accessor);
   begin
      Assert (Yt_Api.Api_Key = "test_key", "Wrong API key.");
      Assert (Yt_Api.Http_Accessor /= null, "Null HTTP accessor given.");
   end Test_New_And_Initialize;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Song_Search_Results
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Song_Search_Results (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Http_Accessor_Mock : constant Web_Methods.Http.Mock.T_Http_Mock_Access :=
        new Web_Methods.Http.Mock.T_Http_Mock;

      Yt_Api : constant T_Youtube_Access :=
        New_And_Initialize ("test_key", Web_Methods.Http.T_Http_Class_Access (Http_Accessor_Mock));

      Search_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_search_video_list.json");
      Single_Page_Playlist_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_playlist_item_list.json");
      Wrong_Json : constant String := "";

      Videos      : T_Song_List := Song.List.Initialize;
      Search_Type : T_Search_Type;
   begin
      ----------------------------------------------------------------------------------------------
      -- Words search type correct JSON
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Search_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos := Yt_Api.Get_Song_Search_Results ("test_1", Search_Type);

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/search?key=test_key&q=test_1&maxResults=10&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");

      Assert (Search_Type = Words, "Wrong search type returned.");
      Assert (Natural (Videos.Length) = 10, "Wrong number of videos returned.");

      -- Check first video info
      Assert (Song_Vectors.Element (Videos.First).Get_Id = "sEXzii5scvg", "Wrong first video ID.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Title =
         "TEST EN CARTON #73 - Animal Crossing : New Horizons",
         "Wrong first video title.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/sEXzii5scvg/default.jpg",
         "Wrong first video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Provider = Api.Youtube_Api,
         "Wrong first video provider.");

      -- Check last video info
      Assert (Song_Vectors.Element (Videos.Last).Get_Id = "3J0VZ6JX60w", "Wrong last video ID.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Title =
         "10 Choix Les Plus Difficiles (Test de Personnalité)",
         "Wrong last video title.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/3J0VZ6JX60w/default.jpg",
         "Wrong last video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last video provider.");

      ----------------------------------------------------------------------------------------------
      -- Words search type wrong JSON
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Wrong_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos := Yt_Api.Get_Song_Search_Results ("test_2", Search_Type);

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/search?key=test_key&q=test_2&maxResults=10&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");

      Assert (Search_Type = Words, "Wrong search type returned.");
      Assert (Natural (Videos.Length) = 0, "Wrong number of videos returned.");

      ----------------------------------------------------------------------------------------------
      -- Video link search type correct JSON
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Search_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos :=
        Yt_Api.Get_Song_Search_Results ("https://www.youtube.com/watch?v=sEXzii5scvg", Search_Type);

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/search?key=test_key&q=sEXzii5scvg&maxResults=10&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");

      Assert (Search_Type = Video_Link, "Wrong search type returned.");
      Assert (Natural (Videos.Length) = 1, "Wrong number of videos returned.");

      -- Check video info
      Assert (Song_Vectors.Element (Videos.First).Get_Id = "sEXzii5scvg", "Wrong video ID.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Title =
         "TEST EN CARTON #73 - Animal Crossing : New Horizons",
         "Wrong video title.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/sEXzii5scvg/default.jpg",
         "Wrong video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Provider = Api.Youtube_Api,
         "Wrong video provider.");

      ----------------------------------------------------------------------------------------------
      -- Video link search type wrong JSON
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Wrong_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos :=
        Yt_Api.Get_Song_Search_Results ("https://www.youtube.com/watch?v=sEXzii5scvg", Search_Type);

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/search?key=test_key&q=sEXzii5scvg&maxResults=10&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");

      Assert (Search_Type = Video_Link, "Wrong search type returned.");
      Assert (Natural (Videos.Length) = 0, "Wrong number of videos returned.");

      ----------------------------------------------------------------------------------------------
      -- Playlist link search type correct JSON
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Single_Page_Playlist_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos :=
        Yt_Api.Get_Song_Search_Results
        ("https://www.youtube.com/playlist?list=OLAK5uy_nF2626M3nvAFVKDgSHbkG0JkUeDe3LNJ8", Search_Type);

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/playlistItems?key=test_key&playlistId=OLAK5uy_nF2626M3nvAFVKDgSHbkG0JkUeDe3LNJ8&pageToken=&maxResults=50&part=snippet",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");

      Assert (Search_Type = Playlist_Link, "Wrong search type returned.");
      Assert (Natural (Videos.Length) = 3, "Wrong number of videos returned.");

      -- Check first video info
      Assert (Song_Vectors.Element (Videos.First).Get_Id = "QdabIfmcqSQ", "Wrong first video ID.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Title =
         "Caravan Palace - Miracle (official video)",
         "Wrong first video title.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/QdabIfmcqSQ/default.jpg",
         "Wrong first video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Provider = Api.Youtube_Api,
         "Wrong first video provider.");

      -- Check last video info
      Assert (Song_Vectors.Element (Videos.Last).Get_Id = "IGBxWcu6iXE", "Wrong last video ID.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Title = "Miracle (Boogie Belgique Remix)",
         "Wrong last video title.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/IGBxWcu6iXE/default.jpg",
         "Wrong last video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last video provider.");

      ----------------------------------------------------------------------------------------------
      -- Playlist link search type wrong JSON
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Wrong_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos :=
        Yt_Api.Get_Song_Search_Results
        ("https://www.youtube.com/playlist?list=OLAK5uy_nF2626M3nvAFVKDgSHbkG0JkUeDe3LNJ8", Search_Type);

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/playlistItems?key=test_key&playlistId=OLAK5uy_nF2626M3nvAFVKDgSHbkG0JkUeDe3LNJ8&pageToken=&maxResults=50&part=snippet",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");

      Assert (Search_Type = Playlist_Link, "Wrong search type returned.");
      Assert (Natural (Videos.Length) = 0, "Wrong number of videos returned.");
   end Test_Get_Song_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Song_Duration
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Song_Duration (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Http_Accessor_Mock : constant Web_Methods.Http.Mock.T_Http_Mock_Access :=
        new Web_Methods.Http.Mock.T_Http_Mock;

      Yt_Api : constant T_Youtube_Access :=
        New_And_Initialize ("test_key", Web_Methods.Http.T_Http_Class_Access (Http_Accessor_Mock));

      Correct_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_video_content_details.json");
      Wrong_Json : constant String := "";

      Video : constant Song.T_Song :=
        Song.Initialize
          (Id             => "UbQgXeY_zi4",
           Title          => "test",
           Thumbnail_Link => "test",
           Provider       => Api.Youtube_Api);
   begin
      ----------------------------------------------------------------------------------------------
      -- Correct JSON
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Correct_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Assert (Yt_Api.Get_Song_Duration (Video) = 171, "Wrong duration returned.");

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/videos?key=test_key&id=UbQgXeY_zi4&part=contentDetails",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");

      ----------------------------------------------------------------------------------------------
      -- Wrong JSON
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Wrong_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Assert (Yt_Api.Get_Song_Duration (Video) = 0, "Wrong duration returned.");

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/videos?key=test_key&id=UbQgXeY_zi4&part=contentDetails",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");
   end Test_Get_Song_Duration;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Related_Songs
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Related_Songs (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Http_Accessor_Mock : constant Web_Methods.Http.Mock.T_Http_Mock_Access :=
        new Web_Methods.Http.Mock.T_Http_Mock;

      Yt_Api : constant T_Youtube_Access :=
        New_And_Initialize ("test_key", Web_Methods.Http.T_Http_Class_Access (Http_Accessor_Mock));

      Correct_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_search_video_list.json");
      Wrong_Json : constant String := "";

      Video : constant Song.T_Song :=
        Song.Initialize
          (Id             => "test",
           Title          => "test",
           Thumbnail_Link => "test",
           Provider       => Api.Youtube_Api);
      Videos : T_Song_List := Song.List.Initialize;
   begin
      ----------------------------------------------------------------------------------------------
      -- Correct JSON
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Correct_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos := Yt_Api.Get_Related_Songs (Video);

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/search?key=test_key&relatedToVideoId=test&maxResults=20&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");
      Assert (Natural (Videos.Length) = 10, "Wrong number of videos returned.");

      -- Check first video info
      Assert (Song_Vectors.Element (Videos.First).Get_Id = "sEXzii5scvg", "Wrong first video ID.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Title =
         "TEST EN CARTON #73 - Animal Crossing : New Horizons",
         "Wrong first video title.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/sEXzii5scvg/default.jpg",
         "Wrong first video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Provider = Api.Youtube_Api,
         "Wrong first video provider.");

      -- Check last video info
      Assert (Song_Vectors.Element (Videos.Last).Get_Id = "3J0VZ6JX60w", "Wrong last video ID.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Title =
         "10 Choix Les Plus Difficiles (Test de Personnalité)",
         "Wrong last video title.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/3J0VZ6JX60w/default.jpg",
         "Wrong last video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last video provider.");

      ----------------------------------------------------------------------------------------------
      -- Wrong JSON
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Wrong_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos := Yt_Api.Get_Related_Songs (Video);

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/search?key=test_key&relatedToVideoId=test&maxResults=20&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");
      Assert (Natural (Videos.Length) = 0, "Wrong number of videos returned.");
   end Test_Get_Related_Songs;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Playlist
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Playlist (Test_Case : in out Aunit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Http_Accessor_Mock : constant Web_Methods.Http.Mock.T_Http_Mock_Access :=
        new Web_Methods.Http.Mock.T_Http_Mock;

      Yt_Api : constant T_Youtube_Access :=
        New_And_Initialize ("test_key", Web_Methods.Http.T_Http_Class_Access (Http_Accessor_Mock));

      Single_Page_Playlist_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_playlist_item_list.json");
      Multiple_Pages_Playlist_1_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_playlist_item_list_multiple_pages_1.json");
      Multiple_Pages_Playlist_2_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_playlist_item_list_multiple_pages_2.json");
      Multiple_Pages_Playlist_3_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_playlist_item_list_multiple_pages_3.json");
      Wrong_Json : constant String := "";

      Multiple_Pages_Playlist_Json : Web_Methods.Http.Mock.T_Get_Response_Vector;

      Videos : T_Song_List := Song.List.Initialize;
   begin
      ----------------------------------------------------------------------------------------------
      -- Single page playlist
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Single_Page_Playlist_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos := Yt_Api.Get_Playlist ("OLAK5uy_nF2626M3nvAFVKDgSHbkG0JkUeDe3LNJ8");

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/playlistItems?key=test_key&playlistId=OLAK5uy_nF2626M3nvAFVKDgSHbkG0JkUeDe3LNJ8&pageToken=&maxResults=50&part=snippet",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");
      Assert (Natural (Videos.Length) = 3, "Wrong number of videos returned.");

      -- Check first video info
      Assert (Song_Vectors.Element (Videos.First).Get_Id = "QdabIfmcqSQ", "Wrong first video ID.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Title =
         "Caravan Palace - Miracle (official video)",
         "Wrong first video title.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/QdabIfmcqSQ/default.jpg",
         "Wrong first video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Provider = Api.Youtube_Api,
         "Wrong first video provider.");

      -- Check last video info
      Assert (Song_Vectors.Element (Videos.Last).Get_Id = "IGBxWcu6iXE", "Wrong last video ID.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Title = "Miracle (Boogie Belgique Remix)",
         "Wrong last video title.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/IGBxWcu6iXE/default.jpg",
         "Wrong last video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last video provider.");

      ----------------------------------------------------------------------------------------------
      -- Multiple pages playlist
      ----------------------------------------------------------------------------------------------
      Multiple_Pages_Playlist_Json.Append (To_Unbounded_String (Multiple_Pages_Playlist_1_Json));
      Multiple_Pages_Playlist_Json.Append (To_Unbounded_String (Multiple_Pages_Playlist_2_Json));
      Multiple_Pages_Playlist_Json.Append (To_Unbounded_String (Multiple_Pages_Playlist_3_Json));

      Http_Accessor_Mock.Set_Multiple_Get_Response (Multiple_Pages_Playlist_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos := Yt_Api.Get_Playlist ("PLn_i-d2YJISzgLoYI8KFVn2ucdTgFmNHQ");

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/playlistItems?key=test_key&playlistId=PLn_i-d2YJISzgLoYI8KFVn2ucdTgFmNHQ&pageToken=CGQQAA&maxResults=50&part=snippet",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 3, "Wrong number of Get called.");
      Assert (Natural (Videos.Length) = 148, "Wrong number of videos returned.");

      -- Check first video info
      Assert (Song_Vectors.Element (Videos.First).Get_Id = "e0r2Apn7OKA", "Wrong first video ID.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Title = """A"" - Nothing",
         "Wrong first video title.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/e0r2Apn7OKA/default.jpg",
         "Wrong first video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Provider = Api.Youtube_Api,
         "Wrong first video provider.");

      -- Check last video info
      Assert (Song_Vectors.Element (Videos.Last).Get_Id = "1naP9GLz0iI", "Wrong last video ID.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Title = "A New Beginning",
         "Wrong last video title.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/1naP9GLz0iI/default.jpg",
         "Wrong last video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last video provider.");

      ----------------------------------------------------------------------------------------------
      -- Wrong JSON returned
      ----------------------------------------------------------------------------------------------
      Http_Accessor_Mock.Set_Get_Response (Wrong_Json);
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Videos := Yt_Api.Get_Playlist ("PLn_i-d2YJISzgLoYI8KFVn2ucdTgFmNHQ");

      Assert
        (Http_Accessor_Mock.Get_Get_Url =
         "https://www.googleapis.com/youtube/v3/playlistItems?key=test_key&playlistId=PLn_i-d2YJISzgLoYI8KFVn2ucdTgFmNHQ&pageToken=&maxResults=50&part=snippet",
         "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");
      Assert (Natural (Videos.Length) = 0, "Wrong number of videos returned.");
   end Test_Get_Playlist;

   -------------------------------------------------------------------------------------------------
   -- Test_Format_Requests
   -------------------------------------------------------------------------------------------------
   procedure Test_Format_Requests (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Http_Accessor : constant Web_Methods.Http.T_Http_Class_Access := new Web_Methods.Http.T_Http;
      Yt_Api        : constant T_Youtube_Access := New_And_Initialize ("test_key", Http_Accessor);
   begin
      Assert
        (Yt_Api.Format_Search_Request ("test") =
         "https://www.googleapis.com/youtube/v3/search?key=test_key&q=test&maxResults=10&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true",
         "Wrong URL request.");

      Assert
        (Yt_Api.Format_Video_Request ("test") =
         "https://www.googleapis.com/youtube/v3/videos?key=test_key&id=test&part=contentDetails",
         "Wrong URL request.");

      Assert
        (Yt_Api.Format_Videos_Related_Request ("test") =
         "https://www.googleapis.com/youtube/v3/search?key=test_key&relatedToVideoId=test&maxResults=20&part=snippet&videoDefinition=any&type=video&safeSearch=none&videoEmbeddable=true",
         "Wrong URL request.");

      Assert
        (Yt_Api.Format_Playlist_Items_Request ("test", "page_1") =
         "https://www.googleapis.com/youtube/v3/playlistItems?key=test_key&playlistId=test&pageToken=page_1&maxResults=50&part=snippet",
         "Wrong URL request.");
   end Test_Format_Requests;

   -------------------------------------------------------------------------------------------------
   -- Test_Get_Request_Response
   -------------------------------------------------------------------------------------------------
   procedure Test_Get_Request_Response (Test_Case : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test_Case);

      Http_Accessor_Mock : constant Web_Methods.Http.Mock.T_Http_Mock_Access :=
        new Web_Methods.Http.Mock.T_Http_Mock;

      Yt_Api : constant T_Youtube_Access :=
        New_And_Initialize ("test_key", Web_Methods.Http.T_Http_Class_Access (Http_Accessor_Mock));
   begin
      Http_Accessor_Mock.Set_Get_Response ("test");
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Assert (Yt_Api.Get_Request_Response ("test_1") = "test", "Wrong response received.");
      Assert (Http_Accessor_Mock.Get_Get_Url = "test_1", "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 1, "Wrong number of Get called.");

      Http_Accessor_Mock.Set_Get_Response ("GET request error.");
      Http_Accessor_Mock.Reset_Number_Of_Get_Called;

      Assert
        (Yt_Api.Get_Request_Response ("test_2") = "GET request error.",
         "Wrong response received.");
      Assert (Http_Accessor_Mock.Get_Get_Url = "test_2", "Wrong URL given.");
      Assert (Http_Accessor_Mock.Get_Number_Of_Get_Called = 10, "Wrong number of Get called.");
   end Test_Get_Request_Response;

   -------------------------------------------------------------------------------------------------
   -- Test_Parse_Video_Search_Results
   -------------------------------------------------------------------------------------------------
   procedure Test_Parse_Video_Search_Results
     (Test_Case : in out Aunit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Test_Case);

      Correct_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_search_video_list.json");
      Wrong_Json : constant String := "";

      Videos : T_Song_List := Song.List.Initialize;
   begin
      Videos := Parse_Video_Search_Results (Correct_Json);

      Assert (Natural (Videos.Length) = 10, "Wrong number of videos returned.");

      -- Check first video info
      Assert (Song_Vectors.Element (Videos.First).Get_Id = "sEXzii5scvg", "Wrong first video ID.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Title =
         "TEST EN CARTON #73 - Animal Crossing : New Horizons",
         "Wrong first video title.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/sEXzii5scvg/default.jpg",
         "Wrong first video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Provider = Api.Youtube_Api,
         "Wrong first video provider.");

      -- Check last video info
      Assert (Song_Vectors.Element (Videos.Last).Get_Id = "3J0VZ6JX60w", "Wrong last video ID.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Title =
         "10 Choix Les Plus Difficiles (Test de Personnalité)",
         "Wrong last video title.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/3J0VZ6JX60w/default.jpg",
         "Wrong last video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last video provider.");

      Videos := Parse_Video_Search_Results (Wrong_Json);

      Assert (Natural (Videos.Length) = 0, "Wrong number of videos returned.");
   end Test_Parse_Video_Search_Results;

   -------------------------------------------------------------------------------------------------
   -- Test_Parse_Playlist_Item_Results
   -------------------------------------------------------------------------------------------------
   procedure Test_Parse_Playlist_Item_Results
     (Test_Case : in out Aunit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Test_Case);

      Correct_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_playlist_item_list.json");
      Correct_Json_String_Multiple_Pages : constant String :=
        Read_File ("ada/test/json_samples/youtube_playlist_item_list_multiple_pages.json");
      Correct_Json_String_Unavailable_Video : constant String :=
        Read_File ("ada/test/json_samples/youtube_playlist_item_list_unavailable_video.json");
      Wrong_Json : constant String := "";

      Videos             : T_Song_List := Song.List.Initialize;
      Total_Results      : Natural;
      Next_Page_Token    : Unbounded_String;
      Unavailable_Videos : Natural     := Natural'First;
   begin
      ----------------------------------------------------------------------------------------------
      -- Single page playlist
      ----------------------------------------------------------------------------------------------
      Videos :=
        Parse_Playlist_Item_Results
          (Correct_Json,
           Total_Results,
           Next_Page_Token,
           Unavailable_Videos);

      -- Check control variables
      Assert (Natural (Videos.Length) = 3, "Wrong number of videos returned.");
      Assert (Total_Results = 3, "Wrong number of total videos.");
      Assert (To_String (Next_Page_Token) = "", "Wrong next page token.");
      Assert (Unavailable_Videos = 0, "Wrong unavailable videos.");

      -- Check first video info
      Assert (Song_Vectors.Element (Videos.First).Get_Id = "QdabIfmcqSQ", "Wrong first video ID.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Title =
         "Caravan Palace - Miracle (official video)",
         "Wrong first video title.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/QdabIfmcqSQ/default.jpg",
         "Wrong first video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Provider = Api.Youtube_Api,
         "Wrong first video provider.");

      -- Check last video info
      Assert (Song_Vectors.Element (Videos.Last).Get_Id = "IGBxWcu6iXE", "Wrong last video ID.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Title = "Miracle (Boogie Belgique Remix)",
         "Wrong last video title.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/IGBxWcu6iXE/default.jpg",
         "Wrong last video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last video provider.");

      ----------------------------------------------------------------------------------------------
      -- Multiple pages playlist
      ----------------------------------------------------------------------------------------------
      Unavailable_Videos := Natural'First;

      Videos :=
        Parse_Playlist_Item_Results
          (Correct_Json_String_Multiple_Pages,
           Total_Results,
           Next_Page_Token,
           Unavailable_Videos);

      -- Check control variables
      Assert (Natural (Videos.Length) = 5, "Wrong number of videos returned.");
      Assert (Total_Results = 74, "Wrong number of total videos.");
      Assert (To_String (Next_Page_Token) = "CAUQAA", "Wrong next page token.");
      Assert (Unavailable_Videos = 0, "Wrong unavailable videos.");

      -- Check first video info
      Assert (Song_Vectors.Element (Videos.First).Get_Id = "cXLadJlS_nA", "Wrong first video ID.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Title = "Electro-Light - Throwback [NCS Release]",
         "Wrong first video title.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/cXLadJlS_nA/default.jpg",
         "Wrong first video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Provider = Api.Youtube_Api,
         "Wrong first video provider.");

      -- Check last video info
      Assert (Song_Vectors.Element (Videos.Last).Get_Id = "Y54ABqSOScQ", "Wrong last video ID.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Title = "Slagsmålsklubben - Sponsored by destiny",
         "Wrong last video title.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/Y54ABqSOScQ/default.jpg",
         "Wrong last video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last video provider.");

      ----------------------------------------------------------------------------------------------
      -- Playlist with unavailable videos
      ----------------------------------------------------------------------------------------------
      Unavailable_Videos := Natural'First;

      Videos :=
        Parse_Playlist_Item_Results
          (Correct_Json_String_Unavailable_Video,
           Total_Results,
           Next_Page_Token,
           Unavailable_Videos);

      -- Check control variables
      Assert (Natural (Videos.Length) = 48, "Wrong number of videos returned.");
      Assert (Total_Results = 955, "Wrong number of total videos.");
      Assert (To_String (Next_Page_Token) = "CGQQAA", "Wrong next page token.");
      Assert (Unavailable_Videos = 2, "Wrong unavailable videos.");

      -- Check first video info
      Assert (Song_Vectors.Element (Videos.First).Get_Id = "AvdU-8V89CQ", "Wrong first video ID.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Title = "Blink 182 - Down",
         "Wrong first video title.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/AvdU-8V89CQ/default.jpg",
         "Wrong first video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.First).Get_Provider = Api.Youtube_Api,
         "Wrong first video provider.");

      -- Check last video info
      Assert (Song_Vectors.Element (Videos.Last).Get_Id = "M0e_0P9OZuM", "Wrong last video ID.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Title = "Fatty Boom Boom",
         "Wrong last video title.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Thumbnail_Link =
         "https://i.ytimg.com/vi/M0e_0P9OZuM/default.jpg",
         "Wrong last video thumbnail link.");
      Assert
        (Song_Vectors.Element (Videos.Last).Get_Provider = Api.Youtube_Api,
         "Wrong last video provider.");

      ----------------------------------------------------------------------------------------------
      -- Wrong JSON file
      ----------------------------------------------------------------------------------------------
      Unavailable_Videos := Natural'First;

      Videos :=
        Parse_Playlist_Item_Results
          (Wrong_Json,
           Total_Results,
           Next_Page_Token,
           Unavailable_Videos);

      -- Check control variables
      Assert (Natural (Videos.Length) = 0, "Wrong number of videos returned.");
      Assert (Total_Results = 0, "Wrong number of total videos.");
      Assert (To_String (Next_Page_Token) = "", "Wrong next page token.");
      Assert (Unavailable_Videos = 0, "Wrong unavailable videos.");
   end Test_Parse_Playlist_Item_Results;

   -------------------------------------------------------------------------------------------------
   -- Test_Parse_Video_Duration_Result
   -------------------------------------------------------------------------------------------------
   procedure Test_Parse_Video_Duration_Result
     (Test_Case : in out Aunit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Test_Case);

      Correct_Json : constant String :=
        Read_File ("ada/test/json_samples/youtube_video_content_details.json");
      Wrong_Json : constant String := "";
   begin
      Assert (Parse_Video_Duration_Result (Correct_Json) = 171, "Wrong duration returned.");
      Assert (Parse_Video_Duration_Result (Wrong_Json) = 0, "Wrong default duration returned.");
   end Test_Parse_Video_Duration_Result;

   -------------------------------------------------------------------------------------------------
   -- Test_Convert_Iso_8601_Duration_To_Seconds
   -------------------------------------------------------------------------------------------------
   procedure Test_Convert_Iso_8601_Duration_To_Seconds
     (Test_Case : in out Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Test_Case);

   begin
      Assert
        (Convert_Iso_8601_Duration_To_Seconds ("PT0H0M0S") = 0,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Convert_Iso_8601_Duration_To_Seconds ("PT21S") = 21,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Convert_Iso_8601_Duration_To_Seconds ("PT12M") = 720,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Convert_Iso_8601_Duration_To_Seconds ("PT3H") = 10800,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Convert_Iso_8601_Duration_To_Seconds ("PT59S") = 59,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Convert_Iso_8601_Duration_To_Seconds ("PT1M0S") = 60,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Convert_Iso_8601_Duration_To_Seconds ("PT59M59S") = 3599,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Convert_Iso_8601_Duration_To_Seconds ("PT1H0M0S") = 3600,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Convert_Iso_8601_Duration_To_Seconds ("PT23H59M59S") = 86399,
         "Wrong Conversion from ISO 8601 to seconds.");

      Assert
        (Convert_Iso_8601_Duration_To_Seconds ("PT15H37M4S") = 56224,
         "Wrong Conversion from ISO 8601 to seconds.");
   end Test_Convert_Iso_8601_Duration_To_Seconds;

end Api.Provider.Youtube.Test;
