%%%-------------------------------------------------------------------
%%% File    : id3v2.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : ID3v2 reader
%%%
%%% Created : 13 Aug 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(id3v2).

-export([read_file/1, read/1, test/0,
         parse_synchsafe/1, strip_nulls/2]).

-define(TESTPATTERN, "/media/everything/music/*/*/*.mp3").
-define(TESTFILE, "/media/everything/music/The Roots/Do You Want More/09 do you want more_!!!__!.mp3").

-define(DBG(Term), io:format("~p: ~p~n", [self(), Term])).
-define(GV(E, P), proplists:get_value(E, P)).

test() ->
    test_all().

test_all() ->
    Start = now(),
    read_files(filelib:wildcard(?TESTPATTERN), 0, 0),
    ?DBG({time, timer:now_diff(now(), Start) / 1000000}).

test_one() ->
    ?DBG(read_file(?TESTFILE)).


read_files([FN|Rest], Total, Fail) ->
    case read_file(FN) of
        {ok, Props} ->
            read_files(Rest, Total+1, Fail);
        not_found -> read_files(Rest, Total+1, Fail+1)
    end;
read_files([], Total, Fail) ->
    ?DBG({total, Total}),
    ?DBG({fail, Fail}).



read_file(Filename) ->
    {ok, File} = file:open(Filename, [read, raw, binary]),
    RV = read(File),
    file:close(File),
    RV.

read(File) ->
    {ok, Start} = file:read(File, 10),
    case read_v2_header(Start) of
        {ok, Props} ->
            read_v2(File, Props);
        not_found ->
            file:position(File, {eof, -128}),
            {ok, Tail} = file:read(File, 128),
            case read_v1_header(Tail) of
                {ok, Props} ->
                    read_v1(File, Props);
                not_found ->
                    not_found
            end
    end.


read_v2_header(<<"ID3", VMaj:8/integer, VMin:8/integer,
             A:1/integer, B:1/integer, C:1/integer, D:1/integer, 0:4,
             S1:8/integer, S2:8/integer, S3:8/integer, S4:8/integer>>)
  when S1 < 128 andalso S2 < 128 andalso S3 < 128 andalso S4 < 128 ->
    [Unsync, Extended, Experimental, Footer] = [bool(X) || X <- [A,B,C,D]],
    Size = parse_synchsafe(<<S1, S2, S3, S4>>),
    {ok, [{version, {2, VMaj, VMin}},
          {unsync, Unsync}, {extended, Extended},
          {experimental, Experimental}, {footer, Footer},
          {size, Size}]};
read_v2_header(_) ->
    not_found.


read_v2(File, Props) ->
    Version = ?GV(version, Props),
    read_v2(Version, File, Props).


read_v2({2, 2, _}, File, Props) ->
    {ok, Header} = file:read(File, ?GV(size, Props)),
    Frames = read_next_frame_22(Header, []),
    {ok, Frames};
    %{ok, [{frames, Frames}|Props]};

read_v2({2, 3, _}, File, Props) ->
    {ok, Header} = file:read(File, ?GV(size, Props)),
    Header2 = skip_v2_extended_header(Header, Props),
    Frames = read_next_frame_23(Header2, []),
    {ok, Frames};
    %{ok, [{frames, Frames}|Props]};

read_v2({2, 4, _}, File, Props) ->
    {ok, Header} = file:read(File, ?GV(size, Props)),
    Header2 = skip_v2_extended_header(Header, Props),
    Frames = read_next_frame_24(Header2, []),
    {ok, Frames}.
    %{ok, [{frames, Frames}|Props]}.


% Untested! None of my mp3 files have extended headers.
skip_v2_extended_header(Header, Props) ->
    case ?GV(extended, Props) of
        true ->
            <<SynchSafeSize:4/binary, _/binary>> = Header,
            ExtSize = parse_synchsafe(SynchSafeSize),
            {_, Header2} = split_binary(Header, ExtSize),
            Header2;
        _ ->
            Header
    end.


read_next_frame_24(<<0, _Rest/binary>>, Frames) ->
    % Assume the header length was a lie, and we're done
    finalize_v2_frames(Frames);
read_next_frame_24(<<FrameID:4/binary,
                    S1:8/integer, S2:8/integer, S3:8/integer, S4:8/integer,
                    FlagBytes:2/binary,
                    Rest/binary>>,
                   Frames)
  when S1 < 128 andalso S2 < 128 andalso S3 < 128 andalso S4 < 128 ->
    Size = parse_synchsafe(<<S1,S2,S3,S4>>),
    case Size of
        0 -> finalize_v2_frames(Frames);
        _ ->
            {Content, Tail} = split_binary(Rest, Size),
            <<0:1, A:1/integer, B:1/integer, C:1/integer, 0:5,
             H:1/integer, 0:2, K:1/integer, M:1/integer, N:1/integer, P:1/integer>> = FlagBytes,
            Flags = list_to_tuple([bool(X) || X <- [A, B, C, H, K, M, N, P]]),
            Frame = parse_v2_frame(FrameID, Size, Content, Flags, {2,4,0}),
            read_next_frame_24(Tail, [Frame|Frames])
    end;
read_next_frame_24(_, Frames) ->
    finalize_v2_frames(Frames).


read_next_frame_23(<<0, _Rest/binary>>, Frames) ->
    % Assume the header length was a lie, and we're done
    finalize_v2_frames(Frames);
read_next_frame_23(<<FrameID:4/binary,
                    Size:32/integer,
                    FlagBytes:2/binary,
                    Rest/binary>>,
                   Frames) when Size > 0 andalso Size =< byte_size(Rest) ->
    {Content, Tail} = split_binary(Rest, Size),
    <<A:1/integer, B:1/integer, C:1/integer, 0:5,
     I:1/integer, J:1/integer, K:1/integer, 0:5>> = FlagBytes,
    Flags = list_to_tuple([bool(X) || X <- [A, B, C, I, J, K]]),
    Frame = parse_v2_frame(FrameID, Size, Content, Flags, {2,3,0}),
    read_next_frame_23(Tail, [Frame|Frames]);
read_next_frame_23(_Other, Frames) ->
    finalize_v2_frames(Frames).



read_next_frame_22(<<FrameID:3/binary, Size:24/integer, Rest/binary>>,
                   Frames) ->
    {Content, Tail} = split_binary(Rest, Size),
    Frame = parse_v2_frame(FrameID, Size, Content, none, {2,3,0}),
    read_next_frame_22(Tail, [Frame|Frames]);
read_next_frame_22(_, Frames) ->
    finalize_v2_frames(Frames).



parse_v2_frame(<<"TIT2">>, _Size, RawContent, _Flags, _Version) ->
    {tit2, extract_v2_string(RawContent)};
parse_v2_frame(<<"TT2">>, _Size, RawContent, _Flags, _Version) ->
    {tit2, extract_v2_string(RawContent)};
parse_v2_frame(_Other, _, _, _, _Version) -> 
    ignored.


finalize_v2_frames(Frames) ->
    lists:reverse([F || F <- Frames, F /= ignored]).
                        




read_v1_header(<<"TAG", Rest/binary>>) ->
    {ok, [{version, {1, 0, 0}},
          {data, Rest}]};
read_v1_header(_) ->
    not_found.


read_v1(File, _Props) ->
    file:position(File, {eof, -355}),
    case file:read(File, 227) of
        <<"TAG+", TitleExt:60/binary, ArtistExt:60/binary, AlbumExt:60/binary,
         _Speed:8/integer, Genre:30/binary, _StartTime:6/binary, _EndTime:6/binary>>
         -> ok;
        _Other ->
            {TitleExt, ArtistExt, AlbumExt, _Speed, Genre, _StartTime, _EndTime} =
                {<<"">>, <<"">>, <<"">>, 0, <<"">>, <<"">>, <<"">>}
    end,

    {ok, Header} = file:read(File, 128),
    <<"TAG", Title:30/binary, Artist:30/binary, Album:30/binary, Year:4/binary,
     CommentAndTrack:30/binary, GenreNum:8/integer>> = Header,

    case CommentAndTrack of
        <<Comment:28/binary, 0:8, Track:8/integer>> when Track > 0 -> ok; % v1
        _ -> Comment = CommentAndTrack, Track=0 % v1.1
    end,

    CB = fun(B1, B2) ->
                 BinStr = erlang:concat_binary([strip_nulls(B1, 0), strip_nulls(B2, 0)]),
                 unicode:characters_to_binary(string:strip(binary_to_list(BinStr)), latin1)
         end,

    GenreStr = case Genre of
                   <<"">> -> v1genre(GenreNum);
                   _ -> Genre
               end,

    Frames = [{tit2, CB(Title, TitleExt)}, {tpe1, CB(Artist, ArtistExt)},
              {talb, CB(Album, AlbumExt)},
              {tdrc, Year}, {comm, Comment}, {trck, Track},
              {tcon, GenreStr}],
              %{speed, Speed}, {startTime, StartTime}, {endTime, EndTime}],
    {ok, Frames}.

    %{ok, [{frames, Frames}|Props]}.



parse_synchsafe(FourBytes) ->
    <<0:1, S1:7/integer, 0:1, S2:7/integer, 0:1, S3:7/integer, 0:1, S4:7/integer>> = FourBytes,
    <<Size:32/integer>> = <<0:4, S1:7/integer, S2:7/integer, S3:7/integer, S4:7/integer>>,
    Size.

bool(1) -> true;
bool(_) -> false.

strip_nulls(Bin, N) ->
    case split_binary(Bin, N) of
        {Stuff, <<0, _Rest/binary>>} -> Stuff;
        {All, <<>>} -> All;
        _Other -> strip_nulls(Bin, N+1)
    end.


extract_v2_string(Binary) ->
    strip_nulls(decode_v2_string(Binary), 0).

decode_v2_string(<<0:8, Rest/binary>>) -> 
    unicode:characters_to_binary(Rest, latin1);
decode_v2_string(<<1:8, BOM:2/binary, Rest/binary>>) -> 
    {Encoding, _} = unicode:bom_to_encoding(BOM),
    unicode:characters_to_binary(Rest, Encoding);
decode_v2_string(<<2:8, Rest/binary>>) -> 
    unicode:characters_to_binary(Rest, {utf16, big});
decode_v2_string(<<3:8, Rest/binary>>) -> 
    unicode:characters_to_binary(Rest, utf8);
decode_v2_string(Other) -> 
    unicode:characters_to_binary(Other, latin1).



v1genre(0) -> "Blues";
v1genre(1) -> "Classic Rock";
v1genre(2) -> "Country";
v1genre(3) -> "Dance";
v1genre(4) -> "Disco";
v1genre(5) -> "Funk";
v1genre(6) -> "Grunge";
v1genre(7) -> "Hip-Hop";
v1genre(8) -> "Jazz";
v1genre(9) -> "Metal";
v1genre(10) -> "New Age";
v1genre(11) -> "Oldies";
v1genre(12) -> "Other";
v1genre(13) -> "Pop";
v1genre(14) -> "R&B";
v1genre(15) -> "Rap";
v1genre(16) -> "Reggae";
v1genre(17) -> "Rock";
v1genre(18) -> "Techno";
v1genre(19) -> "Industrial";
v1genre(20) -> "Alternative";
v1genre(21) -> "Ska";
v1genre(22) -> "Death Metal";
v1genre(23) -> "Pranks";
v1genre(24) -> "Soundtrack";
v1genre(25) -> "Euro-Techno";
v1genre(26) -> "Ambient";
v1genre(27) -> "Trip-Hop";
v1genre(28) -> "Vocal";
v1genre(29) -> "Jazz+Funk";
v1genre(30) -> "Fusion";
v1genre(31) -> "Trance";
v1genre(32) -> "Classical";
v1genre(33) -> "Instrumental";
v1genre(34) -> "Acid";
v1genre(35) -> "House";
v1genre(36) -> "Game";
v1genre(37) -> "Sound Clip";
v1genre(38) -> "Gospel";
v1genre(39) -> "Noise";
v1genre(40) -> "Alternative Rock";
v1genre(41) -> "Bass";
v1genre(42) -> "Soul";
v1genre(43) -> "Punk";
v1genre(44) -> "Space";
v1genre(45) -> "Meditative";
v1genre(46) -> "Instrumental Pop";
v1genre(47) -> "Instrumental Rock";
v1genre(48) -> "Ethnic";
v1genre(49) -> "Gothic";
v1genre(50) -> "Darkwave";
v1genre(51) -> "Techno-Industrial";
v1genre(52) -> "Electronic";
v1genre(53) -> "Pop-Folk";
v1genre(54) -> "Eurodance";
v1genre(55) -> "Dream";
v1genre(56) -> "Southern Rock";
v1genre(57) -> "Comedy";
v1genre(58) -> "Cult";
v1genre(59) -> "Gangsta";
v1genre(60) -> "Top 40";
v1genre(61) -> "Christian Rap";
v1genre(62) -> "Pop/Funk";
v1genre(63) -> "Jungle";
v1genre(64) -> "Native US";
v1genre(65) -> "Cabaret";
v1genre(66) -> "New Wave";
v1genre(67) -> "Psychadelic";
v1genre(68) -> "Rave";
v1genre(69) -> "Showtunes";
v1genre(70) -> "Trailer";
v1genre(71) -> "Lo-Fi";
v1genre(72) -> "Tribal";
v1genre(73) -> "Acid Punk";
v1genre(74) -> "Acid Jazz";
v1genre(75) -> "Polka";
v1genre(76) -> "Retro";
v1genre(77) -> "Musical";
v1genre(78) -> "Rock & Roll";
v1genre(79) -> "Hard Rock";
v1genre(80) -> "Folk";
v1genre(81) -> "Folk-Rock";
v1genre(82) -> "National Folk";
v1genre(83) -> "Swing";
v1genre(84) -> "Fast Fusion";
v1genre(85) -> "Bebob";
v1genre(86) -> "Latin";
v1genre(87) -> "Revival";
v1genre(88) -> "Celtic";
v1genre(89) -> "Bluegrass";
v1genre(90) -> "Avantgarde";
v1genre(91) -> "Gothic Rock";
v1genre(92) -> "Progressive Rock";
v1genre(93) -> "Psychedelic Rock";
v1genre(94) -> "Symphonic Rock";
v1genre(95) -> "Slow Rock";
v1genre(96) -> "Big Band";
v1genre(97) -> "Chorus";
v1genre(98) -> "Easy Listening";
v1genre(99) -> "Acoustic";
v1genre(100) -> "Humour";
v1genre(101) -> "Speech";
v1genre(102) -> "Chanson";
v1genre(103) -> "Opera";
v1genre(104) -> "Chamber Music";
v1genre(105) -> "Sonata";
v1genre(106) -> "Symphony";
v1genre(107) -> "Booty Bass";
v1genre(108) -> "Primus";
v1genre(109) -> "Porn Groove";
v1genre(110) -> "Satire";
v1genre(111) -> "Slow Jam";
v1genre(112) -> "Club";
v1genre(113) -> "Tango";
v1genre(114) -> "Samba";
v1genre(115) -> "Folklore";
v1genre(116) -> "Ballad";
v1genre(117) -> "Power Ballad";
v1genre(118) -> "Rhythmic Soul";
v1genre(119) -> "Freestyle";
v1genre(120) -> "Duet";
v1genre(121) -> "Punk Rock";
v1genre(122) -> "Drum Solo";
v1genre(123) -> "Acapella";
v1genre(124) -> "Euro-House";
v1genre(125) -> "Dance Hall";
v1genre(126) -> "Goa";
v1genre(127) -> "Drum & Bass";
v1genre(128) -> "Club - House";
v1genre(129) -> "Hardcore";
v1genre(130) -> "Terror";
v1genre(131) -> "Indie";
v1genre(132) -> "BritPop";
v1genre(133) -> "Negerpunk";
v1genre(134) -> "Polsk Punk";
v1genre(135) -> "Beat";
v1genre(136) -> "Christian Gangsta Rap";
v1genre(137) -> "Heavy Metal";
v1genre(138) -> "Black Metal";
v1genre(139) -> "Crossover";
v1genre(140) -> "Contemporary Christian";
v1genre(141) -> "Christian Rock";
v1genre(142) -> "Merengue";
v1genre(143) -> "Salsa";
v1genre(144) -> "Thrash Metal";
v1genre(145) -> "Anime";
v1genre(146) -> "JPop";
v1genre(147) -> "Synthpop";
v1genre(_) -> "Unknown".
