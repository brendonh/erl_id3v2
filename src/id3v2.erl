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
-define(TESTFILE, "/media/everything/music/The Roots/Do You Want More/01 intro there's something goin' on.mp3").

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
    {ok, [{frames, Frames}|Props]};

read_v2({2, 3, _}, File, Props) ->
    {ok, Header} = file:read(File, ?GV(size, Props)),
    Header2 = skip_v2_extended_header(Header, Props),
    Frames = read_next_frame_23(Header2, []),
    {ok, [{frames, Frames}|Props]};

read_v2({2, 4, _}, File, Props) ->
    {ok, Header} = file:read(File, ?GV(size, Props)),
    Header2 = skip_v2_extended_header(Header, Props),
    Frames = read_next_frame_24(Header2, []),
    {ok, [{frames, Frames}|Props]}.


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
    lists:reverse(Frames);
read_next_frame_24(<<FrameID:4/binary,
                    S1:8/integer, S2:8/integer, S3:8/integer, S4:8/integer,
                    FlagBytes:2/binary,
                    Rest/binary>>,
                   Frames)
  when S1 < 128 andalso S2 < 128 andalso S3 < 128 andalso S4 < 128 ->
    Size = parse_synchsafe(<<S1,S2,S3,S4>>),
    case Size of
        0 -> lists:reverse(Frames);
        _ ->
            {Content, Tail} = split_binary(Rest, Size),
            <<0:1, A:1/integer, B:1/integer, C:1/integer, 0:5,
             H:1/integer, 0:2, K:1/integer, M:1/integer, N:1/integer, P:1/integer>> = FlagBytes,
            Flags = list_to_tuple([bool(X) || X <- [A, B, C, H, K, M, N, P]]),
            Frame = [{id, FrameID}, {size, Size}, {flags, Flags}, {content, Content}],
            read_next_frame_24(Tail, [Frame|Frames])
    end;
read_next_frame_24(_, Frames) ->
    lists:reverse(Frames).


read_next_frame_23(<<0, _Rest/binary>>, Frames) ->
    % Assume the header length was a lie, and we're done
    lists:reverse(Frames);
read_next_frame_23(<<FrameID:4/binary,
                    Size:32/integer,
                    FlagBytes:2/binary,
                    Rest/binary>>,
                   Frames) when Size > 0 andalso Size < byte_size(Rest) ->

    {Content, Tail} = split_binary(Rest, Size),

    <<A:1/integer, B:1/integer, C:1/integer, 0:5,
     I:1/integer, J:1/integer, K:1/integer, 0:5>> = FlagBytes,
    Flags = list_to_tuple([bool(X) || X <- [A, B, C, I, J, K]]),
    Frame = [{id, FrameID}, {size, Size}, {flags, Flags}, {content, Content}],
    read_next_frame_23(Tail, [Frame|Frames]);
read_next_frame_23(_, Frames) ->
    lists:reverse(Frames).



read_next_frame_22(<<FrameID:3/binary, Size:24/integer, Rest/binary>>,
                   Frames) ->
    {Content, Tail} = split_binary(Rest, Size),
    Frame = [{id, FrameID}, {size, Size}, {content, Content}],
    read_next_frame_22(Tail, [Frame|Frames]);
read_next_frame_22(_, Frames) ->
    lists:reverse(Frames).


read_v1_header(<<"TAG", Rest/binary>>) ->
    {ok, [{version, {1, 0, 0}},
          {data, Rest}]};
read_v1_header(_) ->
    not_found.


read_v1(File, Props) ->
    file:position(File, {eof, -355}),
    case file:read(File, 227) of
        <<"TAG+", TitleExt:60/binary, ArtistExt:60/binary, AlbumExt:60/binary,
         Speed:8/integer, Genre:30/binary, StartTime:6/binary, EndTime:6/binary>>
         -> ok;
        _Other ->
            {TitleExt, ArtistExt, AlbumExt, Speed, Genre, StartTime, EndTime} =
                {<<"">>, <<"">>, <<"">>, 0, <<"">>, <<"">>, <<"">>}
    end,

    {ok, Header} = file:read(File, 128),
    <<"TAG", Title:30/binary, Artist:30/binary, Album:30/binary, Year:4/binary,
     CommentAndTrack:30/binary, GenreNum:1/binary>> = Header,

    case CommentAndTrack of
        <<Comment:28/binary, 0:8, Track:8/integer>> when Track > 0 -> ok; % v1
        _ -> Comment = CommentAndTrack, Track=0 % v1.1
    end,

    CB = fun(B1, B2) ->
                 BinStr = erlang:concat_binary([strip_nulls(B1, 0), strip_nulls(B2, 0)]),
                 iso8859_to_utf8(string:strip(binary_to_list(BinStr)))
         end,

    Frames = [{title, CB(Title, TitleExt)}, {artist, CB(Artist, ArtistExt)},
              {album, CB(Album, AlbumExt)},
              {year, Year}, {comment, Comment}, {track, Track},
              {genre_num, GenreNum}, {genre, Genre},
              {speed, Speed}, {startTime, StartTime}, {endTime, EndTime}],

    {ok, [{frames, Frames}|Props]}.


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
        Other -> strip_nulls(Bin, N+1)
    end.

iso8859_to_utf8([H|T]) when H < 16#80 -> [H | iso8859_to_utf8(T)];
iso8859_to_utf8([H|T]) when H < 16#C0 -> [16#C2,H | iso8859_to_utf8(T)];
iso8859_to_utf8([H|T])                -> [16#C3, H-64 | iso8859_to_utf8(T)];
iso8859_to_utf8([])                   -> [].
