-module(weatherlinkerl_loop).

-export([
        print_sample/0,
        open_sample/0,
        close_sample/1,
        read_packet/1,
        print_packets/1,
        type/1,
        date/1,
        time/1,
        outside_temp/1,
        high_temp/1,
        low_temp/1,
        rainfall/1
    ]).

-include("weatherlinkerl.hrl").

-define(DMP_PKT_LENGTH, 52).

print_sample() ->
    IO = open_sample(),
    print_packets(IO).

open_sample() ->
    File = code:priv_dir(?STR(?APP)) ++ "/sample-loop.dat",
    {ok, IO} = file:open(lists:flatten(File), [read,binary]),
    IO.

close_sample(IO) ->
    file:close(IO).

%% reads the first byte from the samples. Docs don't mention this byte for web
%% protocol. When reading from a serial device first byte would be a page #.
% read_page(IO) ->
%     {ok, Byte} = file:read(IO, 1),
%     Byte.

read_packet(IO) ->
    case file:read(IO, ?DMP_PKT_LENGTH) of
        {ok, Pkt} ->
            Pkt;
        eof ->
            done;
        {error, _Error} ->
            done
    end.

format_packet(P) ->
    Type = type(P),
    {Y, M, D} = date(P),
    {Hr, Min} = time(P),
    Temp = outside_temp(P),
    HighTemp = high_temp(P),
    LowTemp = low_temp(P),
    Rain = rainfall(P),
    io:format("~p\n", [P]),
    io:format("Time: ~w-~w-~w ~w:~w - Temp: ~w (~w,~w), Rain: ~w, Type: ~w\n",
        [Y, M, D, Hr, Min, Temp, LowTemp, HighTemp, Rain, Type]).

print_packets(IO, done) ->
    close_sample(IO),
    ok;
print_packets(IO, P) ->
    format_packet(P),
    print_packets(IO, read_packet(IO)).

print_packets(IO) ->
    % P=read_page(IO),
    % io:format("Page: ~w\n", [P]),
    print_packets(IO, read_packet(IO)).

type(Pkt) ->
    Type = binary:at(Pkt,42),
    if
        Type =:= 0 ->
            rev_b;
        Type =:= 255 ->
            rev_a;
        true ->
            unknown
    end.



date(Pkt) ->
    <<Y:7,M:4,D:5>> = binary:part(Pkt, 0, 2),
    {2000+Y, M, D}.

time(Pkt) ->
    <<T:16>> = binary:part(Pkt, 2, 2),
    { T div 100, T rem 100 }.

outside_temp(Pkt) ->
    <<T:16>> = binary:part(Pkt, 4, 2),
    T/10.

high_temp(Pkt) ->
    <<T:16>> = binary:part(Pkt, 6, 2),
    T/10.

low_temp(Pkt) ->
    <<T:16>> = binary:part(Pkt, 8, 2),
    T/10.

rainfall(Pkt) ->
    <<T:16>> = binary:part(Pkt, 10, 2),
    T * 0.01.

