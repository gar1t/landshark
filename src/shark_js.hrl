-include_lib("eunit/include/eunit.hrl").

args_test() ->
    ?assertEqual("", args([])),
    ?assertEqual("1", args([1])),
    ?assertEqual("1,2", args([1, 2])),
    ?assertEqual("\"foo\"", args(["foo"])),
    ?assertEqual("\"foo\",\"bar\"", args(["foo", "bar"])),
    ?assertEqual("\"foo\"", args([foo])),
    ?assertEqual("1,\"foo\",{\"bar\":2,\"bam\":\"Bam\"}",
                 args([1, "foo", {struct, [{bar, 2}, {bam, "Bam"}]}])),
    ok.
