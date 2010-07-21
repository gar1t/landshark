-module(shark_static_tests).

-include_lib("eunit/include/eunit.hrl").

from_env_test() ->
    ?assertEqual({filepath, "/var/www/img/foo.png", []},
		 shark_static:from_env([{path_info, "/img/foo.png"}],
					"/var/www")),
    ?assertEqual({filepath, "/var/www/img/foo.png", []},
		 shark_static:from_env([{path_info, "img/foo.png"}],
					"/var/www")),
    ?assertEqual({filepath, undefined, []},
		 shark_static:from_env([{path_info, "img/foo.png"}],
					undefined)).
from_path_test() ->
    ?assertEqual({filepath, "/var/www", []},
		 shark_static:from_path("/", "/var/www")),    
    ?assertEqual({filepath, "/var/www/img/foo.gif", []},
		 shark_static:from_path("/img/foo.gif", "/var/www")),
    ?assertEqual({filepath, undefined, []},
		 shark_static:from_path("/", undefined)).
