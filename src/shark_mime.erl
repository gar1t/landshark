-module(shark_mime).

-export([mime_type/1]).

%%% XXX fill in as needed per http://www.iana.org/assignments/media-types/

%%
%% text types
%%
mime_type(html) -> "text/html";
mime_type(css) -> "text/css";
mime_type(plain) -> "text/plain";
mime_type(xml) -> "text/xml";

%%
%% application types
%%
mime_type(javascript) -> "application/javascript";
mime_type(json) -> "application/json";

%%
%% image types
%%
mime_type(gif) -> "image/gif";
mime_type(jpeg) -> "image/jpeg";
mime_type(png) -> "image/png";
mime_type(tiff) -> "image/tiff".
