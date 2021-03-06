%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%
%% ------------------------------------------------------------
%% A simple demo for choosing
%% colors in a window.
%% ------------------------------------------------------------

-module(color_demo).
-compile([{nowarn_deprecated_function,{gs,button,2}},
          {nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,scale,2}},
          {nowarn_deprecated_function,{gs,start,0}},
          {nowarn_deprecated_function,{gs,window,2}}]).

-export([start/0,init/0]).

start() ->
    spawn(color_demo,init,[]).

init() ->
    I= gs:start(),
    W= gs:window(I,[{title,"Color Demo"},{width,300},{height,195}]), 
    B=gs:button(W,[{label,{text,"Quit"}},{x,261},{y,166},{width,40}]),
    gs:config(B,[{bg,yellow},{fg,hotpink1},{data,quit}]),
    gs:scale(W,[{text,"Red"},{y,0},{range,{0,255}},{orient,horizontal},
		{height,65},{data,red},{pos,42}]),
    gs:scale(W,[{text,"Blue"},{y,65},{range,{0,255}},{orient,horizontal},
		{height,65},{data,blue},{pos,42}]),
    gs:scale(W,[{text,"Green"},{y,130},{range,{0,255}},{orient,horizontal},
		{height,65},{data,green},{pos,42}]),
    gs:config(W,{map,true}),
    loop(W,0,0,0).

loop(W,R,G,B) ->
    gs:config(W,{bg,{R,G,B}}),
    receive
	{gs,_,click,red,[New_R|_]} ->
	    loop(W,New_R,G,B);
	{gs,_,click,green,[New_G|_]} ->
	    loop(W,R,New_G,B);
	{gs,_,click,blue,[New_B|_]} ->
	    loop(W,R,G,New_B);
	{gs,_,click,quit,_} ->
	    true;
	{gs,W,destroy,_,_} ->
	    true
    end.

%% ------------------------------------------------------------
