#!/bin/bash

get_ejabberd_header(){
	echo "Downloading ejabberd.hrl and jlib.hrl"
	curl -O "https://raw.github.com/processone/ejabberd/master/src/ejabberd.hrl"
	curl -O "https://raw.github.com/processone/ejabberd/master/src/jlib.hrl"
}

move_header(){
	echo "Move header files to include dir"
	mv -vf ejabberd.hrl ./include
	mv -vf jlib.hrl ./include
}

is_file_exists(){
	local f="$1"
	[[ -f "$f" ]] && return 0 || return 1
}

echo "Download header files"
get_ejabberd_header
move_header
