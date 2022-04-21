#!/bin/bash

function focus()
{
	JS="""
	global.get_window_actors().forEach(function(w)
	{
		const mw = w.meta_window;

		if(mw.get_pid() === $PID)
		{
			mw.move_resize_frame(0, 0, 0, 1024, 576);
			mw.focus(0);
		}
	});
	"""

	RES=$(gdbus call \
		--session \
		--dest org.gnome.Shell \
		--object-path /org/gnome/Shell \
		--method org.gnome.Shell.Eval \
		"$JS")

	if [ "${RES//[[:blank:]]/}" != "(true,'')" ]
	then
		tput setaf 1
		echo "ERROR: Failed resize window using GJS. You may need to run 'global.context.unsafe_mode = true' in Looking Glass." 1>&2
		tput sgr0
		exit 1
	fi

	sleep 1
}

function screenshot()
{
	gdbus call \
		--session \
		--dest org.gnome.Shell.Screenshot \
		--object-path /org/gnome/Shell/Screenshot \
		--method org.gnome.Shell.Screenshot.ScreenshotWindow \
		"false" "false" "true" "$1" \
		> /dev/null
	mogrify -gravity center \
		-alpha off \
		-trim \
		-fuzz 1% \
		-transparent black \
		+repage \
		"$1"
}

function launch()
{
	if [ ! -z "$(pgrep celluloid)" ]
	then
		tput setaf 1
		echo "ERROR: An instance of Celluloid is already running." 1>&2
		tput sgr0
		exit 1
	fi

	celluloid --mpv-panscan=1 --mpv-autofit=0 &
	PID=$!
	sleep 1
	gdbus call \
		--session \
		--dest org.mpris.MediaPlayer2.io.github.celluloid_player.Celluloid.instance-1 \
		--object-path /org/mpris/MediaPlayer2 \
		--method org.mpris.MediaPlayer2.Player.OpenUri \
		"$2" \
		> /dev/null
	gdbus call \
		--session \
		--dest org.mpris.MediaPlayer2.io.github.celluloid_player.Celluloid.instance-1 \
		--object-path /org/mpris/MediaPlayer2 \
		--method org.mpris.MediaPlayer2.Player.Pause \
		> /dev/null
	gdbus call \
		--session \
		--dest org.mpris.MediaPlayer2.io.github.celluloid_player.Celluloid.instance-1 \
		--object-path /org/mpris/MediaPlayer2 \
		--method org.mpris.MediaPlayer2.Player.SetPosition \
		'/io/github/CelluloidPlayer/Celluloid/Track/0' \
		"$1" \
		> /dev/null
	ps -p $PID > /dev/null

	if [ $? -ne 0 ]
	then
		tput setaf 1
		echo "ERROR: Failed to start Celluloid." 1>&2
		tput sgr0
		exit 1
	fi
	sleep 1
}

function set_csd_enable()
{
	gsettings set \
		io.github.celluloid-player.Celluloid \
		csd-enable \
		"$1"
}

function set_show_playlist()
{
	gsettings set \
		io.github.celluloid-player.Celluloid.window-state \
		show-playlist \
		"$1"
}

function run()
{
	echo "Taking screenshot $3 (csd=$1, playlist=$2)..."
	set_csd_enable "$1"
	set_show_playlist "$2"
	launch "$4" "$5"
	focus
	screenshot "$3"
	kill -TERM $PID
	PID=""
	sleep 1
}

function cleanup()
{
	[ ! -z "$TMPDIR" ] && rm -r $TMPDIR
	[ ! -z "$PID" ] && kill -TERM $PID
}

if [ $# -ne 2 ]
then
	echo "Usage: $0 TIMESTAMP VIDEO_FILE" 1>&2
	exit 1
fi

export TMPDIR=$(mktemp -d)
export XDG_CONFIG_HOME=$TMPDIR
export GSETTINGS_BACKEND=keyfile

trap cleanup EXIT

ARRAY=({true,false}\ {false,true})
COUNT=0

for X in "${ARRAY[@]}"
do
	run $X "$(pwd)/images/screenshot-$COUNT.png" "$1" "$2"
	((COUNT++))
done
