#!/bin/ksh

# Script dir
SCRIPTDIR=$(dirname $0)

# Source lib
. $SCRIPTDIR/unpack_lib.ksh 
. $SCRIPTDIR/guessit_lib.ksh

# Define Tv and Movie folder (can be redefined with input args)
TV=/data/TvShows
MOVIE=/data/Movies

#Input data
while [ "$1" ]; do
    case $1 in
       -torrent)      TORRENT="$2"; shift 1     ;;
       -torrentpath)  TORRENTPATH="$2"; shift 1 ;;
       -envfile)      ENVFILE="$2"; shift 1     ;;
       -no)           DUMMY=echo                ;;
       -tvfolder)     TV="$2"; shift 1          ;;
       -moviefolder)  MOVIE="$2"; shift 1       ;;
       *)             echo "Strange input"      ;;
    esac
    shift 1
done

# Verify input data
if [[ "$TORRENT" == "" || "$TORRENTPATH" == "" ]]; then
    echo "Torrent or Torrentpath is undefined"
    return 1
fi
set -x
# Guess torrent ($type $series $season $year inherent from guessit_lib function)
guessit $TORRENT 
[[ $? != 0 ]] && echo "guessit did not work!" && return 1
if [[ "$type" == "unknown" ]]; then
    # Try with .mkv
    TORRENTFILE="$TORRENT.mkv"
    guessit $TORRENTFILE
    [[ $? != 0 ]] && echo "guessit did not work!" && return 1
    echo "Torrent $TORRENT"
fi

# Determine target folder
if [[ "$type" == "movie" ]]; then
    echo "Torrent is a movie"
    TARGET=$MOVIE/$TORRENT
    echo "Target: $TARGET"
elif [[ "$type" == "episode" ]]; then
    echo "Torrent is a TvShow"
    if [[ "$series" != unknown || "$season" != unknown ]]; then
        echo $series | sed 's/ /./g' | read series
        echo "Show: $series"
        echo "Season: $season"
        if [[ $season -lt 9 ]]; then
            season="0$season"
        fi
        if [[ "$year" != "" && "$year" != unknown ]]; then
            TARGET="$TV/$series.$year/S$season"
        else
            TARGET="$TV/$series/S$season"
        fi
        echo "Target: $TARGET"
    else
	echo "Missing information about tvshow"
        exit 1
   fi
else
    echo "Torrent is type: $type"
    exit 2
fi

[[ $TARGET == "" ]] && echo "Target missing for $TORRENT" && return 1

# Determine file format
if [[ -d $TORRENTPATH/$TORRENT ]]; then
    find $TORRENTPATH/$TORRENT -type f -name *.rar | grep -q '.rar'
    if [[ $? == 0 ]]; then
        echo "Torrent is packed as rar files"
        # Unpack torrent to target
        $DUMMY unpack $TORRENTPATH/$TORRENT $TARGET
        [[ $? != 0 ]] && echo "Unpacking failed!" && return 1
    else
        echo "Torrent isn't packed as a rar files"
        find $TORRENTPATH/$TORRENT -type f \( -iname \*.mkv -o -iname \*.avi -o -iname \*.mp4 \) | while read file; do
            $DUMMY mkdir -p $TARGET
            echo "Copying $file to $TARGET"
            $DUMMY cp $file $TARGET
        done
    fi
elif [[ -f $TORRENTPATH/$TORRENT ]]; then
    $DUMMY mkdir -p $TARGET
    echo "Copying $TORRENTPATH/$TORRENT to $TARGET"
    $DUMMY cp $TORRENTPATH/$TORRENT $TARGET
else
    echo "$TORRENT at $TORRENTPATH doesn't exist"
    return 1
fi

# Set chmod g+rwx
$DUMMY chmod -R g+rwx $TARGET

# Log variable to file for jenkins
if [[ "$ENVFILE" && "$DUMMY" != echo ]]; then
    echo "type=$type" >> $ENVFILE
    echo "target=$TARGET" >> $ENVFILE
    echo "torrent=$TORRENT" >> $ENVFILE
fi
