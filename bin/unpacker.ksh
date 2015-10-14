#!/bin/ksh

# Script dir
SCRIPTDIR=$(dirname ${.sh.file})
ROOTDIR=${SCRIPTDIR%/*}

# Source lib
. $ROOTDIR/lib/unpack_lib.ksh 
. $ROOTDIR/lib/guessit_lib.ksh

function usage {
    echo "Usage of ${.sh.file}"
    echo "  -torrent <torret name>            [M]   Name/folder of the torrent"
    echo "  -torrentpath <path to torrent>    [M]   Path to torrent excluding name/folder"
    echo "  -envfile <file>                   [O]   Used to output env variables to files"
    echo "                                          usable for Jenkins"
    echo "  -guessitopt \"<guessit options>\"   [O]   Guessit passthrough options"
    echo "  -no                               [O]   Dummys the script, for debuging"
    echo "  -tvfolder <folder>                [MO]  Extraction path for TvShows"
    echo "  -moviefolder <folder>             [MO]  Extraction path for Movies"
    echo "                                          If only -tvfolder or -moviefolder is"
    echo "                                          is present type will be forced to that type"
    echo ""
    echo "  -help                                   show this help information"
    echo "M=mandatory, O=optional"
    exit 1
}

[[ $1 == "" ]] && usage
#Input data
while [ "$1" ]; do
    case $1 in
       -torrent)      TORRENT="$2"; shift 1              ;;
       -torrentpath)  TORRENTPATH="$2"; shift 1          ;;
       -envfile)      ENVFILE="$2"; shift 1              ;;
       -no)           DUMMY=echo                         ;;
       -tvfolder)     TV="$2"; shift 1                   ;;
       -moviefolder)  MOVIE="$2"; shift 1                ;;
       -guessitopt)   GUESSITOPT="$2"; shift 1           ;;
       -help)         usage                              ;;
       *)             echo "Strange input" && usage      ;;
    esac
    shift 1
done

# Verify input data
if [[ "$TORRENT" == "" || "$TORRENTPATH" == "" ]]; then
    echo "Torrent or Torrentpath is undefined"
    usage
fi
# Verify folders
if [[ $TV == "" && $MOVIE == "" ]]; then
    echo "Missing -tvfolder and/or -moviefolder"
    usage
elif [[ $TV != "" && $MOVIE != "" ]]; then
    TYPE=""
elif [[ $TV != "" && $MOVIE == "" ]]; then
    TYPE=episode
elif [[ $TV == "" && $MOVIE != "" ]]; then
    TYPE=movie
fi

# Set guessit options
if [[ $GUESSITOPT != "" ]]; then
    echo "guessit options overridden (execpt -n) with $GUESSITOPT"
elif [[ $TYPE != "" ]]; then
    GUESSITOPT="-t $TYPE"
    echo "guessit options: $GUESSITOPT"
fi

# Guess torrent ($type $series $season $year inherent from guessit_lib function)
guessit "$TORRENT" "$GUESSITOPT" 
[[ $? != 0 ]] && echo "guessit did not work!" && return 1
if [[ "$type" == "unknown" ]]; then
    # Try with .mkv
    TORRENTFILE="$TORRENT.mkv"
    guessit "$TORRENTFILE"
    [[ $? != 0 ]] && echo "guessit did not work!" && return 1
    echo "Torrent $TORRENT"
fi

# Determine target folder
if [[ "$type" == "movie" ]]; then
    echo "Torrent is a movie"
    TARGET="$MOVIE/$TORRENT"
    echo "Target: $TARGET"
elif [[ "$type" == "episode" ]]; then
    echo "Torrent is a TvShow"
    if [[ "$series" != unknown || "$season" != unknown ]]; then
        echo $series | sed 's/ /./g' | read series
        echo "Show: $series"
        echo "Season: $season"
        if [[ $season -lt 9 || $season = 9 ]]; then
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
if [[ -d "$TORRENTPATH/$TORRENT" ]]; then
    find "$TORRENTPATH/$TORRENT" -type f -name *.rar | grep -q '.rar'
    if [[ $? == 0 ]]; then
        echo "Torrent is packed as rar files"
        # Unpack torrent to target
        $DUMMY unpack "$TORRENTPATH/$TORRENT" $TARGET
        [[ $? != 0 ]] && echo "Unpacking failed!" && return 1
    else
        echo "Torrent isn't packed as a rar files"
        find "$TORRENTPATH/$TORRENT" -type f \( -iname \*.mkv -o -iname \*.avi -o -iname \*.mp4 \) | while read file; do
            $DUMMY mkdir -p $TARGET
            echo "Copying $file to $TARGET"
            $DUMMY cp "$file" $TARGET
        done
    fi
elif [[ -f $TORRENTPATH/$TORRENT ]]; then
    $DUMMY mkdir -p $TARGET
    echo "Copying $TORRENTPATH/$TORRENT to $TARGET"
    $DUMMY cp "$TORRENTPATH/$TORRENT" $TARGET
else
    echo "$TORRENT at $TORRENTPATH doesn't exist"
    return 1
fi

# Set chmod g+rwx
$DUMMY chmod -R g+rwx "$TARGET"

# Log variable to file for jenkins
if [[ "$ENVFILE" && "$DUMMY" != echo ]]; then
    echo "type=$type" >> $ENVFILE
    echo "target=$TARGET" >> $ENVFILE
    echo "torrent=$TORRENT" >> $ENVFILE
fi
