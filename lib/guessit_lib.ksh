#!/bin/ksh


# Check if torrent is included in the unknown torrents
function unknown {
    case $1 in
        NBA.*)     series=NBA
                   type=episode
                   season=0
                   year=unknown ;;
        *)                      ;;
    esac
   [[ "$series" != "" ]] && return 0
   return 1
}

function guessit {
    unset type series year season
    typeset TORRENT GUESSITOPT
    [[ "$1" == "" ]] && echo "Missing input to guessit" && return 1
    GUESSITOPT="-n -y $2"
    TORRENT=$1
    unknown "$TORRENT"
    if [[ $? == 0 ]]; then
        return 0
    fi
    python -m guessit $GUESSITOPT "$TORRENT" | while read key value; do
        echo "Info: $key $value"
            case $key in
                *type*)        type=$value   ;;
                *series*)      series=$value ;;
                *year*)        year=$value   ;;
                *season*)      season=$value ;;
                *title*)       series=$value ;;
                *)                         ;;
            esac
    done
    echo "$type" | sed -e 's/"//g' -e 's/,//g' | read type
    echo "$series" | sed -e 's/"//g' -e 's/,//g' | read series
    echo "$year" | sed -e 's/"//g' -e 's/,//g' | read year
    echo "$season" | sed -e 's/"//g' -e 's/,//g' | read season
    [[ "$type" == "" ]] && type=unknown
    [[ "$year" == "" ]] && year=unknown
    [[ "$series" == "" ]] && series=unknown
    [[ "$season" == "" ]] && season=unknown
    # Workaround for The Voice UK
    echo $TORRENT | grep -q The.Voice.UK
    if [[ $? == 0 && "$series" == "The Voice" ]]; then
        series="The Voice UK"
    fi 
    #echo "$type $series $year $season"
    return 0
}

#guessit "$1" 
#echo "$type $series $year $season"
