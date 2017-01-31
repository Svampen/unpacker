from unrar import rarfile
import sys, getopt, os, re, guessit, rarfile

# Usage information
def usage():
    print(sys.argv[0], "-d <path> -l <tv|movie> -t <torrent>")
    print("-d, --destination <path>")
    print("-l, --label <tv|movie>")
    print("-t, --torrent <full path to torrent>")

def args():
    torrentpath = ""
    torrentname = ""
    label = ""
    dest = ""
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hd:l:t:",
                                   ["help", "destination=",
                                    "label=", "torrent="])
    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit(2)

    if opts:
        for opt, arg in opts:
            if opt in ("-h", "--help"):
                usage()
                sys.exit(0)
            elif opt in ("-d", "--destination"):
                print("Destination:", arg)
                dest = verify_dest(arg)
            elif opt in ("-l", "--label"):
                print("Label:", arg)
                label = verify_label(arg)
            elif opt in ("-t", "--torrent"):
                print("Torrent:", arg)
                torrentpath, torrentname = verify_torrent(arg)
    else:
        usage()
        sys.exit(2)

    if torrentpath == "" or torrentname == "":
        print("Torrent argument is missing")
        usage()
        sys.exit(2)
    elif label == "":
        print("Label argument is missing")
        usage()
        sys.exit(2)
    elif dest == "":
        print("Destination argument is missing")
        usage()
        sys.exit(2)
    return torrentpath, torrentname, dest, label

def verify_label(label):
    if label == "tv" or label == "movie":
        return label
    else:
        print("Wrong label used")
        usage()
        sys.exit(2)

def verify_dest(dest):
    if os.path.exists(dest):
        return dest
    else:
        print("Destination doesn't exist:", dest)
        usage()
        sys.exit(2)

def verify_torrent(torrent):
    if os.path.exists(torrent):
        torrentpath, torrentname = os.path.split(torrent)
        return torrentpath, torrentname
    else:
        print("Torrent path doesn't exist:", torrent)
        usage()
        sys.exit(2)

# Torrent class
class Torrent:
    def __init__(self, path, name, dest, label):
        self.name = name
        self.path = path
        self.dest = dest
        self.label = label
    def collect_info(self):
        self.files = find_files(os.path.join(self.path, self.name))
        guessit_info(self)

# Find all relavante files in the torrent directory
def find_files(dirorfile):
    print("find files in ", dirorfile)
    files = dict([('rar', []), ('video', [])])
    if os.path.isfile(dirorfile):
        file_check(dirorfile, os.path.dirname(dirorfile), files)
        
    for dirname, dirnames, filenames in os.walk(dirorfile):
        for subdirname in dirnames:
            # ignore certain directories (don't walk into them)
            regex = "sample"
            m = re.match(regex, subdirname, re.IGNORECASE)
            if m:
                dirnames.remove(subdirname)
        for filename in filenames:
            file_check(filename, dirname, files)
    # If rar file(s) exist, look into the rar file for video files
    for rar in files['rar']:
        rar_check(rar)
    return files

def file_check(filename, dirname, files):
    # Find rar files
    if filename.lower().endswith(".rar"):
        # See if any of the rar files are "part" files
        m = re.search("\.part[0-9]+\.rar", filename, re.IGNORECASE)
        if m:
            # We only need the part1.rar file
            if m.group().lower() == ".part1.rar":
                files['rar'].append(os.path.join(dirname, filename))
        else:
            # Adding the rar file
            files['rar'].append(os.path.join(dirname, filename))
    # Find any videos by the known file extensions
    elif filename.lower().endswith((".mkv", ".avi", ".mp4")):
        files['video'].append(os.path.join(dirname, filename))

def rar_check(rar):
    files = rarfile.Rarfile(rar)
    for f in files:
        print(f.filename)

def guessit_info(torrent):
    # Run guessit based on label
    if torrent.label == 'tv':
        print("torrent is a tv show")
        guess = guessit.guess_episode_info(torrent.name)
        print(guess)
        # TODO: Check if show has episodeNumber or not. If not then
        # this may be a season pack Friends.S01.720p.Bluray-group
    elif torrent.label == 'movie':
        print("torrent is a movie")
        guess = guessit.guess_movie_info(torrent.name)
        print(guess)
        
torrentpath, torrentname, dest, label = args()
t = Torrent(torrentpath, torrentname, dest, label)
print(t.path, t.name, t.dest, t.label)
t.collect_info()
print("rarfiles", t.files['rar'])
print("video files", t.files['video'])
