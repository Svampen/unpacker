from unrar import rarfile
import sys, getopt, os, re

# Usage information
def usage():
    print(sys.argv[0], "-d <path> -l <tv|movie> -t <torrent>")
    print("-d, --destination <path>")
    print("-l, --label <tv|movie>")
    print("-t, --torrent <full path to torrent>")

def args():
    torrent = ""
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
                torrent = verify_torrent(arg)
    else:
        usage()
        sys.exit(2)

    if torrent == "":
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
    return torrent, dest, label

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
        return torrent
    else:
        print("Torrent path doesn't exist:", torrent)
        usage()
        sys.exit(2)

# Torrent class
class Torrent:
    def __init__(self, torrent, dest, label):
        self.torrent = torrent
        self.dest = dest
        self.label = label
    def collect_info(self):
        self.files = find_files(self.torrent)

# Find all relavante files in the torrent directory
def find_files(pathorfile):
    files = dict([('rar', []), ('video', [])])
    if os.path.isfile(pathorfile):
        file_check(pathorfile, os.path.dirname(pathorfile), files)
        
    for dirname, dirnames, filenames in os.walk(pathorfile):
        for subdirname in dirnames:
            # ignore certain directories (don't walk into them)
            regex = "sample"
            m = re.match(regex, subdirname, re.IGNORECASE)
            if m:
                dirnames.remove(subdirname)
        for filename in filenames:
            file_check(filename, dirname, files)

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
    
        
torrent, dest, label = args()
t = Torrent(torrent, dest, label)
print(t.torrent, t.dest, t.label)
t.collect_info()
print("rarfiles", t.files['rar'])
print("video files", t.files['video'])
