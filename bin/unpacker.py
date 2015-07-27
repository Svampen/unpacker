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
def find_files(path):
    files = dict([('rar', []), ('video', [])])
    for dirname, dirnames, filenames in os.walk(path):
        for subdirname in dirnames:
            # ignore directories (don't walk into them)
            regex = "sample"
            m = re.match(regex, subdirname, re.IGNORECASE)
            if m.group():
                dirnames.remove(m.group())
            print(os.path.join(dirname, subdirname))
        for filename in filenames:
            if filename.endswith(".rar"):
                #TODO: check for part*.rar files
                files['rar'].append(os.path.join(dirname, filename))
            elif filename.endswith(".mkv"):
                files['video'].append(os.path.join(dirname, filename))
            elif filename.endswith(".avi"):
                files['video'].append(os.path.join(dirname, filename))
            elif filename.endswith(".mp4"):
                files['video'].append(os.path.join(dirname, filename))
            print(os.path.join(dirname, filename))
    return files
    
        
torrent, dest, label = args()
t = Torrent(torrent, dest, label)
print(t.torrent, t.dest, t.label)
t.collect_info()
print(t.files['rar'])
print(t.files['video'])
