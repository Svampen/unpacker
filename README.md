unpacker
========

Usage of unpacker.ksh
  -torrent <torret name>            [M]   Name/folder of the torrent
  -torrentpath <path to torrent>    [M]   Path to torrent excluding name/folder
  -envfile <file>                   [O]   Used to output env variables to files
                                          usable for Jenkins
  -guessitopt "<guessit options>"   [O]   Guessit passthrough options
  -no                               [O]   Dummys the script, for debuging
  -tvfolder <folder>                [MO]  Extraction path for TvShows
  -moviefolder <folder>             [MO]  Extraction path for Movies
                                          If only -tvfolder or -moviefolder is
                                          is present type will be forced to that type
  
  -help                                   show this help information
M=mandatory, O=optional
