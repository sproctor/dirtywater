Welcome to Dirty Water
======================

This is a mud server written in OCaml.

You need to have oasis, ocaml, ocaml-lua, syck, and ocaml-syck installed.

syck: https://github.com/indeyets/syck

ocaml-syck: https://github.com/sproctor/ocaml-syck

To build to program run:

    oasis setup
    ./configure
    make

at the command line. The to run the server type:

    mud [port] [-debug level]

Port must be a valid port number which you have permission to open.

level is a number between 0 and 10

What do the different levels mean?

* 0 - show only the most basic output (start/stop/fatal errors)
* 1 - show important output (new character created, non-fatal errors)
* 2 - show interesting ouput (player logged in or logged out)
* 3 - show non-reoccurring output (anything mildly interesting that won't be
      displayed once every iteration)
* 4 - show the most important reoccurring output
* 5 - show slightly interesting reoccurring output
* 6 through 9 - ?
* 10 - show everything

You can then try login in to the server with a telnet client by typing:

    telnet [host] [port]

You should get a login prompt. Valid logins are defined by the characters
which exist in the game. if a name doesn't exist, one is created.

Check the AUTHORS file for contact info
Check the docs directory for internal code documentation
