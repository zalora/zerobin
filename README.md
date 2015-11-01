About
=====
This is a library and a command-line utility
to share secrets via "zerobin" sites like https://paste.ec
using client-side encryption with [SJCL](https://crypto.stanford.edu/sjcl/).

This library reimplements encryption part of [SJCL](https://crypto.stanford.edu/sjcl/)
allowing you to post secrets from Haskell programs and shell script.

Requirements
============
ZeroBin is written in Haskell with [GHC](http://www.haskell.org/ghc/).
All required Haskell libraries are listed in [zerobin.cabal](zerobin.cabal).
Use [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install)
to fetch and build all pre-requisites automatically.

Installation
============
    $ git clone https://github.com/zalora/zerobin.git
    $ cd zerobin
    $ cabal install

Command-line utility
====================
The command-line utility `zerobin` encrypts text or file,
post the encrypted data to https://paste.ec and
prints URI to be shared or error message:

    $ zerobin 'heinrich hertz'
    https://paste.ec/paste/1j3GBy-7#dg0PXHFglISOhXzRnU4KLWbSAh5jX5KjX4wZEiYM8QA6


Type `zerobin --help` to see usage summary:

    Usage:
      zerobin [options] TEXT

    Options:
      -e, --expire=E  Set expiration of paste: once, day, week, month [default: week]
      -f, --file      Paste the content of file TEXT instead of plain TEXT

      -h, --help      Show this message

    Examples:
      zerobin hello           paste "hello" for a week
      zerobin -f /etc/fstab   paste file /etc/fstab for a week
      zerobin -e once hello   paste "hello", it will burn after reading


Hacking
=======
There is a simple test program in the [./nodejs](./nodejs) directory.
It uses this library to encrypt a message and original SJCL
running by [Node.js](https://nodejs.org) to decrypt:

    $ # get nodejs and npm, e. g. on Debian; sudo apt-get install nodejs npm
    $ npm install sjcl
    $ git clone https://github.com/zalora/zerobin.git
    $ cd zerobin
    $ cabal install --dependencies-only
    $ cabal install -f nodejs --ghc-option="-Werror"
    $ ./dist/build/zerobin-nodejs/zerobin-nodejs
    heinrich hertz


