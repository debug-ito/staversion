# staversion

![travis status](https://api.travis-ci.org/debug-ito/staversion.png)

staversion is a command-line tool to look for version numbers of Haskell packages. It mainly focuses on package versions in stackage, i.e. it answers to questions like "What version is the package X in stackage lts-Y.ZZ?" It aims to make it easier to write `build-depends` section in YOUR_PACKAGE.cabal.

    $ staversion --resolver lts-4.2 conduit
    ------ lts-4.2
    conduit ==1.2.6.1
    
    $ staversion --resolver lts-4.2 --resolver lts-7.0 conduit
    ------ lts-4.2
    conduit ==1.2.6.1
    
    ------ lts-7.0
    conduit ==1.2.7
    
    $ staversion --resolver lts-4.2 --resolver lts-6 conduit base
    ------ lts-4.2
    conduit ==1.2.6.1,
    base ==4.8.2.0
    
    ------ lts-6
    conduit ==1.2.8,
    base ==4.8.2.0

staversion first reads build plan YAML files that are stored locally in your computer, then it tries to fetch them over network.

You can also look up the latest version numbers hosted on hackage.

    $ staversion --hackage conduit base
    ------ latest in hackage
    conduit ==1.2.8,
    base ==4.9.0.0

See `--help` message for detail.


## TODO

- Read `build-depends` sections .cabal files for package name queries.
- Show version number ranges supported by the given resolvers.
- Cache build plans in some local storage (SQLite?)

## Author

Toshio Ito <debug.ito@gmail.com>
