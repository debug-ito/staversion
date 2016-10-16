# staversion

staversion is a command-line tool to look for version numbers for Haskell packages in specific stackage resolvers. It answers to questions like "What version is the package X in stackage lts-Y.ZZ?" It aims to make it easier to write `build-depends` section in YOUR_PACKAGE.cabal.

    $ staversion --resolver lts-4.2 conduit
    ------ lts-4.2
    conduit ==1.2.6.1
    
    $ staversion --resolver lts-4.2 --resolver lts-7.0 conduit
    ------ lts-4.2
    conduit ==1.2.6.1
    
    ------ lts-7.0
    conduit ==1.2.7
    
    $ staversion --resolver lts-4.2 --resolver lts-7.0 conduit base
    ------ lts-4.2
    conduit ==1.2.6.1,
    base ==4.8.2.0
    
    ------ lts-7.0
    conduit ==1.2.7,
    base ==4.9.0.0

Currently staversion reads build plan YAML files that are stored locally in your computer.


## TODO

- Fetch build plan YAML files from github.com
- Cache build plans in some local storage (SQLite?)
- Expand major version resolvers (lts-X) into full resolvers (lts-X.YY)
- Search for the latest version numbers hosted in hackage.
- Read `build-depends` sections .cabal files for package name queries.
- Show version number ranges supported by the given resolvers.

## Author

Toshio Ito <debug.ito@gmail.com>
