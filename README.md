# staversion

![travis status](https://api.travis-ci.org/debug-ito/staversion.png)

staversion is a command-line tool to look for version numbers of Haskell packages.

See `--help` message for detailed usage.

## Package version in Stackage

staversion mainly focuses on package versions in stackage, i.e. it answers to questions like "What version is the package X in stackage lts-Y.ZZ?" It aims to make it easier to write `build-depends` section in YOUR_PACKAGE.cabal.

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
    
    ------ lts-6 (lts-6.31)
    conduit ==1.2.9.1,
    base ==4.8.2.0

staversion first reads build plan YAML files that are stored locally in your computer, then it tries to fetch them over network.

## Package version in Hackage

You can also look up the latest version numbers hosted on hackage with `--hackage` (`-H`) option.

    $ staversion --hackage conduit base
    ------ latest in hackage
    conduit ==1.2.8,
    base ==4.9.0.0

## Package version for build-depends

You can also specify .cabal files in the query. In that case, staversion reads `build-depends` fields in all sections of those .cabal files, and shows versions of the dependency packages.

    $ staversion --hackage staversion.cabal 
    ------ latest in hackage
    -- staversion.cabal - library
    base ==4.9.0.0,
    unordered-containers ==0.2.7.2,
    aeson ==1.0.2.1,
    text ==1.2.2.1,
    bytestring ==0.10.8.1,
    yaml ==0.8.21.1,
    filepath ==1.4.1.1,
    directory ==1.3.0.0,
    optparse-applicative ==0.13.0.0,
    containers ==0.5.9.1,
    http-client ==0.5.5,
    http-client-tls ==0.3.3,
    http-types ==0.9.1,
    transformers ==0.5.2.0,
    transformers-compat ==0.5.1.4,
    megaparsec ==5.1.2
    
    (snip)


## TODO

- Cache build plans in some local storage (SQLite?)

## Author

Toshio Ito <debug.ito@gmail.com>
