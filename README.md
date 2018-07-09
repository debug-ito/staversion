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

## Package version of your current stack project

If you omit `--resolver` (`-r`) option (or explicitly specify `--stack-default` (`-S`) option), staversion reads `stack.yaml` of your current project, and searches for package versions in the resolver specified in the `stack.yaml`.

    $ staversion conduit
    ------ default stack resolver (lts-10.8)
    conduit ==1.2.13.1


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

## Package version for stack projects

You can also specify stack.yaml file in the query. In that case, staversion reads its `packages` field and treats all .cabal files under those package directories as the query.

    $ staversion --hackage ./staversion/stack.yaml 
    ------ latest in hackage
    -- ./staversion/./staversion.cabal - library
    base ==4.11.1.0,
    unordered-containers ==0.2.9.0,
    aeson ==1.4.0.0,
    text ==1.2.3.0,
    
    (snip)

"stack.yaml" in the query has a special meaning. It means the default stack.yaml of your current project. The "stack.yaml" does not have to be in the working directory. This query is implied by default if you pass no query arguments.

    $ staversion --hackage stack.yaml
    ------ latest in hackage
    -- /home/toshio/programs/git/staversion/./staversion.cabal - library
    base ==4.11.1.0,
    unordered-containers ==0.2.9.0,
    aeson ==1.4.0.0,
    text ==1.2.3.0,
    
    (snip)


## Package version ranges over different resolvers

With `--aggregate` (`-a`) option, you can aggregate version numbers in different resolvers into a version range using the given aggregation rule.

For example, `or` rule just concatenates versions with `(||)` condition.

    $ staversion --aggregate or -r lts-5 -r lts-6 -r lts-7 -H aeson
    ------ lts-5 (lts-5.18), lts-6 (lts-6.31), lts-7 (lts-7.20), latest in hackage
    aeson ==0.9.0.1 || ==0.11.3.0 || ==1.1.1.0

`pvp` rule aggregates versions into a range that should be compatible with the obtained versions in terms of PVP (Package Versioning Policy.)

    $ staversion --aggregate pvp -r lts-5 -r lts-6 -r lts-7 -H aeson
    ------ lts-5 (lts-5.18), lts-6 (lts-6.31), lts-7 (lts-7.22), latest in hackage
    aeson >=0.9.0.1 && <0.10 || >=0.11.3 && <0.12 || ==1.2.*

You can use `--aggregate` option with querying .cabal files.

    $ staversion --aggregate pvp -r lts-6 -r lts-7 -r lts-8 staversion.cabal 
    ------ lts-6 (lts-6.31), lts-7 (lts-7.22), lts-8 (lts-8.13)
    -- staversion.cabal - library
    base >=4.8.2 && <4.10,
    unordered-containers >=0.2.8 && <0.3,
    aeson >=0.11.3 && <0.12 || >=1.0.2.1 && <1.1,
    text >=1.2.2.1 && <1.3,
    bytestring >=0.10.6 && <0.11,
    yaml >=0.8.22 && <0.9,
    filepath ==1.4.*,
    directory >=1.2.2 && <1.4,
    optparse-applicative >=0.12.1 && <0.13 || >=0.13.2 && <0.14,
    containers >=0.5.6.2 && <0.6,
    http-client >=0.4.31.2 && <0.5 || >=0.5.6.1 && <0.6,
    
    (snip)


## TODO

- Cache build plans in some local storage (SQLite?)

## Author

Toshio Ito <debug.ito@gmail.com>
