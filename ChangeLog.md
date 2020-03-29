# Revision history for staversion

## 0.2.3.4  -- 2020-03-29

* Confirm test with `QuickCheck-2.14`.

## 0.2.3.3  -- 2020-01-02

* Confirm test with `megaparsec-8.0.0` and `base-4.13`.

## 0.2.3.2  -- 2019-10-05

* Support the following major versions of dependencies.

    * Cabal-3.0
    * optparse-applicative-0.15
    * semigroups-0.19

## 0.2.3.1  -- 2019-04-20

* Support the following major versions of dependencies.

    * base-4.12
    * yaml-0.9
    * yaml-0.10
    * yaml-0.11
    * containers-0.6
    * http-client-0.6
    * megaparsec-7.0
    * Cabal-2.4
    * QuickCheck-2.12
    * QuickCheck-2.13


## 0.2.3.0  -- 2018-07-10

* Now it reads "stack.yaml" file as a query (#4).
* Now if no query argument is specified, "stack.yaml" is implied by default.
* Now it warns if it gets no result. This is necessary now that it's
  not an error to specify no query argument.


## 0.2.2.0  -- 2018-07-01

* Add `--stack` and `--stack-default` (`-S`) options (#4).
* Now `--stack-default` option is implied if there is no package source specified (#4).
* Support `aeson-1.4` and `megaparsec-6.5.0`.
* [Bug fix] `-H` option: It now proceed if hackage returns 404
  error. That is probably because the user specified a package not on
  hackage, so it's not fatal.

## 0.2.1.4  -- 2018-03-26

* Support `base-4.11.0.0`.


## 0.2.1.3  -- 2018-03-09

* Support `aeson-1.3.0.0`, `base-4.10.0.0`, `Cabal-2.2.0.0`,
  `http-types-0.12.1`, `megaparsec-6.4.1`, `QuickCheck-2.11.3`,
  `transformers-0.6.0.6`


## 0.2.1.2  -- 2017-07-29

* Fix build and test with `megaparsec-4`.


## 0.2.1.1  -- 2017-07-29

* Support `megaparsec-6.0`, with compatibility wrapper "Staversion.Internal.Megaparsec".
* Support `Cabal-2.0`, with compatibility wrapper "Staversion.Internal.Version".


## 0.2.1.0  -- 2017-06-18

* Add `--format-version` option.
* `--format-version cabal-caret` option uses the caret operator (`^>=`) if possible. (#2)


## 0.2.0.0  -- 2017-05-14

* [breaking change] `pvp` aggregator now does "trailing-zero
  normalization". For example, it now assumes versions "2.2" and
  "2.2.0.0" are practically the same (#2).
* Add `pvp-major` aggregator, which is just an alias for `pvp`
  aggregator.
* Add `pvp-minor` aggregator, which is similar to `pvp-major` but it
  uses minor versions for upper bounds (#2).


## 0.1.4.0  -- 2017-04-08

* Add `--aggregate` option, which aggregates versions in different LTS resolvers.
  There are `or` and `pvp` aggregators.
* Bug fix: when it fails to load a given .cabal file, now it continues processing the next target.


## 0.1.3.2  -- 2017-01-05

* Fix dependency lower bound for `base`.
  It was `>=4.6`, but now it's `>=4.8` due to dependency on `megaparsec`.

## 0.1.3.1  -- 2017-01-03

* Now staversion can parse the "curly brace" format of .cabal files (to some extent.)
* Confirmed test with `aeson-1.1.0.0`.

## 0.1.3.0  -- 2016-12-29

* Now staversion shows the exact resolver for a partial resolver (e.g. "lts-4" -> "lts-4.2")
* Now staversion reads .cabal files, and uses their `build-depends` fields as query.
* Fix minor error in ordering the result.

## 0.1.2.0  -- 2016-11-10

* New option `--hackage`, which searches hackage for the latest version number.

## 0.1.1.0  -- 2016-11-03

* Now staversion fetches build plan YAML files over network, if necessary.
* Now staversion disambiguates partial resolvers (e.g. "lts-2") into exact resolvers (e.g. "lts-2.22").
* New option `--no-network`, which forbids staversion to access network.

## 0.1.0.0  -- 2016-10-16

* First version. Released on an unsuspecting world.
