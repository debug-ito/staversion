# Revision history for staversion

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
