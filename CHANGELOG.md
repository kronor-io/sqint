# Changelog

## 0.1.1.0

* The dynamic pass handles reworked changes. A baseline whose name the branch
  reworks again is placed at the occurrence the database has deployed up to,
  read from the sqitch registry; before, it resolved to the branch's own
  occurrence and the pass checked nothing, without saying so. When the
  registry cannot be read it reports the new `setup/baseline-ambiguous`.
* A change reworked more than once after the baseline is deployed as
  `change@tag` and observed from its snapshot script; before, the pass
  stopped with sqitch's "is ambiguous" error.

## 0.1.0.2

* Accept string-interpolate 0.3.4, which sqint builds with; 0.1.0.1 asked
  for 1.0.
* The flake builds on macOS again: 0.1.0.1's libpg_query 18 kept a bare
  install name dyld could not find, so that tag published no binaries.

## 0.1.0.1

* Parse with libpg_query 18 (PostgreSQL 18's grammar), through
  `libpg-query` on Hackage instead of Kronor's `pg-query` fork.
* Read PL/pgSQL trigger bodies again under libpg_query 18.0.0 and 18.1.0,
  whose JSON for a trigger's implicit variables does not decode.

## 0.1.0.0

First release.
