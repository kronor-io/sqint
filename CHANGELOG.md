# Changelog

## 0.1.0.1

* Parse with libpg_query 18 (PostgreSQL 18's grammar), through
  `libpg-query` on Hackage instead of Kronor's `pg-query` fork.
* Read PL/pgSQL trigger bodies again under libpg_query 18.0.0 and 18.1.0,
  whose JSON for a trigger's implicit variables does not decode.

## 0.1.0.0

First release.
