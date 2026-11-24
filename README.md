# sqint

sqint lints [sqitch](https://sqitch.org) deploy scripts for PostgreSQL migrations that are dangerous on a busy database: statements that take heavy locks, rewrite or scan whole tables, defeat indexes, or leave SQL functions broken between changes.

```console
$ cat db/deploy/add_orders_status.sql
-- Deploy shop:add_orders_status to pg

begin;

alter table public.orders add column status text not null default 'pending';
alter table public.orders add constraint orders_amount_positive check (amount > 0);
create index orders_customer on public.orders (customer_id);

commit;

$ sqint --database-root db db/deploy/add_orders_status.sql
BLOCK db/deploy/add_orders_status.sql:6 [lock/add-constraint] ADD CONSTRAINT CHECK — scans every row under AccessExclusiveLock
WARN db/deploy/add_orders_status.sql:7 [lock/create-index] CREATE INDEX (non-concurrent) — Share lock blocks writes for the duration of the build
1 informational or suppressed finding hidden — pass --verbose to show.
```

It has two passes:

- **Static**, on the SQL alone. Each statement is parsed with PostgreSQL's own parser ([libpg_query](https://github.com/pganalyze/libpg_query)) and classified by the lock it takes and whether it rewrites the table. It needs no database, so it fits a pre-commit hook.
- **Dynamic**, with `--with-postgres` and `--baseline-change`. sqint deploys every change after the baseline in plan order to a scratch database, and before each one runs its statements in a transaction it rolls back while a second connection reads `pg_locks`. It reports the locks actually taken, table rewrites, deploy failures, and SQL functions a change leaves broken until a later change fixes them. Meant for CI.

## Install

Install it from the flake. CI builds it for x86_64-linux, aarch64-linux and aarch64-darwin and pushes the builds to the `kronor-open` cachix cache, which the flake names, so nix downloads sqint rather than building it once you accept the flake's settings.

```bash
nix run github:kronor-io/sqint -- --help
```

```bash
nix profile install github:kronor-io/sqint
```

Each version tag also publishes an image for amd64 and arm64, with sqitch and psql beside sqint for the dynamic pass:

```bash
docker run --rm -v "$PWD:/work" ghcr.io/kronor-io/sqint --help
```

On Linux without nix, each [release](https://github.com/kronor-io/sqint/releases) has a static binary for x86_64 and aarch64, which runs on any distribution. The tarball holds `bin/sqint` and its bash, zsh and fish completions under `share/`, so unpacking it into `~/.local` (or `/usr/local`) installs both:

```bash
curl -fsSL "https://github.com/kronor-io/sqint/releases/latest/download/sqint-$(uname -m)-linux.tar.xz" | tar -xJ -C ~/.local
```

The binary is sqint alone: the dynamic pass also needs `sqitch` on `PATH`.

In a clone, `nix build` puts the binary in `result/bin`, `nix build .#dockerImage` the image, and `scripts/build-static` the release tarball, in `dist-static/`. `nix develop` gives a shell with GHC, cabal, libpg_query, Postgres and sqitch, where `cabal build` and `cabal test` work.

## Usage

```bash
sqint [--database-root DIR] [--plan-file FILE] [--with-postgres DSN --baseline-change CHANGE] [--format text|json] [--verbose] FILE...
```

| Option | |
|---|---|
| `FILE...` | Deploy scripts to lint, usually the ones a branch adds or changes. |
| `--database-root DIR` | The sqitch top directory, which holds `deploy/`. Default `database`. |
| `--plan-file FILE` | The plan that orders changes for the dynamic pass. Default `DIR/sqitch.plan`. |
| `--with-postgres DSN` | A libpq connection string to a database deployed up to the baseline. Turns on the dynamic pass. |
| `--baseline-change CHANGE` | The last change of the target branch's plan. Every change after it is new and is deployed in order. |
| `--format text\|json` | Text for people, JSON for tools. |
| `--verbose` | Also print informational and suppressed findings. |
| `--deferred-backfills` | Turn on the `deferred/` rule (below). |

sqint exits non-zero when any finding is `BLOCK`.

For the dynamic pass, deploy the target branch to a scratch database, check out the branch under review, and run sqint from the directory with `sqitch.conf`; it runs `sqitch deploy --to-change … --verify` for each new change, with the connection given in the `PG*` environment.

```bash
sqitch deploy db:pg://postgres@localhost/scratch    # on the target branch
git switch my-branch
PGHOST=localhost PGUSER=postgres PGDATABASE=scratch \
  sqint --database-root . --with-postgres "postgres://postgres@localhost/scratch" \
    --baseline-change "$(git show main:sqitch.plan | awk '!/^[%#@]/ && NF {c=$1} END {print c}')" \
    $(git diff --name-only main -- deploy/)
```

## Rules

Every finding has a severity (`BLOCK`, `WARN` or `INFO`) and a `family/rule` ID.

| Family | Rules | |
|---|---|---|
| `lock/` | `add-column`, `alter-column-type`, `add-constraint`, `alter-domain`, `create-index`, `drop-index` | A lock the static pass predicts from the statement, with whether it rewrites or scans the table. |
| | `observed-<mode>` | A lock the dynamic pass saw a statement take, such as `lock/observed-access-exclusive`. |
| `deploy/` | `non-transactional`, `statement-not-observed`, `failed` | The dynamic pass deploying the new changes. |
| `function/` | `broken-body`, `check-skipped` | A SQL function a change leaves failing until a later change fixes it. |
| `index/` | `cast-on-column`, `function-on-column`, `defeated` | A cast or function on a column in a `WHERE`, `JOIN` or `ON CONFLICT` predicate, which stops a plain index on the column being used; `defeated` is confirmed by the planner. |
| `txn/` | `set-local-across-commit` | A `SET LOCAL` or `set_config(…, true)` in a PL/pgSQL body that also commits: only the first batch keeps the setting. |
| `deferred/` | `unfinished-backfill` | Opt-in, see below. |
| `suppression/` | `missing-reason` | A `lint:ignore` without a reason. |
| `setup/` | `unparsable-sql`, `unparsable-domain-file`, `unparsable-plan`, `baseline-missing` | sqint could not run a check; nothing is known to be wrong with the migration. |

### Accepting a finding

Put `-- lint:ignore reason="…"` on the line above the statement or at the end of any of its lines. It silences that statement only, never the next one, and the reason is required.

```sql
-- lint:ignore reason="orders is empty until the launch"
alter table public.orders add constraint orders_amount_positive check (amount > 0);
```

### Deferred backfills

With `--deferred-backfills`, sqint supports projects that backfill large tables outside their migrations. A change registers a backfill with `insert into deferred.migration (…)`, naming the table; a later change that validates a constraint or sets `NOT NULL` on that table must call `deferred.ensure_finished(…)` first, or it would pass on the empty tables of development and CI and then fail, or run the backfill inline, in production. Projects without that convention leave the flag off.

## Tests

`cabal test` (or `nix flake check`) runs the static rules against SQL written in the tests. The dynamic pass needs a running PostgreSQL and is not covered by them.

## Licence

MIT
