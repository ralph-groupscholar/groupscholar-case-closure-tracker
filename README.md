# groupscholar-case-closure-tracker

CLI for logging scholar case closures, follow-ups, and risk-level summaries for Group Scholar ops.

## Features
- Track case closures with owner, program, and risk metadata
- Summaries by status and risk level
- Quick listing with filters for owner, status, and risk
- Postgres schema with seed data for immediate reporting

## Tech
- Common Lisp (SBCL)
- PostgreSQL

## Setup
1. Ensure `sbcl` and `psql` are installed.
2. Export `DATABASE_URL` for production (do not hardcode credentials).
3. Initialize the database:

```bash
bin/gs-case-closure init-db --seed
```

## Usage
```bash
bin/gs-case-closure add \
  --case-id CC-2026-006 \
  --scholar "Elena Park" \
  --program "Promise Scholars" \
  --status "closed-won" \
  --closed-on 2026-02-08 \
  --owner "Jordan Lee" \
  --summary "Award letter signed." \
  --next-step "Schedule onboarding." \
  --risk-level low

bin/gs-case-closure list --status follow-up --limit 20
bin/gs-case-closure summary
```

## Tests
```bash
make test
```
