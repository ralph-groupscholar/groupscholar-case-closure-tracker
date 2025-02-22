CREATE SCHEMA IF NOT EXISTS groupscholar_case_closure;

CREATE TABLE IF NOT EXISTS groupscholar_case_closure.case_closures (
  id SERIAL PRIMARY KEY,
  case_id TEXT NOT NULL,
  scholar TEXT NOT NULL,
  program TEXT NOT NULL,
  status TEXT NOT NULL,
  closed_on DATE,
  owner TEXT NOT NULL,
  summary TEXT,
  next_step TEXT,
  risk_level TEXT NOT NULL DEFAULT 'medium',
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS case_closures_status_idx
  ON groupscholar_case_closure.case_closures (status);

CREATE INDEX IF NOT EXISTS case_closures_owner_idx
  ON groupscholar_case_closure.case_closures (owner);

CREATE INDEX IF NOT EXISTS case_closures_risk_idx
  ON groupscholar_case_closure.case_closures (risk_level);
