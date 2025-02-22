INSERT INTO groupscholar_case_closure.case_closures
  (case_id, scholar, program, status, closed_on, owner, summary, next_step, risk_level)
VALUES
  ('CC-2026-001', 'Amira Patel', 'STEM Scholars', 'closed-won', '2026-01-28', 'Jordan Lee',
   'Scholar accepted full award package; onboarding completed.',
   'Schedule first semester check-in.', 'low'),
  ('CC-2026-002', 'Luis Hernandez', 'Bridge Grant', 'closed-lost', '2026-01-18', 'Renee Clark',
   'Unable to verify enrollment; withdrew request.',
   'Offer re-apply reminder in fall.', 'medium'),
  ('CC-2026-003', 'Zara Okafor', 'Transfer Pathway', 'follow-up', NULL, 'Jordan Lee',
   'Awaiting final transcript and FAFSA confirmation.',
   'Collect documents; review in 2 weeks.', 'high'),
  ('CC-2026-004', 'Noah Kim', 'Leadership Fund', 'closed-won', '2026-02-02', 'Sam Rivera',
   'Decision letter signed and uploaded to record.',
   'Send donor update and alumni intro.', 'low'),
  ('CC-2026-005', 'Janelle Brooks', 'Opportunity Award', 'follow-up', NULL, 'Renee Clark',
   'Missing employment verification; paused for now.',
   'Coordinate with program partner for verification.', 'medium');
