
ALTER TABLE records ADD COLUMN authnReadPolicyId VARCHAR;
CREATE INDEX records_authnreadpolicyid_idx
    ON test.records USING btree
    (authnreadpolicyid ASC NULLS LAST)
    TABLESPACE pg_default;