-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 13 THEN
    EXECUTE 'ALTER TABLE "tx" DROP COLUMN "ex_unit_fee"' ;
    EXECUTE 'ALTER TABLE "tx_in" ADD CONSTRAINT "tx_in_redeemer_id_fkey" FOREIGN KEY("redeemer_id") REFERENCES "redeemer"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" ADD CONSTRAINT "stake_deregistration_redeemer_id_fkey" FOREIGN KEY("redeemer_id") REFERENCES "redeemer"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "delegation_redeemer_id_fkey" FOREIGN KEY("redeemer_id") REFERENCES "redeemer"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "withdrawal" ADD CONSTRAINT "withdrawal_redeemer_id_fkey" FOREIGN KEY("redeemer_id") REFERENCES "redeemer"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'CREATe TABLE "script"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"hash" hash28type NOT NULL,"type" scripttype NOT NULL,"serialised_size" uinteger NULL)' ;
    EXECUTE 'ALTER TABLE "script" ADD CONSTRAINT "unique_script" UNIQUE("hash")' ;
    EXECUTE 'ALTER TABLE "script" ADD CONSTRAINT "script_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
