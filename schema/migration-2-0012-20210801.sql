-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 12 THEN
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "ex_unit_fee" lovelace NOT NULL' ;
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "script_size" uinteger NOT NULL' ;
    EXECUTE 'ALTER TABLE "stake_address" ADD COLUMN "script_hash" hash28type NULL' ;
    EXECUTE 'ALTER TABLE "tx_out" ADD COLUMN "address_has_script" BOOLEAN NOT NULL' ;
    EXECUTE 'ALTER TABLE "tx_in" ADD COLUMN "redeemer_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" ADD COLUMN "redeemer_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "delegation" ADD COLUMN "redeemer_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "withdrawal" ADD COLUMN "redeemer_id" INT8 NULL' ;
    EXECUTE 'CREATe TABLE "redeemer"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"units_mem" word64type NOT NULL,"units_steps" word64type NOT NULL,"fee" lovelace NOT NULL,"purpose" scriptpurposetype NOT NULL,"index" uinteger NOT NULL,"script_hash" hash28type NULL)' ;
    EXECUTE 'ALTER TABLE "redeemer" ADD CONSTRAINT "unique_redeemer" UNIQUE("tx_id","purpose","index")' ;
    EXECUTE 'ALTER TABLE "redeemer" ADD CONSTRAINT "redeemer_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
