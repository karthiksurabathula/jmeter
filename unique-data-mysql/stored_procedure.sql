use <database>;
/*********************************************************************************/
/*************************Create additional colums required for each table********/
/*********************************************************************************/
/****Add additional columns****/
ALTER TABLE <database>.<tablename> ADD COLUMN (
    d_id INT auto_increment,
    d_count INT NOT NULL,
    d_state varchar(1) NOT NULL DEFAULT 'A',
	d_datetime DATETIME,
	d_accessed VARCHAR(255),
    PRIMARY KEY (d_id),
    INDEX (d_count)
);
/*********************************************************************************/
/*************************Stored Procedures***************************************/
/****Individual stored procedure need to be added for each csv file uploaded.*****/
/*********************************************************************************/
/****Sequential****/
DELIMITER $$
CREATE PROCEDURE <database>.<tablename>_seq(IN accessedby VARCHAR(255)) 
BEGIN
	SELECT @id:=d_id,@count:=d_count FROM <database>.<tablename> WHERE d_state='A' AND d_count=(SELECT min(d_count) FROM <database>.<tablename> FOR SHARE SKIP LOCKED) LIMIT 1 FOR UPDATE SKIP LOCKED;
	UPDATE <database>.<tablename> SET d_count=@count+1,d_datetime=SYSDATE(),d_accessed=accessedby WHERE d_id=@id and d_state='A';
    SELECT * FROM <database>.<tablename> WHERE d_id=@id and d_state='A';
END$$
DELIMITER ;
/****Unique****/
DELIMITER $$
  CREATE PROCEDURE <database>.<tablename>_unique(IN accessedby VARCHAR(255))
  BEGIN
	set @id=null;
    SELECT @id:=d_id,@count:=d_count FROM <database>.<tablename> WHERE d_state='A' and d_count=(SELECT min(d_count) FROM <database>.<tablename> FOR SHARE SKIP LOCKED) LIMIT 1 FOR UPDATE SKIP LOCKED;
    IF (@id IS NOT NULL) then
		UPDATE <database>.<tablename> SET d_count=@count+1,d_datetime=SYSDATE(),d_state='L',d_accessed=accessedby WHERE d_id=@id and d_state='A';
		SELECT * FROM <database>.<tablename> WHERE d_id=@id and d_state='L';
	else
		Select "UniqueDataNotFound";
    end if;
END $$
/****Clear Locked****/
DELIMITER $$
CREATE PROCEDURE <database>.<tablename>_unique_clear(IN accessedby VARCHAR(255),IN id VARCHAR(255))
BEGIN
	UPDATE <database>.<tablename> SET d_datetime=SYSDATE(),d_state='A',d_accessed=accessedby WHERE d_id=CAST(id AS UNSIGNED) and d_state='L';
	SELECT * FROM <database>.<tablename> WHERE d_id=CAST(id AS UNSIGNED) and d_state='A';
END$$
DELIMITER ;
/*********************************************************************************/
/*************************Reset Data Access***************************************/
/*********************************************************************************/
SET SQL_SAFE_UPDATES = 0;
Update <database>.<tablename> set d_state='A',d_count=0 ;
Update <database>.<tablename> set d_state='A',d_count=0 ;
SET SQL_SAFE_UPDATES = 1;
/****Show database****/
SELECT * FROM <database>.<tablename> LIMIT 10;