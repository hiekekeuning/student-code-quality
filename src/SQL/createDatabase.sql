BEGIN TRANSACTION;
CREATE TABLE `startup_event` (
	`id`	INTEGER,
	`session_id`	INTEGER,
	`time`	INTEGER,
	`user_id`	INTEGER
);
CREATE TABLE "snapshot" (
	`sf_id`	INTEGER NOT NULL,
	`event_id`	INTEGER NOT NULL,
	`name`	TEXT NOT NULL,
	PRIMARY KEY(`sf_id`,`event_id`)
);
CREATE TABLE "loc" (
	`filename`	TEXT,
	`loc`	INTEGER,
	`sfid`	INTEGER
);
CREATE TABLE issue3(
  name TEXT,
  count INT,
  info TEXT,
  source_file_id INT,
  event_id INT,
  filename TEXT
);
CREATE TABLE "issue" (
	`name`	TEXT,
	`count`	INTEGER,
	`info`	TEXT,
	`source_file_id`	INTEGER,
	`event_id`	INTEGER,
	`filename`	TEXT
);
CREATE TABLE "extension" (
	`event_id`	INTEGER,
	`name`	TEXT
);
CREATE TABLE "event" (
	`id`	INTEGER NOT NULL,
	`sequence_num`	INTEGER NOT NULL,
	`session_id`	INTEGER NOT NULL,
	`time`	INTEGER,
	`user_id`	INTEGER,
	PRIMARY KEY(`id`)
);
CREATE TABLE `category` (
	`issue`	TEXT,
	`cat`	TEXT,
	PRIMARY KEY(`issue`)
);
CREATE INDEX `startup_sess` ON `startup_event` (`session_id` );
CREATE INDEX `startup` ON `startup_event` (`id` );
CREATE INDEX `snapshot_fn` ON `snapshot` (`name` );
CREATE INDEX `snapshot_evid` ON `snapshot` (`event_id` );
CREATE INDEX `loc_sfid` ON `loc` (`sfid` );
CREATE INDEX `loc_fn` ON `loc` (`filename` )
;
CREATE INDEX `issue_name` ON `issue` (`name` );
CREATE INDEX `issue_fn` ON `issue` (`filename` );
CREATE INDEX `issue_evid` ON `issue` (`event_id` );
CREATE INDEX issue3se ON issue3(source_file_id, event_id);
CREATE INDEX issue3s ON issue3(source_file_id);
CREATE INDEX issue3e ON issue3(event_id);
CREATE INDEX `ex_e` ON `extension` (`event_id` );
CREATE INDEX `event_sess` ON `event` (`session_id` );
CREATE INDEX `event_id` ON `event` (`id` );
CREATE VIEW issue2 AS

select * from issue where name <> "Duplicate50"

union all

select name, count(*), '', source_file_id, event_id, filename from issue
where name = "Duplicate50"
group by filename

union all

select 'Duplicate100', count(*) count, '', source_file_id, event_id, filename from issue
where name = "Duplicate50" 
AND cast((substr(info, 1, instr(info, '-'))) as Integer) >= 100
group by filename;
COMMIT;
