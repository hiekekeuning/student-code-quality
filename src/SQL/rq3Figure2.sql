-- Checkstyle
select total(ai)*1000/10833 from 
(
	select sf_id, total(tc)/total(ml) ai
	from(
		select sf_id, total(count) tc, loc ml
		from (snapshot s 
		inner join event e on e.id=s.event_id
		inner join loc l on l.filename=s.name)
		left join issue3 i on (i.source_file_id=s.sf_id AND i.event_id=s.event_id and i.name <> "LawOfDemeter")
		where session_id in 
		  (select session_id from startup_event se inner join extension ex
		   on ex.event_id=se.id where name LIKE "Checkstyle")
		group by s.sf_id, s.event_id)
	group by sf_id
);

-- PMD
select total(ai)*1000/4299 from 
(
	select sf_id, total(tc)/total(ml) ai
	from(
		select sf_id, total(count) tc, loc ml
		from (snapshot s 
		inner join event e on e.id=s.event_id
		inner join loc l on l.filename=s.name)
		left join issue3 i on (i.source_file_id=s.sf_id AND i.event_id=s.event_id and i.name <> "LawOfDemeter")
		where session_id in 
		  (select session_id from startup_event se inner join extension ex
		   on ex.event_id=se.id where name LIKE "PMD%")
		group by s.sf_id, s.event_id)
	group by sf_id
);

-- PatternCoder
select total(ai)*1000/609 from 
(
	select sf_id, total(tc)/total(ml) ai
	from(
		select sf_id, total(count) tc, loc ml
		from (snapshot s 
		inner join event e on e.id=s.event_id
		inner join loc l on l.filename=s.name)
		left join issue3 i on (i.source_file_id=s.sf_id AND i.event_id=s.event_id and i.name <> "LawOfDemeter")
		where session_id in 
		  (select session_id from startup_event se inner join extension ex
		   on ex.event_id=se.id where name LIKE "PatternCoder")
		group by s.sf_id, s.event_id)
	group by sf_id
);

-- Some tool
select total(ai)*1000/12442 from 
(
	select sf_id, total(tc)/total(ml) ai
	from(
		select sf_id, total(count) tc, loc ml
		from ((snapshot s 
		inner join event e on e.id=s.event_id)
		inner join loc l on l.filename=s.name)
		left join issue3 i on (i.source_file_id=s.sf_id AND i.event_id=s.event_id AND i.name <> "LawOfDemeter")
		where session_id in 
		  (select session_id from startup_event se inner join extension ex
		  on ex.event_id=se.id)
		group by s.sf_id, s.event_id)
	group by sf_id
);

-- No tool
select total(ai)*1000/436324 from 
(
	select sf_id, total(tc)/total(ml) ai
	from(
		select sf_id, total(count) tc, loc ml
		from (snapshot s 
		inner join event e on e.id=s.event_id
		inner join loc l on l.filename=s.name)
		left join issue3 i on (i.source_file_id=s.sf_id AND i.event_id=s.event_id AND i.name <> "LawOfDemeter")
		where session_id not in 
		  (select session_id from startup_event se inner join extension ex
		  on ex.event_id=se.id)
		and session_id IN (SELECT session_id FROM startup_event)
		group by s.sf_id, s.event_id)
	group by sf_id
);

