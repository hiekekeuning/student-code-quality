-- June
select cat, sum(t) from
(
select n, total(x)/72519 t from (
 select i.name n, total(count)/(SELECT sum(loc) FROM loc WHERE sfid=sf_id)*1000 x from 
    snapshot s inner join issue3 i on (i.event_id=s.event_id AND sf_id=source_file_id  )
  where s.event_id in (select id from event where strftime('%m', time/1000, 'unixepoch')="06")
  and i.name <> "LawOfDemeter"
  group by sf_id, i.name
  )
group by n
)
inner join category on issue=n
group by cat;

-- March
select cat, sum(t) from
(
select n, total(x)/129487 t from (
 select i.name n, total(count)/(SELECT sum(loc) FROM loc WHERE sfid=sf_id)*1000 x from 
    snapshot s inner join issue3 i on (i.event_id=s.event_id AND sf_id=source_file_id  )
  where s.event_id in (select id from event where strftime('%m', time/1000, 'unixepoch')="03")
  and i.name <> "LawOfDemeter"
  group by sf_id, i.name
  )
group by n
)
inner join category on issue=n
group by cat;

-- September
select cat, sum(t) from
(
select n, total(x)/109273 t from (
 select i.name n, total(count)/(SELECT sum(loc) FROM loc WHERE sfid=sf_id)*1000 x from 
    snapshot s inner join issue3 i on (i.event_id=s.event_id AND sf_id=source_file_id  )
  where s.event_id in (select id from event where strftime('%m', time/1000, 'unixepoch')="09")
  and i.name <> "LawOfDemeter"
  group by sf_id, i.name
  )
group by n
)
inner join category on issue=n
group by cat;

-- December
select cat, sum(t) from
(
select n, total(x)/142681 t from (
 select i.name n, total(count)/(SELECT sum(loc) FROM loc WHERE sfid=sf_id)*1000 x from 
    snapshot s inner join issue3 i on (i.event_id=s.event_id AND sf_id=source_file_id  )
  where s.event_id in (select id from event where strftime('%m', time/1000, 'unixepoch')="12")
  and i.name <> "LawOfDemeter"
  group by sf_id, i.name
  )
group by n
)
inner join category on issue=n
group by cat;