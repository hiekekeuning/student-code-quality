select ex.name, count(*), sum(loc), count(distinct sf_id),
count(distinct s.name) 
from extension ex
inner join startup_event se on ex.event_id=se.id
inner join event ev on se.session_id=ev.session_id
inner join snapshot s on s.event_id=ev.id
inner join loc l on l.filename=s.name
group by ex.name;