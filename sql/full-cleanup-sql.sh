#!/bin/bash
docker compose exec -T mariadb mariadb -uets -pets ets <<EOF
DELETE FROM people WHERE NOT admin AND neptun NOT LIKE "TESZT_%";
DELETE FROM session;

DELETE FROM group_members;
DELETE FROM group_times;
DELETE FROM groups;
DELETE FROM log;
DELETE FROM presence;
DELETE FROM queries;
DELETE FROM responses;
DELETE FROM scores;
EOF
