-- -*- sql-product: mariadb -*-

DELETE FROM people
 WHERE ADMIN <> 'Y' AND
       CHAR_LENGTH(neptun) = 6 AND
       neptun NOT IN ("neptun1", "neptun2") AND
       LOCATE('(', name) = 0;

DELETE FROM session;
DELETE FROM progress;
DELETE FROM complete;
DELETE FROM log;
DELETE FROM queries;
DELETE FROM responses;
DELETE FROM scores;
DELETE FROM group_members;
DELETE FROM group_times;
DELETE FROM groups;
DELETE FROM presence;
