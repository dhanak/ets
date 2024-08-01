-- -*- sql-product: mariadb -*-
CREATE EVENT cleanup_sessions ON SCHEDULE EVERY 5 MINUTE DO
  DELETE FROM session
  WHERE NOW() > DATE_ADD(last, INTERVAL 1 HOUR);

CREATE EVENT cleanup_completed ON SCHEDULE EVERY 5 MINUTE DO
  DELETE FROM complete
  WHERE CHAR_LENGTH(neptunSID) = 36 AND
  neptunSID NOT IN (SELECT sid FROM session);
