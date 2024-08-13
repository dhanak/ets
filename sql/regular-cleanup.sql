-- -*- sql-product: mariadb -*-
CREATE EVENT cleanup_sessions ON SCHEDULE EVERY 5 MINUTE DO
  DELETE FROM session
  WHERE NOW() > DATE_ADD(last, INTERVAL 1 HOUR);
