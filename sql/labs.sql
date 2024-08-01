-- -*- sql-product: mariadb -*-

-- dp hallgatói csoportbeosztás

DROP TABLE IF EXISTS presence;
DROP TABLE IF EXISTS group_times;
DROP TABLE IF EXISTS group_members;
DROP TABLE IF EXISTS groups;

-- csoportok
CREATE TABLE groups (
  groupID         INTEGER PRIMARY KEY, -- csoportazonosító
  head            CHAR(6) NOT NULL    -- a csoport konzulense
);

-- hallgatók csoportbeosztása
CREATE TABLE group_members (
  neptun           CHAR(6),
  groupID          INTEGER,
  PRIMARY KEY      (neptun, groupID)
);

-- gyakorlatok
CREATE TABLE group_times (
  groupTimeID      INTEGER AUTO_INCREMENT PRIMARY KEY,
  groupID          INTEGER,
  startsAt         DATETIME,
  room             CHAR(6),
  type             ENUM ('class', 'lab') DEFAULT 'class',
  UNIQUE           (groupID, startsAt)
);

-- jelenléti ív
CREATE TABLE presence (
  neptun           CHAR(6),
  scheduledTimeID  INTEGER, -- a hallgató ütemezett alkalma
  status           ENUM ('present', 'absent'),
  actualTimeID     INTEGER, -- az alkalom, amikor a hallgató a mulasztott alkalmat teljesítette
  PRIMARY KEY (neptun, scheduledTimeID)
);
