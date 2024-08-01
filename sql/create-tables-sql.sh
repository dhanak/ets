#!/bin/bash
#
# This is run as a shell script and not as an SQL file because we interpolate
# environment variables in the script below.
#
mariadb -u${MARIADB_USER} -p${MARIADB_PASSWORD} -D${MARIADB_DATABASE} <<EOF
CREATE TABLE news (
  id              INT(8) PRIMARY KEY AUTO_INCREMENT,
  poster          CHAR(6),
  text            TEXT,
  ptime           DATETIME
);

INSERT INTO news VALUES
  (0, "ADMIN", "Üdvözlünk az ETS-ben!", NOW());

CREATE TABLE links (
  id              INT(8) PRIMARY KEY AUTO_INCREMENT,
  type            CHAR(4),
  name            VARCHAR(64),
  url             VARCHAR(255)
);

CREATE TABLE people (
  neptun          CHAR(6) PRIMARY KEY,
  name            VARCHAR(64),
  email           VARCHAR(64),
  password        VARCHAR(32),
  admin           CHAR(1) DEFAULT "N",
  god             CHAR(1) DEFAULT "N",
  licence         CHAR(1) DEFAULT "N" -- FIXME
);

INSERT INTO people VALUES
  ("ADMIN",
   "DP Admin",
   "${CONTACT:-dp@iit.bme.hu}",
   "${ETS_ADMIN_PASSWORD}",
   "Y",
   "N",
   "N")
;
CREATE TABLE session (
  sid             CHAR(36) PRIMARY KEY, -- uuid4
  data            BLOB,
  last            DATETIME
);

CREATE TABLE categories (
  id              CHAR(4) PRIMARY KEY,
  name            VARCHAR(64) NOT NULL,
  chapter         INT(4)
);

CREATE TABLE catgroups (
  rank            INT(4) PRIMARY KEY,
  name            VARCHAR(64) NOT NULL,
  members         BLOB
);

CREATE TABLE levels (
  id              INT(4) PRIMARY KEY,
  name            VARCHAR(64) NOT NULL
);

INSERT INTO levels VALUES
  ("0", "könnyû"),
  ("1", "közepes"),
  ("2", "nehéz");

CREATE TABLE exercises (
  id              INT(8) PRIMARY KEY AUTO_INCREMENT,
  category        CHAR(4) NOT NULL,
  scheme          CHAR(4) NOT NULL,
  level           INT(4) NOT NULL,
  data            BLOB
);

CREATE TABLE scripts (
  id              INT(8) PRIMARY KEY AUTO_INCREMENT,
  name            VARCHAR(64) NOT NULL,
  script          TEXT,
  UNIQUE          (name)
);

CREATE TABLE progress (
  neptun          CHAR(6) NOT NULL,
  category        CHAR(4) NOT NULL,
  done            INT(4),
  PRIMARY KEY     (neptun, category)
);

CREATE TABLE complete (
  neptunSID       CHAR(36) NOT NULL,
  exercise        INT(8) NOT NULL,
  count           INT(4),
  PRIMARY KEY     (neptunSID, exercise)
);

CREATE TABLE log (
  user            CHAR(6),
  stamp           DATETIME,
  event           VARCHAR(64),
  params          TEXT
);

CREATE TABLE queries (
  id              INT(8) PRIMARY KEY AUTO_INCREMENT,
  type            ENUM("yesno", "text", "multichoice") NOT NULL,
  text            VARCHAR(64),
  deadline        DATE
);

CREATE TABLE responses (
  query           INT(8) NOT NULL,
  user            CHAR(6) NOT NULL,
  answer          VARCHAR(64),
  time            DATETIME,
  PRIMARY KEY     (query, user),
  INDEX           (query)
);

CREATE TABLE parameters (
  id              CHAR(16) PRIMARY KEY,
  value           TEXT
);

CREATE TABLE scores_meta (
  idx             INT(4) PRIMARY KEY AUTO_INCREMENT,
  id              CHAR(10) NOT NULL,
  name            VARCHAR(64),
  formula         VARCHAR(255),
  UNIQUE          (id)
);

CREATE TABLE scores (
  id              CHAR(10) NOT NULL,
  neptun          CHAR(6) NOT NULL,
  value           INT(4),
  notes           VARCHAR(255),
  PRIMARY KEY     (id, neptun),
  INDEX           (id),
  INDEX           (neptun)
);

CREATE TABLE templates (
  name            CHAR(64) NOT NULL,
  type            ENUM("SQL", "mail") NOT NULL,
  contents        TEXT,
  PRIMARY KEY     (name, type)
);
EOF
