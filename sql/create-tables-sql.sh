#!/bin/bash
#
# This is run as a shell script and not as an SQL file because we interpolate
# environment variables in the script below.
#
mariadb -u${MARIADB_USER} -p${MARIADB_PASSWORD} ${MARIADB_DATABASE} <<EOF
CREATE TABLE news (
  id              INT(8) PRIMARY KEY AUTO_INCREMENT,
  poster          CHAR(10),
  text            TEXT,
  ptime           DATETIME
);

INSERT INTO news VALUES
  (0, "${ETS_ADMIN_NEPTUN_CODE}", "Üdvözlünk az ETS-ben!", NOW());

CREATE TABLE people (
  neptun          CHAR(10) PRIMARY KEY,
  name            VARCHAR(64) NOT NULL DEFAULT "",
  email           VARCHAR(64) NOT NULL DEFAULT "",
  admin           BOOLEAN NOT NULL DEFAULT FALSE,
  god             BOOLEAN NOT NULL DEFAULT FALSE,
  licence         BOOLEAN NOT NULL DEFAULT FALSE
);

INSERT INTO people (neptun, name, admin, god) VALUES
  ("${ETS_ADMIN_NEPTUN_CODE}", "DP Admin", TRUE, TRUE);

CREATE TABLE session (
  sid             CHAR(36) PRIMARY KEY, -- uuid4
  data            TEXT,
  last            DATETIME
);

CREATE TABLE log (
  user            CHAR(10),
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
  user            CHAR(10) NOT NULL,
  answer          VARCHAR(64),
  time            DATETIME,
  PRIMARY KEY     (query, user),
  INDEX           (query)
);

CREATE TABLE archive_specs (
  name            VARCHAR(64) PRIMARY KEY,
  spec            TEXT
);

INSERT INTO archive_specs VALUES
  ("semester", "people:name scores_meta:idx-idx scores:neptun,id queries responses groups group_members group_times:groupID,startsAt presence"),
  ("scores", "scores:id");

CREATE TABLE scores_meta (
  idx             INT(4) PRIMARY KEY AUTO_INCREMENT,
  id              CHAR(10) NOT NULL,
  name            VARCHAR(64),
  formula         VARCHAR(255),
  UNIQUE          (id)
);

CREATE TABLE scores (
  id              CHAR(10) NOT NULL,
  neptun          CHAR(10) NOT NULL,
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
