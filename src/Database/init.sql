CREATE TABLE user (id INTEGER PRIMARY KEY, username text UNIQUE, password text, session_id text);
INSERT INTO user (username, password, session_id) VALUES ('admin', "admin", NULL);
