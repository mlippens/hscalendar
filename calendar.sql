DROP TYPE IF EXISTS recurtype;
CREATE TYPE recurtype AS ENUM('none','daily','monthly','yearly');

DROP TABLE IF EXISTS events;
CREATE TABLE events (
	id SERIAL PRIMARY KEY,
	title TEXT NOT NULL,
	authuser SERIAL NOT NULL,
	isrecurring BOOLEAN,
	recurinterval recurtype,
	day INT NOT NULL,
	month INT NOT NULL,
	year INT NOT NULL
);

