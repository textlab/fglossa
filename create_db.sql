CREATE TABLE Search(
    Id INTEGER PRIMARY KEY,
    CorpusCode TEXT NOT NULL,
    Queries TEXT NOT NULL,
    Metadata TEXT,
    CreatedAt NUMERIC
);
