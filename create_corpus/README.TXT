Start by adding a directory for a new corpus in src/Corpora/corpora which contains a tab-separated
file of metadata with codes for metadata categories on the first line. Each metadata code may be
optionally followed by '$num' to indicate a numerical category rather than a string category.

The first column should be called 'tid' and contain a unique text id (or speaker ID in the case
of spoken corpora). For spoken corpora, the last column should be called 'bounds' and contain all
the corpus bounds of the speaker. For written corpora, the last two columns should be
called 'startpos' and 'endpos' and contain the start and ending position of the text.

After adding such a directory and file for, say, the new corpus my_corpus (with the metadata file
my_corpus_texts.tsv), run the following from the present directory:

$ ./create_metadata_categories.rb my_corpus MyCorpus > ../src/Corpora/corpora/my_corpus/Shared/MetadataCategories.fs
$ ./create_db_definition.rb ../src/Corpora/corpora/my_corpus/my_corpus_texts.tsv > ../src/Corpora/corpora/my_corpus/create_db.sql
$ ./config_corpora.sh

Then create a corpus-specific database from the database definitian file. From the corpus directory:
$ sqlite3 my_corpus.sqlite --init ./create_db.sql

Then import the metadata into the database. To import a tab-separated file where the columns may contain double quotes, 
use ascii mode instead of tabs mode:

sqlite> .mode ascii
sqlite> .separator "\t" "\n"
sqlite> .import mycorpus_texts.tsv texts

Finally recompile the client and server projects to include the new types and corpus-specific code.
Note that it may be necessary to run 'dotnet fable clean' to make all of the new client code
be picked up by Fable in development mode.

