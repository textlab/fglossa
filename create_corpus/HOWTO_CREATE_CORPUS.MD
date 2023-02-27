In order to add a new corpus, start by adding a new directory in `src/Corpora/corpora` with the same name as the
CWB registry ID of the corpus (for corpora using CWB as their search engine, that is).

In that directory, create a tab-separated file of metadata where the first line provides codes for metadata categories (e.g. `age`, `birth_year` etc).
Each metadata code may be optionally followed by '$num' to indicate a numerical category rather than a string category (e.g. `birth_year$num`),
which will allow Glossa to display an interval selector as well as a list view for the category in the metadata menu.
Each following line provides metadata for a corpus text.

The first column should be called `tid` and contain a unique text ID (or speaker ID in the case
of spoken corpora, where each speaker is considered a "text"). For spoken corpora, the last column should be called `bounds` and contain all
the bounds of sections with material from the speaker, given as start and end corpus positions separated by a colon (e.g. `0-4:10-32:34-34`).
For written corpora, the last two columns should be called `startpos` and `endpos` and contain the start and end positions of the text.

After adding the directory and metadata file for a new corpus called, say, `my_corpus` (with the metadata file
`my_corpus_texts.tsv`), run the following from the current directory:

```bash
./create_corpus.rb my_corpus "My magnificent corpus"
```

(If you name the metadata file something different than `my_corpus_texts.tsv`, provide the file name as an
additional argument to `create_corpus.rb`.)

This will create a number of files as well as the corpus-specific SQLite database and import the metadata
from the TSV file into the database. The sqlite3 client will be opened at the end, but it can simply be
closed by typing `.quit` or pressing ctrl-d.

Finally recompile the client and server projects to include the new F# types and corpus-specific code.
Note that it may be necessary to run `dotnet fable clean` to make all of the new client code
be picked up by Fable in development mode. In other words (compiling in development mode):

```bash
dotnet fable clean
dotnet run
```

In addition, for corpora using CQP as their search engine, a CQP corpus with the same ID as the new corpus
directory must be added to the CQP registry. Additional configuration of the corpus can be done in subdirectories
called `Client`, `Server` and `Shared` within the corpus directory (see example corpora).
