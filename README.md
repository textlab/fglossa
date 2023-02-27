The application was created using the dotnet [SAFE Template](https://safe-stack.github.io/docs/template-overview/). To learn more about the template, see the [quick start](https://safe-stack.github.io/docs/quickstart/) guide.

## Install pre-requisites

On your development machine, you'll need to install the following pre-requisites in order to import your corpora into Glossa and build the application:

* [.NET SDK 6.0](https://www.microsoft.com/net/download)
* [Node LTS](https://nodejs.org/en/download/)
* [SQLite 3](https://www.sqlite.org/index.html)
* [Ruby](https://www.ruby-lang.org/en/)

None of these are required on your production server if you build a self-contained executable on your development machine (see `create_executable.sh` for an example).

If you would rather create a smaller, framework-dependent application (using `dotnet run -- Bundle`), you will need the .NET SDK on your server as well (or alternatively run .NET in a [Docker container](https://hub.docker.com/_/microsoft-dotnet)).

## Add corpus definitions

After checking out this repository, you will need to create the directory that will contain your corpus definitions:

```bash
mkdir -p src/Corpora/corpora
./create_db.sh
```
Simply exit from the sqlite command line using `.quit` or ctrl-d. Then `cd` into the `create_corpus` directory and follow the instructions in the `README.MD` to create your first corpus in Glossa.

Note that the `src/Corpora` directory and its contents are included in `.gitignore` to prevent your corpus-specific code from being included in the main repo.
You may want to initialize a separate git repo in `src/Corpora` to keep your corpus definitions under version control.

## Starting the application

Before you run the project **for the first time only** you must install dotnet "local tools" with this command:

```bash
dotnet tool restore
```

To concurrently run the server and the client components in watch mode use the following command:

```bash
dotnet run
```

Then open `http://localhost:8080` in your browser.

To run the project in production:

```bash
dotnet run -- Bundle
cd deploy
dotnet ./Server.dll
```

Alternatively, you could create a self-contained executable, which can then be simply copied to the server and run like any other executable without any pre-requisites.
(see `create_executable.sh` for an example of how to do this). Note that the target platform does not have to be the same as the develoopment platform,
so you could for instance use a Mac to create an executable for 64-bit Linux.

## SAFE Stack Documentation

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)