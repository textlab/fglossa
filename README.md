# Development

The application was created using the dotnet [SAFE Template](https://safe-stack.github.io/docs/template-overview/). To learn more about the template, see the [quick start](https://safe-stack.github.io/docs/quickstart/) guide.

## Install pre-requisites

You'll need to install the following pre-requisites in order to build SAFE applications

* [.NET SDK](https://www.microsoft.com/net/download) 6.0
* [Node LTS](https://nodejs.org/en/download/)

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

## SAFE Stack Documentation

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)