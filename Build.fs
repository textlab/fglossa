open Fake.Core
open Fake.DotNet
open Fake.IO

let execContext =
    Context.FakeExecutionContext.Create false "build.fsx" []

Context.setExecutionContext (Context.RuntimeContext.Fake execContext)

let sharedPath =
    Path.getFullName "src/Shared"

let serverPath =
    Path.getFullName "src/Server"

let clientPath =
    Path.getFullName "src/Client"

let deployDir = Path.getFullName "deploy"
// let sharedTestsPath = Path.getFullName "tests/Shared"
// let serverTestsPath = Path.getFullName "tests/Server"

let npm args workingDir =
    let npmPath =
        match ProcessUtils.tryFindFileOnPath "npm" with
        | Some path -> path
        | None ->
            "npm was not found in path. Please install it and make sure it's available from your path. "
            + "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
            |> failwith

    let arguments =
        args |> String.split ' ' |> Arguments.OfArgs

    Command.RawCommand(npmPath, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let dotnet cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""

    if result.ExitCode <> 0 then
        failwith $"'dotnet %s{cmd}' failed in %s{workingDir}"


Target.create "Clean" (fun _ -> Shell.cleanDir deployDir)

Target.create "InstallClient" (fun _ -> npm "install" ".")

Target.create "BuildClient" (fun _ -> dotnet "fable --outDir build --run webpack build" clientPath)

Target.create "Bundle" (fun _ ->
    dotnet $"publish -c Release -o \"%s{deployDir}\"" serverPath
    dotnet "fable --outDir build --run webpack build" clientPath)

Target.create "Run" (fun _ ->
    dotnet "build" sharedPath

    [ async { dotnet "watch run" serverPath }
      async { dotnet "fable watch --outDir build -s --run webpack-dev-server" clientPath } ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore)

// Target.create "RunTests" (fun _ ->
//     dotnet "build" sharedTestsPath
//     [ async { dotnet "watch run" serverTestsPath }
//       async { dotnet "fable watch --run webpack-dev-server --config ../../webpack.tests.config.js" "tests/Client" } ]
//     |> Async.Parallel
//     |> Async.RunSynchronously
//     |> ignore
// )

open Fake.Core.TargetOperators

let dependencies =
    [ "Clean" ==> "InstallClient" ==> "BuildClient"

      "Clean" ==> "InstallClient" ==> "Run"

      //"Clean"
      //    ==> "InstallClient"
      //    ==> "RunTests"
      ]

[<EntryPoint>]
let main args =
    try
        match args with
        | [| target |] -> Target.runOrDefault target
        | _ -> Target.runOrDefault "Run"

        0
    with
    | e ->
        printfn $"%A{e}"
        1
