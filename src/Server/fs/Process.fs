module Process

let runCmd (cmd: string) (args: string) =
    use proc =
        System.Diagnostics.Process.Start(cmd, args)

    proc.WaitForExit()

let runCmdWithOutput (cmd: string) (args: string) =
    let startInfo =
        System.Diagnostics.ProcessStartInfo(cmd, args)

    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardOutput <- true

    use proc =
        System.Diagnostics.Process.Start(startInfo)

    let output = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    output

let runCmdWithInputAndOutput (cmd: string) (args: string) (input: string) =
    let startInfo =
        System.Diagnostics.ProcessStartInfo(cmd, args)

    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardInput <- true

    use proc =
        System.Diagnostics.Process.Start(startInfo)

    use writer = proc.StandardInput
    writer.Write(input)
    writer.Close()

    let output = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    output

let runCmdWithInputOutputErrorAndEncoding
    (cmd: string)
    (args: string)
    (encoding: System.Text.Encoding)
    (input: string)
    =
    let startInfo =
        System.Diagnostics.ProcessStartInfo(cmd, args)

    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardInput <- true
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.StandardOutputEncoding <- encoding

    use proc =
        System.Diagnostics.Process.Start(startInfo)

    use writer = proc.StandardInput
    writer.Write(input)
    writer.Close()

    let output = proc.StandardOutput.ReadToEnd()
    let error = proc.StandardError.ReadToEnd()
    proc.WaitForExit()
    (output, error)

let runCmdWithInputOutputAndError (cmd: string) (args: string) (input: string) =
    runCmdWithInputOutputErrorAndEncoding cmd args System.Text.Encoding.UTF8 input
