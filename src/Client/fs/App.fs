module App

open Elmish
open Elmish.React

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

// TODO: Replace the jQuery-based jPlayer and the accompanying TextBox defined in
// jplayer.js with something more suitable for React-based applications!
Fable.Core.JsInterop.importSideEffects ("../../../node_modules/jquery/dist/jquery.js")
Fable.Core.JsInterop.importSideEffects ("../jquery.jplayer.js")
Fable.Core.JsInterop.importSideEffects ("../jplayer.js")

Program.mkProgram Update.init Update.update View.Index.MainView
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
