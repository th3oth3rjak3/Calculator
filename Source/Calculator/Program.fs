namespace Calculator

open Elmish
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls


type MainWindow() as this =
    inherit HostWindow()
    do
        base.Icon <- WindowIcon("Assets\Icons\Calculator.ico")
        base.Height <- 565.0
        base.MinHeight <- 565
        base.MaxHeight <- 565
        base.Width <- 400.0
        base.MinWidth <- 400.0
        base.MaxWidth <- 400.0
        base.Title <- "Calculator"     

        Elmish.Program.mkSimple (fun () -> Calculator.init) Calculator.update Calculator.view
        |> Program.withHost this
        |> Program.run

        
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme(baseUri = null, Mode = FluentThemeMode.Dark))

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)