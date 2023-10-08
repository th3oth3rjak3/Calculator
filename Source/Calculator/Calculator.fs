
namespace Calculator

module Calculator =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    
    open Avalonia.FuncUI
    open Avalonia.Controls.Primitives
    open Avalonia.Media
    
    open Calculator.Lib

    type State = { UserInput: string }
    let init = { UserInput = "" }

    type Msg = 
    | AddUserInput of string
    | Reset
    | Calculate

    let addInputToState state input =
        { state with UserInput = (state.UserInput + input) }

    let updateUserInput state input =
        match input with
        | "1" | "2" | "3" | "4" | "5" -> addInputToState state input
        | "6" | "7" | "8" | "9" | "0" -> addInputToState state input
        | "(" | ")" | "." -> addInputToState state input
        | "+" | "-" | "*" | "/" -> addInputToState state input
        | "C" -> init
        | "B" -> { state with UserInput = state.UserInput[..state.UserInput.Length - 2] }
        | "=" -> { state with UserInput = Application.tryCalculate state.UserInput }
        | _ -> state




    let update (msg: Msg) (state: State) : State =
        match msg with
        | AddUserInput input -> updateUserInput state input
        | Reset -> init
        | Calculate -> { state with UserInput = Application.tryCalculate state.UserInput }
    
    let view (state: State) (dispatch: Msg -> unit) =
        StackPanel.create [
            StackPanel.children [
                Border.create [
                    Border.dock Dock.Top
                    Border.cornerRadius 5.0
                    Border.borderThickness 1.0
                    Border.background "#003030"
                    Border.width 300
                    Border.height 55
                    Border.margin (40, 40, 40, 20)
                    Border.padding 5
                    Border.horizontalAlignment HorizontalAlignment.Center
                    Border.verticalAlignment VerticalAlignment.Center
                    Border.child (
                        TextBlock.create [
                            TextBlock.fontSize 38.0
                            TextBlock.height 45
                            TextBlock.placement PlacementMode.Bottom
                            TextBlock.width 290
                            TextBlock.textAlignment TextAlignment.Left
                            TextBlock.background "#014a4a"
                            TextBlock.foreground "white"
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.text (" " + state.UserInput)
                        ]
                    )
                ]
                UniformGrid.create [
                    UniformGrid.columns 4
                    UniformGrid.rows 5
                    UniformGrid.margin (40, 10, 40, 20)
                    UniformGrid.height 400
                    UniformGrid.width 300
                    UniformGrid.horizontalAlignment HorizontalAlignment.Center
                    UniformGrid.verticalAlignment VerticalAlignment.Bottom
                    UniformGrid.children (
                        ["("; ")"; "C"; "B";
                            "7"; "8"; "9"; "/";
                            "4"; "5"; "6"; "*";
                            "1"; "2"; "3"; "-";
                            "."; "0"; "="; "+"]
                        |> List.map (fun buttonText -> 
                            Button.create [
                                Button.onClick (fun _ -> dispatch (AddUserInput buttonText))
                                Button.background "#014a4a"
                                Button.foreground "white"
                                Button.content buttonText
                                Button.fontSize 30
                                Button.verticalContentAlignment VerticalAlignment.Center
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.margin 2.
                                Button.width 70.
                                Button.height 72. 
                            ] |> generalize )
                    )
                ]
            ]
        ]
        