module ManipulateTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Fake.IO
open System.IO
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Font
open iText.IO.Font
open Shrimp.Pdf.Manipulates
open Shrimp.Pdf.DSL


let manipulateTests =
  testList "Manipulates Tests" [
    testCase "change stroke color b255 to m100" <| fun _ -> 
        Flow.Manipulate (
            modify(
                "change stroke color b255 to m100",
                PageSelector.First,
                [
                    { Selector = Path(Info.StrokeColoris DeviceRgb.BLUE)
                      Modifier = Fix [Modify.SetStrokeColor(DeviceCmyk.MAGENTA)]
                    }
                ]
            ) 
        )
        |> runWithBackup "datas/manipulate/change stroke color b255 to m100.pdf" 
        |> ignore

    testCase "xobject_change stroke color b255 to m100" <| fun _ -> 
        Flow.Manipulate (
            modify (
                "xobject_change stroke color b255 to m100",
                PageSelector.First,
                [
                    { Selector = Path(Info.StrokeColoris DeviceRgb.BLUE)
                      Modifier = Fix [Modify.SetStrokeColor(DeviceCmyk.MAGENTA)] }
                ]
            )
        )
        |> runWithBackup "datas/manipulate/xobject_change stroke color b255 to m100.pdf" 
        |> ignore

    testCase "add bound to text" <| fun _ -> 
        Flow.Manipulate (
            modify(
                "add bound to text",
                PageSelector.All,
                [
                    { Selector = Text(fun _ _ -> true) 
                      Modifier = AddNew[
                        Modify.AddRectangleToBound(fun args -> 
                            { args with StrokeColor = PdfCanvasColor.Specific DeviceCmyk.MAGENTA}
                        )
                      ]
                    }
                ]
            )
        )
        |> runWithBackup "datas/manipulate/add bound to text.pdf" 
        |> ignore

    testCase "add text to position" <| fun _ -> 
        Flow.Manipulate (
            addNew
                ("add text to position",
                  PageSelector.All,
                  PageBoxKind.ActualBox,
                  Operator.AddText("你好", fun args ->
                    { args with 
                        PdfFontFactory = PdfFontFactory.Registerable (RegisterableFonts.AlibabaPuHuiTiBold)
                        CanvasFontSize = CanvasFontSize.Numeric 25. 
                        FontColor = DeviceCmyk.MAGENTA 
                        FontRotation = Rotation.None 
                        Position = Position.LeftTop(0., 0.)}
                  )
                )
        )
        |> runWithBackup "datas/manipulate/add text to position.pdf" 
        |> ignore
    
    testCase "add page-scaling text" <| fun _ -> 
        Flow.Manipulate(
            addNew(
                "add page-scaling text",
                PageSelector.All,
                PageBoxKind.ActualBox,
                Operator.AddText("你好", fun args ->
                { args with 
                    PdfFontFactory = PdfFontFactory.Registerable (RegisterableFonts.AlibabaPuHuiTiBold)
                    CanvasFontSize = CanvasFontSize.OfRootArea 0.8 
                    FontColor = DeviceCmyk.MAGENTA 
                    FontRotation = Rotation.None 
                    Position = Position.Center(0., 0.)}
                ))
        )
        |> runWithBackup "datas/manipulate/add page-scaling text.pdf" 
        |> ignore

    testCase "trim to visible test" <| fun _ -> 
        Flow.Manipulate(
            trimToVisible (Margin.Create(mm 6)) PageSelector.All
        )
        |> runWithBackup "datas/manipulate/trim to visible.pdf" 
        |> ignore

  ]