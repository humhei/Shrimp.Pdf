﻿module ParsingTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Image
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open iText.Layout
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.icms2
open Shrimp.FSharp.Plus
open Shrimp.Pdf.RegisterableFonts
open Shrimp.Pdf.RegisterableFonts.YaHei
open FontNames.Query
open Imposing
open Shrimp.Pdf.Parser
open Fake.IO
open iText.IO.Font.Constants
open iText.IO.Font
open iText.Kernel.Font
open Shrimp.Pdf.Image
open iText.Kernel.Pdf


type BookTagColors =
    static member PageNumber = 
        FsSeparation.Create(
            "PANTONE DS 130-1 U",
            FsValueColor.Cmyk(FsDeviceCmyk.Create(0.f, 1.f, 0.35f, 0.20f))
        )
        
    static member BorderDash =
        FsSeparation.Create(
            "PANTONE DS 130-8 C",
            FsValueColor.Cmyk(FsDeviceCmyk.Create(0.f, 0.20f, 0.05f, 0.10f))
        )

    static member 帖标文本 =
        FsSeparation.Create(
            "帖标文本",
            FsValueColor.Cmyk(FsDeviceCmyk.Create(0.f, 0.758f, 0.668f, 0.f))
        )


    static member 虚线裁切框 =
        FsSeparation.Create(
            "虚线裁切框",
            FsValueColor.CreateRGB(0.264f, 0.615f, 0.449f)
        )

    static member 帖标 =
        FsSeparation.Create(
            "帖标",
            FsValueColor.Cmyk(FsDeviceCmyk.Create(0.f, 0.436f, 0.616f, 0.f))
        )

    static member 内页裁切标记 =
        FsSeparation.Create(
            "PANTONE 10219 C",
            FsValueColor.Lab(FsLab.Create(29.f, 34.f, -43.f))
        )

    static member 贴标文本颜色 = FsGray.WHITE
        
    static member AllBookTagColors =
        [ 
            FsColor.Separation BookTagColors.BorderDash
            FsColor.Separation BookTagColors.PageNumber
            FsColor.Separation BookTagColors.内页裁切标记
            FsColor.Separation BookTagColors.帖标
            FsColor.Separation BookTagColors.帖标文本
            FsColor.Separation BookTagColors.虚线裁切框
            FsColor.valueColor BookTagColors.贴标文本颜色
        ]



let parsingTests =
  testList "Parsing Tests" [
    testCase "extract concated text" <| fun _ -> 
        let path = Path.getFullName "datas/parsing/extract concated text.pdf"
        let texts = PdfRunner.ReadTextInfos(PdfFile path)
        match texts.[0].[0].Text() with 
        | "EXPEDITEUR:" -> pass()
        | _ -> fail()

    ftestCase "parsing empty pages" <| fun _ -> 
        let path = 
            Path.getFullName "datas/parsing/parsing empty pages.pdf" 

        let emptablePageNumbers =
            PdfRunner.ReadEmptyPages(PdfFile path, ignoreColors = BookTagColors.AllBookTagColors, inShadowMode = false)
        
        pass()


    testCase "extract dataTable in B255" <| fun _ -> 

        let flow =
            ModifyPage.ReadDataTable(
                PageSelector.First, { ColNum = 2 }, boundSelector = Info.StrokeColorIs FsColor.RGB_BLUE
            )

        Flow.Manipulate flow
        |> runTest "datas/parsing/extract dataTable in B255.pdf" 
        |> ignore

    testCase "extract dataTable in B255_2" <| fun _ -> 
        let path = Path.getFullName "datas/parsing/extract dataTable in B255_2.pdf" 
        let backUp = Path.changeExtension ".tests.pdf" path

        let flow =
            ModifyPage.ReadDataTable(
                PageSelector.First, { ColNum = 2 }, boundSelector = Info.StrokeColorIs FsColor.RGB_BLUE
            )
            |> Flow.Manipulate

        let texts = PdfRunner.OneFileFlow_UserState(PdfFile path, backUp) flow

        pass()


  ]