namespace Shrimp.Pdflect

open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Filters
open Shrimp.Pdf.Extensions
open Types

[<RequireQualifiedAccess>]
module FsSizeOfMM =
    open Shrimp.Utils
    open Shrimp.Pdf.Types

    let sizeLitterBigger (width: float<mm>) (height: float<mm>) (size: FsSizeOfMM) =
        size.Width >++ width
        && size.Height >++ height
    
    let toFsSizeMapping (func: FsSizeOfMM -> 'result) =
        fun size ->
            FsSizeOfMM.ofFsSize size
            |> func

[<RequireQualifiedAccess>]
module Select =
    let fillColors xs (info: AbstractRenderInfo) =
        xs |> List.exists (Color.equal (info.GetFillColor()))  && AbstractRenderInfo.hasSolidFill info

    let strokeColors xs (info: AbstractRenderInfo) =
        xs |> List.exists ( fun x ->
            Color.equal (info.GetStrokeColor()) x 
        ) && AbstractRenderInfo.hasSolidStroke info

    let colors xs = fillColors xs <|> strokeColors xs

    let inRegion pageBox (info: AbstractRenderInfo) =
        let rect = AbstractRenderInfo.getBound info
        Rectangle.inbox rect pageBox

    let crossOrInRegion pageBox (info: AbstractRenderInfo) =
        let rect = AbstractRenderInfo.getBound info
        Rectangle.crossBox rect pageBox
        || 
            match info with 
            | :? PathRenderInfo as prInfo -> 
                PathRenderInfo.hasSolidFill prInfo && Rectangle.inbox pageBox rect
            | _ -> false


    let visible (info: AbstractRenderInfo) =
        AbstractRenderInfo.isVisible info

    let all (info: AbstractRenderInfo) =
        visible info

    let path (info: AbstractRenderInfo) =
        AbstractRenderInfo.isPathRenderInfo info && visible info

    let text (info: AbstractRenderInfo) =
        AbstractRenderInfo.isTextRenderInfo info && visible info

[<RequireQualifiedAccess>]
module Path =
    open Shrimp.Pdf.Types
    open Shrimp.Utils

    let stroke colors (info: PathRenderInfo) =
        PathRenderInfo.hasSolidStroke info 
        && colors |> List.exists (Color.equal (info.GetStrokeColor()))
    
    let fill colors (info: PathRenderInfo) =
        PathRenderInfo.hasSolidFill info 
        && colors |> List.exists (Color.equal (info.GetFillColor()))

    let size (predicate: FsSize -> bool) (info: PathRenderInfo) =
        PathRenderInfo.getBound info
        |> FsSize.ofRect
        |> predicate

    let colors inputColors = fill inputColors <|> stroke inputColors

    let dash (info: PathRenderInfo) =
        let dashArray,_ = info.GetGraphicsState() |> CanvasGraphicsState.getDashPattern
        match dashArray with 
        | [] -> false
        | _ -> true

[<RequireQualifiedAccess>]
module Text =
    open System.Text.RegularExpressions
    open iText.Kernel.Colors
    open FParsec

    let filter f (info: TextRenderInfo) =
        info.GetText() |> f

    let fparsec parsers (info: TextRenderInfo) =
        let text = info.GetText()
        match run parsers text with 
        | ParserResult.Success _ -> true
        | _ -> false

    let regex pattern (info: TextRenderInfo) =
        let r = Regex.Match(info.GetText(), pattern)
        r.Success

    let prediate (f: string -> bool) (info: TextRenderInfo) =
        info.GetText() |> f
        
    let fill colors (info: TextRenderInfo) =
        TextRenderInfo.hasSolidFill info 
        && colors |> List.exists (Color.equal (info.GetFillColor()))

    let stroke (colors: Color list) (info: TextRenderInfo) =
        TextRenderInfo.hasSolidStroke info 
        && colors |> List.exists (Color.equal (info.GetStrokeColor()))

[<RequireQualifiedAccess>]
module AnalyticSubpath =
    open Shrimp.Utils
    open Shrimp.Pdf.Types

    let sizeLitterBigger (width: float<mm>) (height: float<mm>) (analyticSubpath: AnalyticSubpath) =
        let bound = AnalyticSubpath.getBound analyticSubpath
        let unitedSize = FsSizeOfMM.ofRect bound
        FsSizeOfMM.sizeLitterBigger width height unitedSize

