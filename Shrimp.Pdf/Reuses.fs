﻿namespace Shrimp.Pdf
open Imposing
open System
open Fake.IO
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf
open System.IO

type PageNumSequence = private PageNumSequence of int list
with 
    member x.Value = 
        let (PageNumSequence value) = x
        value


[<RequireQualifiedAccess>]
module PageNumSequence =
    let create (sequence: int list) =
        if sequence.IsEmpty then failwith "page sequence cannot be empty"
        elif List.exists (fun pageNumber -> pageNumber <= 0) sequence then failwithf "number in page sequence %A must be bigger than 0" sequence
        PageNumSequence sequence



module Reuses =
    let impose (fArgs) =
        fun flowModel (splitDocument: SplitDocument) ->
            let imposingArguments = ImposingArguments.Create fArgs
            Logger.info (sprintf "IMPOSE %A" imposingArguments)
            let imposingDocument = new ImposingDocument(splitDocument, imposingArguments)
            imposingDocument.Build()

            imposingDocument.Draw()
            (imposingArguments, imposingDocument)
        |> Reuse


    /// e.g. [1; 3; 5] will pick page1, page3, page5
    let sequencePages (pageNumSequence: PageNumSequence) =
        fun flowModel (splitDocument: SplitDocument) ->
            Logger.info (sprintf "SEQUENCEPAGES %A" pageNumSequence)

            let duplicatedReaderPages = 
                splitDocument.Reader
                |> PdfDocument.getPages
                |> List.mapi (fun i page ->
                    let pageNum = i + 1
                    pageNumSequence.Value
                    |> List.filter ((=) pageNum)
                    |> List.map (fun _ -> page)
                )|> List.concat

            for page in duplicatedReaderPages do
                let writerPageResource = page.CopyTo(splitDocument.Writer)
                splitDocument.Writer.AddPage(writerPageResource) |> ignore
            
            flowModel.UserState
        |> Reuse

    let duplicatePages (pageSelector: PageSelector) (copiedNumber: int)  =
        if copiedNumber <= 0 then failwithf "copied number %d should be bigger than 0" copiedNumber

        fun flowModel (splitDocument: SplitDocument) ->
            Logger.info (sprintf "DUPLICATEPAGES %d" copiedNumber)

            let pageNumSequence = 
                splitDocument.Reader.GetPageNumbers(pageSelector)
                |> List.collect (fun pageNum ->
                    List.replicate copiedNumber pageNum
                )
                |> PageNumSequence.create

            (sequencePages pageNumSequence).Value flowModel splitDocument
        |> Reuse

    let tilePages (tileTable: TileTable) =

        let colNum = tileTable.ColNum
        let rowNum = tileTable.RowNum

        fun flowModel (splitDocument: SplitDocument) ->
            Logger.info (sprintf "TILEPAGES %d*%d" tileTable.ColNum tileTable.RowNum)
            let userState = (duplicatePages (PageSelector.All) (colNum * rowNum)).Value flowModel splitDocument
            let tileTable = TileTable.create colNum rowNum
            let writer = splitDocument.Writer
            for i = 1 to writer.GetNumberOfPages() do
                let page = writer.GetPage(i)
                let actualBox = page.GetActualBox()
                let tileIndexer = TileIndexer.create tileTable (i - 1)
                let tileBox = Rectangle.getTile tileIndexer actualBox
                PdfPage.setPageBox PageBoxKind.AllBox tileBox page
                |> ignore
            
            userState
        |> Reuse





