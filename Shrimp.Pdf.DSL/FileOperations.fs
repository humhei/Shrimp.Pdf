namespace Shrimp.Pdf
open Fake.IO
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf
open System.IO
open Fake.IO.FileSystemOperators
open Shrimp.FSharp.Plus

[<RequireQualifiedAccess>]
type DocumentSplitOutputDirectoryOptions =
    | ReaderDirectoryPath_Current
    | ReaderDirectoryPath_Next
    | CustomDirectoryPath of string




type DocumentSplitArguments =
    { ChunkSize: int 
      OutputDirectory: DocumentSplitOutputDirectoryOptions
      PartText: string
      CleanOutputFirst: bool
      Override: bool }
with 
    static member DefalutValue =
        { ChunkSize = 1 
          OutputDirectory = DocumentSplitOutputDirectoryOptions.ReaderDirectoryPath_Next
          Override = false
          PartText = "部分"
          CleanOutputFirst = false }
    
type DocumentMergingArguments =
    { TargetDocumentPath: string 
      Override: bool }
with 
    ///TargetDocumentPath = Path.GetTempFileName() |> Path.changeExtension ".pdf"
    ///
    ///Override = false 
    static member DefalutValue =
        { TargetDocumentPath = Path.GetTempFileName() |> Path.changeExtension ".pdf"
          Override = false }

type DocumentSplitSequenceTarget =
    { PageNumSequence: ``Int>=1`` list 
      PathPart: string option }
with 
    static member Create(pageNumberSequence: int list, ?pathPart) =
        { PageNumSequence = List.map ``Int>=1``.Create pageNumberSequence
          PathPart = pathPart }

type DocumentSplitPageCountTarget =
    { PageCount: ``Int>=1``
      PathPart: string option  }
with 
    static member Create(pageCount, ?pathPart) =
        { PageCount = ``Int>=1``.Create pageCount
          PathPart = pathPart }

[<RequireQualifiedAccess>]
module FileOperations =
    /// expose this function for modifyAsync
    let internal mergeDocumentsInternal (targetDocumentName: string) (writer: PdfDocument)  =
        fun (flowModels: FlowModel<'userState> list) ->
            for pageNum = 1 to writer.GetNumberOfPages() do
                writer.RemovePage(1)

            for flowModel in flowModels do
                let readerDocument = new ReaderDocument(flowModel.File)
                for pageNum = 1 to readerDocument.Reader.GetNumberOfPages() do
                    let page = readerDocument.Reader.GetPage(pageNum)
                    let writerPageResource = page.CopyTo(writer)
                    writer.AddPage(writerPageResource) |> ignore

                readerDocument.Reader.Close()

            { PdfFile = PdfFile targetDocumentName 
              UserState = flowModels |> List.map (fun m -> m.UserState)
            }
            |> List.singleton


        |> FileOperation


    let mergeDocuments (f) =
        let args = f DocumentMergingArguments.DefalutValue

        fun (flowModels: _ list) ->
            if flowModels.Length < 2 then failwithf "Cannot mergeDocuments when input page count %A < 2" flowModels.Length

            Directory.ensure (Path.getDirectory args.TargetDocumentPath)
            if File.exists args.TargetDocumentPath && not args.Override then failwithf "target file %s already exists" args.TargetDocumentPath
            else File.delete args.TargetDocumentPath 

            let writer = new PdfDocument(new PdfWriter(args.TargetDocumentPath))
            let flow = mergeDocumentsInternal (args.TargetDocumentPath) writer

            let result = flow.Value flowModels

            writer.Close()
            result

        |> FileOperation



    let splitDocumentBySequences(sequenceTargets: DocumentSplitSequenceTarget list, isOverride) =
        fun (flowModels: FlowModel<'userState> list)  ->
            flowModels
            |> List.collect (fun flowModel ->
                let readerDocument = new ReaderDocument(flowModel.File)
                let reader = readerDocument.Reader

                let readerPages = 
                    reader
                    |> PdfDocument.getPages

                let baseDir = (Path.getDirectory flowModel.File)

                let newModels = 
                    let fileFullPaths =
                        sequenceTargets
                        |> List.mapi(fun i sequenceTarget ->
                            let baseFileName = Path.GetFileNameWithoutExtension flowModel.File
                            let dir = baseDir </> baseFileName
                            match sequenceTarget.PathPart with 
                            | Some pathPart -> 
                                match pathPart.ToLower().EndsWith ".pdf" with 
                                | true -> dir </> pathPart
                                | false ->
                                    dir </> baseFileName + "_" + pathPart + (i+1).ToString() + ".pdf"

                            | None -> 
                                dir </> baseFileName + "_Part" + (i+1).ToString() + ".pdf"
                        ) 

                    fileFullPaths
                    |> List.map(Path.getDirectory >> FsFullPath)
                    |> List.distinct
                    |> List.iter (FsFullPath.path >> Directory.ensure)

                    sequenceTargets
                    |> List.mapi(fun i sequenceTarget ->
                        let fileFullPath = fileFullPaths.[i]
                            
                        match File.exists fileFullPath, isOverride with 
                        | true, false -> failwithf "File %s already exists" fileFullPath
                        | false, _ -> ()
                        | true, true -> File.delete fileFullPath

                        let writer = new PdfDocument(new PdfWriter(fileFullPath))

                        sequenceTarget.PageNumSequence
                        |> List.map(fun m -> readerPages.[m.Value-1])
                        |> List.iter(fun readerPage ->
                            let writerPageResource = readerPage.CopyTo(writer)
                            writer.AddPage(writerPageResource) |> ignore
                        )

                        writer.Close()

                        { PdfFile = PdfFile fileFullPath 
                          UserState = flowModel.UserState }
                    )

                let newFiles = 
                    newModels
                    |> List.map (fun m -> 
                        m.File
                    )
                    |> String.concat "\n"

                Logger.info (sprintf "SPLIT document %s to %s" flowModel.File newFiles) 
                reader.Close()
                newModels
        )
        |> FileOperation

    let splitDocumentByPageCounts(pageCountsTarget: DocumentSplitPageCountTarget list, isOverride) =
        let sequenceTargets =
            (0, pageCountsTarget)
            ||> List.mapFold(fun i target ->
                let pageNumberSequence = 
                    [0 .. target.PageCount.Value-1]
                    |> List.map(fun m -> m + 1 + i)
                    |> List.map ``Int>=1``.Create
                let r = 
                    { PageNumSequence = pageNumberSequence
                      PathPart = target.PathPart }

                r, i + target.PageCount.Value
            )
            |> fst

        splitDocumentBySequences(sequenceTargets, isOverride)

    let splitDocumentToMany (f: DocumentSplitArguments -> DocumentSplitArguments)  =
        let args = f DocumentSplitArguments.DefalutValue

        if args.ChunkSize <= 0 then failwithf "Page num %d per document must be bigger than 1" args.ChunkSize
        fun (flowModels: FlowModel<'userState> list) ->
            flowModels
            |> List.collect (fun flowModel ->
                let readerDocument = new ReaderDocument(flowModel.File)
                let reader = readerDocument.Reader

                let outputDirectory =
                    match args.OutputDirectory with 
                    | DocumentSplitOutputDirectoryOptions.ReaderDirectoryPath_Current ->
                        Path.getDirectory flowModel.File

                    | DocumentSplitOutputDirectoryOptions.ReaderDirectoryPath_Next -> 
                        Path.getDirectory flowModel.File </> Path.GetFileNameWithoutExtension flowModel.File

                    | DocumentSplitOutputDirectoryOptions.CustomDirectoryPath directory ->
                        Path.GetFullPath directory

                match args.CleanOutputFirst with 
                | true -> Shell.cleanDir outputDirectory
                | _ -> ()

                Directory.ensure outputDirectory

                let fileNameWithoutExtension = Path.GetFileNameWithoutExtension(flowModel.File)

                let newModels = 
                    reader
                    |> PdfDocument.getPages
                    |> List.chunkBySize(args.ChunkSize)
                    |> List.mapi(fun i pages ->
                        let number = i + 1
                        let fileName = 
                            match args.PartText.ToLower().EndsWith ".pdf" with 
                            | true -> args.PartText
                            | false ->
                                sprintf "%s_%s%d.pdf" fileNameWithoutExtension args.PartText number
                        let fileFullPath = Path.Combine(outputDirectory, fileName)
                        match File.exists fileFullPath, args.Override with 
                        | true, false -> failwithf "File %s already exists" fileFullPath
                        | false, _ -> ()
                        | true, true ->
                            File.delete fileFullPath

                        let writer = new PdfDocument(new PdfWriter(fileFullPath))
                        for page in pages do 
                            let writerPageResource = page.CopyTo(writer)
                            writer.AddPage(writerPageResource) |> ignore
                        writer.Close()

                        { PdfFile = PdfFile fileFullPath 
                          UserState = flowModel.UserState }
                    )
                let newFiles = 
                    newModels
                    |> List.map (fun m -> 
                        m.File
                    )
                    |> String.concat "\n"
                Logger.info (sprintf "SPLIT document %s to %s" flowModel.File newFiles) 
                reader.Close()
                newModels
        )
        |> FileOperation



type PdfRunner =
    static member MergeDocuments (inputs: PdfFile AtLeastTwoList, ?fArgs) =
        
        let flow =
            FileOperations.mergeDocuments (defaultArg fArgs id)
            |> Flow.FileOperation

        let flowModels = 
            inputs.AsList
            |> List.map (fun m -> {PdfFile = m; UserState = ()})


        runManyWithFlowModels flowModels flow
        |> List.exactlyOne
        |> fun m -> m.PdfFile

    static member MergeDocuments (inputs: PdfFile al1List, ?fArgs) =
        match inputs with 
        | AtLeastOneList.Many inputs -> PdfRunner.MergeDocuments(inputs, ?fArgs = fArgs)
        | AtLeastOneList.One input ->
            let args = 
                match fArgs with 
                | Some fArgs -> fArgs DocumentMergingArguments.DefalutValue
                | None -> DocumentMergingArguments.DefalutValue



            File.Copy(input.Path, args.TargetDocumentPath, args.Override)

            PdfFile args.TargetDocumentPath

    static member SplitDocumentToMany (inputPdfFile: PdfFile, ?fArgs) =
        
        let flow =
            FileOperations.splitDocumentToMany (defaultArg fArgs id)
            |> Flow.FileOperation

        let flowModel =  {PdfFile = inputPdfFile; UserState = () }

        runWithFlowModel flowModel flow
        |> List.map (fun m -> m.PdfFile)


    static member SplitDocumentBySequences (inputPdfFile: PdfFile, sequenceTargets, ?isOverride) =
        
        let flow =
            FileOperations.splitDocumentBySequences(sequenceTargets, defaultArg isOverride false)
            |> Flow.FileOperation

        let flowModel =  {PdfFile = inputPdfFile; UserState = () }

        runWithFlowModel flowModel flow
        |> List.map (fun m -> m.PdfFile)

    static member SplitDocumentByPageCounts (inputPdfFile: PdfFile, pageCounts, ?isOverride) =
        
        let flow =
            FileOperations.splitDocumentByPageCounts(pageCounts, defaultArg isOverride false)
            |> Flow.FileOperation

        let flowModel =  {PdfFile = inputPdfFile; UserState = () }

        runWithFlowModel flowModel flow
        |> List.map (fun m -> m.PdfFile)

