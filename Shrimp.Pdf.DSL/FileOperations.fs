namespace Shrimp.Pdf
#nowarn "0104"
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
    ///TargetDocumentPath = Path.GetTempFileNameEx() |> Path.changeExtension ".pdf"
    ///
    ///Override = false 
    static member DefalutValue =
        { TargetDocumentPath = Path.GetTempFileNameEx() |> Path.changeExtension ".pdf"
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
    let internal mergeDocumentsInternal config (targetDocumentName: string) (writer: PdfDocument)  =
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
              Configuration = config
            }
            |> List.singleton


        |> FileOperation


    let mergeDocuments (f) =
        let args = f DocumentMergingArguments.DefalutValue

        fun (flowModels: FlowModel<_> list) ->
            if flowModels.Length < 2 
            then 
                let files = 
                    flowModels
                    |> List.map(fun m -> m.File)

                failwithf "Cannot mergeDocuments when input document %A count %A < 2" files flowModels.Length

            let config = flowModels.Head.Configuration
            Directory.ensure (Path.getDirectory args.TargetDocumentPath)
            if File.exists args.TargetDocumentPath && not args.Override then failwithf "target file %s already exists" args.TargetDocumentPath
            else File.delete args.TargetDocumentPath 

            let writer = new PdfDocument(new PdfWriter(args.TargetDocumentPath))
            let flow = mergeDocumentsInternal config (args.TargetDocumentPath) writer

            let result = flow.Value flowModels

            writer.Close()
            result

        |> FileOperation

    /// flowModels.Length < 2  -> Keep
    ///
    /// flowModels.Length >= 2 -> Merge
    let mergeDocuments_or_keep (f) =

        fun (flowModels: FlowModel<_> list) ->
            if flowModels.Length < 2 
            then 
                flowModels
                |> List.map(fun flowModel -> flowModel.MapUserState List.singleton)
            else
                (mergeDocuments f).Value flowModels

        |> FileOperation


    let splitDocumentBySequences(fSequenceTargets: TotalNumberOfPages -> DocumentSplitSequenceTarget list, isOverride) =
        fun (flowModels: FlowModel<'userState> list)  ->
            flowModels
            |> List.collect (fun flowModel ->
                let readerDocument = new ReaderDocument(flowModel.File)
                let reader = readerDocument.Reader

                let readerPages = 
                    reader
                    |> PdfDocument.getPages

                let sequenceTargets = 
                    (readerPages.Length)
                    |> TotalNumberOfPages
                    |> fSequenceTargets

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
                          UserState = i, flowModel.UserState
                          Configuration = flowModel.Configuration }
                    )

                let newFiles = 
                    newModels
                    |> List.map (fun m -> 
                        m.File
                    )
                    |> String.concat "\n"

                match flowModel.Configuration.LoggerLevel with 
                | PdfLoggerLevel.Info -> PdfLogger.info (sprintf "SPLIT document %s to %s" flowModel.File newFiles) 
                | PdfLoggerLevel.Slient -> ()

                reader.Close()
                newModels
        )
        |> FileOperation

    let splitDocumentByPageCounts(fPageCountsTarget: TotalNumberOfPages -> DocumentSplitPageCountTarget list, isOverride) =
        let sequenceTargets totalPageCount =
            (0, fPageCountsTarget totalPageCount)
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
                          UserState = flowModel.UserState
                          Configuration = flowModel.Configuration }
                    )
                let newFiles = 
                    newModels
                    |> List.map (fun m -> 
                        m.File
                    )
                    |> String.concat "\n"
                match flowModel.Configuration.LoggerLevel with 
                | PdfLoggerLevel.Info -> PdfLogger.info (sprintf "SPLIT document %s to %s" flowModel.File newFiles) 
                | PdfLoggerLevel.Slient -> ()
                reader.Close()
                newModels
        )
        |> FileOperation




type PdfRunner =
    static member MergeDocuments (inputs: PdfFile AtLeastTwoList, ?fArgs, ?config) =
        
        let flow =
            FileOperations.mergeDocuments (defaultArg fArgs id)
            |> Flow.FileOperation

        let config = defaultArg config Configuration.DefaultValue

        let flowModels = 
            inputs.AsList
            |> List.map (fun m -> {PdfFile = m; UserState = (); Configuration = config })


        runManyWithFlowModels config flowModels flow
        |> List.exactlyOne
        |> fun m -> m.PdfFile

    /// Will copy to target if inputs'length is 1
    static member MergeDocuments (inputs: PdfFile al1List, ?fArgs, ?config) =
        match inputs with 
        | AtLeastOneList.Many inputs -> PdfRunner.MergeDocuments(inputs, ?fArgs = fArgs, ?config = config)
        | AtLeastOneList.One input ->
            match fArgs with 
            | Some fArgs -> 
                let args = fArgs DocumentMergingArguments.DefalutValue
                File.Copy(input.Path, args.TargetDocumentPath, args.Override)

                PdfFile args.TargetDocumentPath
            | None -> input

    static member mergeDocumentsTo targetPdfPath (inputs: PdfFile al1List) =
        PdfRunner.MergeDocuments(
            inputs,
            fun args -> { args with TargetDocumentPath = targetPdfPath; Override = true }
        )



    static member SplitDocumentToMany (inputPdfFile: PdfFile, ?fArgs, ?config) =
        
        let flow =
            FileOperations.splitDocumentToMany (defaultArg fArgs id)
            |> Flow.FileOperation

        let flowModel = 
            { PdfFile = inputPdfFile
              UserState = ()
              Configuration = defaultArg config Configuration.DefaultValue }

        runWithFlowModel flowModel flow
        |> List.map (fun m -> m.PdfFile)


    static member SplitDocumentBySequences (inputPdfFile: PdfFile, fSequenceTargets, ?isOverride, ?config) =
        
        let flow =
            FileOperations.splitDocumentBySequences(fSequenceTargets, defaultArg isOverride false)
            |> Flow.FileOperation

        let flowModel = 
            { PdfFile = inputPdfFile
              UserState = ()
              Configuration = defaultArg config Configuration.DefaultValue }

        runWithFlowModel flowModel flow
        |> List.map (fun m -> m.PdfFile)

    static member SplitDocumentByPageCounts (inputPdfFile: PdfFile, fPageCounts, ?isOverride, ?config) =
        
        let flow =
            FileOperations.splitDocumentByPageCounts(fPageCounts, defaultArg isOverride false)
            |> Flow.FileOperation

        let flowModel =  
            { PdfFile = inputPdfFile
              UserState = ()
              Configuration = defaultArg config Configuration.DefaultValue }

        runWithFlowModel flowModel flow
        |> List.map (fun m -> m.PdfFile)

