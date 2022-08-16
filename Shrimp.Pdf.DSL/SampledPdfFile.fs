namespace Shrimp.Pdf.SampledPdfFile
#nowarn "0104"
open Shrimp.Pdf.Parser
open Shrimp.Pdf
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf
open System.IO
open Fake.IO
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open Imposing

[<AutoOpen>]
module _SampledPdfFile =
    open iText.Kernel.Pdf.Canvas.Parser

    type EntityOrSample =
        | Entity = 0
        | Sample = 1

    [<AutoOpen>]
    module _EntityOrSampleExtensions =
        type EntityOrSample with
            member internal x.DotnetPath =
                match x with 
                | EntityOrSample.Entity -> ".entity"
                | EntityOrSample.Sample -> ".sample"

            member x.RedirectPdfPath (pdfPath: PdfPath) = 
                let dir = 
                    let dir = Path.getDirectory pdfPath.Path 
                    match dir with 
                    | String.EndsWith ".entity" 
                    | String.EndsWith ".sample" ->
                       Path.getDirectory dir </> x.DotnetPath

                    | _ -> dir </> x.DotnetPath

                Directory.ensure dir

                dir </> Path.GetFileName pdfPath.Path
                |> PdfPath

    type SampledPdfFileUserState<'UserState> =
        { EntityPdfFileUserState: Lazy<'UserState>
          SamplePdfFileUserState: 'UserState }

    type SampledPdfFileWithUserState<'UserState> =
        { EntityPdfFileWithUserState: Lazy<PdfFile * 'UserState>
          SamplePdfFileWithUserState: PdfFile * 'UserState
          UseSampleFile: bool
          SampleFileConfiguration: Configuration }
    with
        member x.UserState =
            { EntityPdfFileUserState = lazy snd x.EntityPdfFileWithUserState.Value
              SamplePdfFileUserState = snd x.SamplePdfFileWithUserState }

        member private x.SamplePdfFile = fst x.SamplePdfFileWithUserState

        member private x.BaseDir = 
            x.SamplePdfFile.Path
            |> Path.getDirectory 
            |> Path.getDirectory 

        member x.EntityPdfFileDirectory = x.BaseDir </> EntityOrSample.Entity.DotnetPath

        member x.SamplePdfFileDirectory = x.BaseDir </> EntityOrSample.Sample.DotnetPath

        member x.Bind(fTargetPdfPath: _ -> PdfFile * 'userState) =
            let entityPdfFile =
                lazy 
                    fTargetPdfPath {| EntityOrSample = EntityOrSample.Entity; File = x.EntityPdfFileWithUserState.Value |}

            let targetSamplePdfFile = 
                fTargetPdfPath 
                    {| EntityOrSample = EntityOrSample.Sample; File = x.SamplePdfFileWithUserState |}
                    

            { EntityPdfFileWithUserState = entityPdfFile 
              SamplePdfFileWithUserState = targetSamplePdfFile
              UseSampleFile = x.UseSampleFile
              SampleFileConfiguration = x.SampleFileConfiguration }

    type SampledPdfFile =
        { EntityPdfFile: Lazy<PdfFile>
          SamplePdfFile: PdfFile
          UseSampleFile: bool
          SampleFileConfiguration: Configuration }
    with 
        member internal x.WithUserState() =
            { EntityPdfFileWithUserState =
                lazy (x.EntityPdfFile.Value, ())
              SamplePdfFileWithUserState = x.SamplePdfFile, ()
              UseSampleFile = x.UseSampleFile 
              SampleFileConfiguration = x.SampleFileConfiguration }

        member x.BaseDir = 
            x.SamplePdfFile.Path
            |> Path.getDirectory 
            |> Path.getDirectory 

        member x.Configuration(entityOrSample) =
            match x.UseSampleFile, entityOrSample with 
            | true, EntityOrSample.Sample -> x.SampleFileConfiguration
            | _ -> Configuration.DefaultValue

        member x.EntityPdfFileDirectory = x.BaseDir </> EntityOrSample.Entity.DotnetPath
        member x.SamplePdfFileDirectory = x.BaseDir </> EntityOrSample.Sample.DotnetPath

        member x.EntityPdfFilePath = 
            let fileName = Path.GetFileName x.SamplePdfFile.Path
            x.EntityPdfFileDirectory </> fileName
            |> PdfPath

        member x.EntityPdfFileName = Path.GetFileName x.EntityPdfFilePath.Path

        member x.Bind(fTargetPdfPath:_ -> PdfFile) =
            let targetSamplePdfFile = 
                fTargetPdfPath {| EntityOrSample = EntityOrSample.Sample; File = x.SamplePdfFile |}
                

            let entityPdfFile =
                lazy 
                    fTargetPdfPath {| EntityOrSample = EntityOrSample.Entity; File = x.EntityPdfFile.Value |}
                        

            { EntityPdfFile = entityPdfFile 
              SamplePdfFile = targetSamplePdfFile
              UseSampleFile = x.UseSampleFile
              SampleFileConfiguration = x.SampleFileConfiguration }

        member internal x.Binds(fTargetPdfPath: _ -> PdfFile list) =
            let targetSamplePdfFiles = 
                fTargetPdfPath {| EntityOrSample = EntityOrSample.Sample; File = x.SamplePdfFile |}

            let entityPdfFiles =
                lazy 
                    fTargetPdfPath {| EntityOrSample = EntityOrSample.Entity; File = x.EntityPdfFile.Value |}

            targetSamplePdfFiles
            |> List.mapi(fun i targetSamplePdfFile ->
                { EntityPdfFile = lazy entityPdfFiles.Value.[i]
                  SamplePdfFile = targetSamplePdfFile
                  UseSampleFile = x.UseSampleFile
                  SampleFileConfiguration = x.SampleFileConfiguration }
            )


        static member Create(pdfFile: PdfFile, ?useSampleFile, ?sampleFileConfiguration) =
            let baseDir = Path.getDirectory pdfFile.Path
            let targetPdfFile = 
                let dir = baseDir </> ".entity"
                Directory.ensure dir
                Shell.copyFile_toDir__ensureDir_override dir pdfFile.FileInfo
                |> PdfFile

            let samplePdfFile() = 
                let targetPdfPath =     
                    let dir = baseDir </> ".sample"
                    Directory.ensure dir
                    dir </> Path.GetFileName pdfFile.Path
                    
                Reuses.CreatePageTemplate()
                |> PdfRunner.Reuse(pdfFile, targetPdfPath)
  
            let useSampleFile = defaultArg useSampleFile true

            { EntityPdfFile = lazy targetPdfFile
              SamplePdfFile = 
                match useSampleFile with 
                | true -> samplePdfFile()
                | false -> targetPdfFile
              UseSampleFile = useSampleFile
              SampleFileConfiguration = defaultArg sampleFileConfiguration Configuration.DefaultValue }


        member x.CopyToTargetDir(targetDirectory) =
            x.Bind(fun pdfFile ->
                Shell.copyFile_toDir__ensureDir_override (targetDirectory </> pdfFile.EntityOrSample.DotnetPath) pdfFile.File.FileInfo
                |> PdfFile
            )
        
    type SampledPdfFileWithUserState<'UserState> with 
        member x.PdfFile =
            { SamplePdfFile = fst x.SamplePdfFileWithUserState
              EntityPdfFile = lazy fst x.EntityPdfFileWithUserState.Value
              UseSampleFile = x.UseSampleFile
              SampleFileConfiguration = x.SampleFileConfiguration
              }

    [<RequireQualifiedAccess>]
    type NewEntityPdfFileOptions =    
        | FullPath of PdfPath
        | FileName of string
    with 
        member x.FileNameValue =
            match x with 
            | NewEntityPdfFileOptions.FullPath path -> Path.GetFileName path.Path
            | NewEntityPdfFileOptions.FileName fileName -> fileName

    type PdfRunner with 
        static member OneFileFlow_FileWithUserState__Sampled(sampledPdfFile: SampledPdfFile, ?fNewEntityPdfFileOptions) =
            fun fFlow ->
                let newEntityPdfFileOptions: NewEntityPdfFileOptions option =
                    match fNewEntityPdfFileOptions with 
                    | None -> None 
                    | Some fNewFileName ->
                        fNewFileName sampledPdfFile.EntityPdfFileName
                        |> Some

                let newPdfPath (entityOrSample: EntityOrSample) =
                    match newEntityPdfFileOptions with 
                    | None -> None
                    | Some newEntityPdfFileOptions ->
                        match newEntityPdfFileOptions with 
                        | NewEntityPdfFileOptions.FileName newFileName ->
                            sampledPdfFile.BaseDir </> entityOrSample.DotnetPath </> newFileName
                            |> PdfPath
                            |> Some

                        | NewEntityPdfFileOptions.FullPath path ->
                            entityOrSample.RedirectPdfPath path
                            |> Some

                sampledPdfFile.WithUserState().Bind(fun zippedPdfFile -> 
                    
                    let pdfFile = fst zippedPdfFile.File

                    let run =
                        match newPdfPath zippedPdfFile.EntityOrSample with 
                        | None -> runWith (sampledPdfFile.Configuration zippedPdfFile.EntityOrSample) pdfFile.Path
                        | Some newPdfPath -> 
                            runWithBackupAndConfiguration
                                (sampledPdfFile.Configuration zippedPdfFile.EntityOrSample)
                                (newPdfPath.Path) 
                                pdfFile.Path 
                        
                    run (fFlow zippedPdfFile.EntityOrSample)
                    |> List.exactlyOne
                    |> fun m -> m.PdfFile, m.UserState
                )

        static member OneFileFlow_Sampled(sampledPdfFile: SampledPdfFile, ?fNewEntityPdfFileOptions) =
            fun fFlow ->
                fFlow
                |> PdfRunner.OneFileFlow_FileWithUserState__Sampled(sampledPdfFile, ?fNewEntityPdfFileOptions = fNewEntityPdfFileOptions)
                |> fun m -> m.PdfFile

        static member OneFileFlow_UserState__Sampled(sampledPdfFile: SampledPdfFile, ?fNewEntityPdfFileOptions) =
            fun fFlow ->
                fFlow
                |> PdfRunner.OneFileFlow_FileWithUserState__Sampled(sampledPdfFile, ?fNewEntityPdfFileOptions = fNewEntityPdfFileOptions)
                |> fun m -> m.UserState

        static member Reuse_Sampled(sampledPdfFile, ?fNewEntityPdfFileOptions) =
            fun fFlow ->
                let fFlow = fFlow >> Flow.Reuse 
                PdfRunner.OneFileFlow_Sampled(sampledPdfFile, ?fNewEntityPdfFileOptions = fNewEntityPdfFileOptions) fFlow

        static member Manipulate_Sampled(sampledPdfFile, ?fNewEntityPdfFileOptions) =
            fun fFlow ->
                let fFlow = fFlow >> Flow.Manipulate 
                PdfRunner.OneFileFlow_Sampled(sampledPdfFile, ?fNewEntityPdfFileOptions = fNewEntityPdfFileOptions) fFlow

        static member MergeDocuments_Sampled (inputs: SampledPdfFile al1List, ?newEntityPdfFileOptions) =
            match inputs with 
            | AtLeastOneList.One sampledPdfFile -> 
                match newEntityPdfFileOptions with 
                | None -> sampledPdfFile
                | Some entityOptions ->
                    let path =
                        match entityOptions with 
                        | NewEntityPdfFileOptions.FileName fileName ->
                            let dir = Path.GetTempFileName()
                            Directory.ensure dir
                            dir </> fileName
                            |> PdfPath 
                            
                        | NewEntityPdfFileOptions.FullPath pdfPath -> pdfPath
                    

                    sampledPdfFile.Bind(fun zippedPdfFile ->
                        let targetPdfPath = zippedPdfFile.EntityOrSample.RedirectPdfPath path
                        File.Copy(zippedPdfFile.File.Path, targetPdfPath.Path, true)
                        PdfFile targetPdfPath
                    )


            | AtLeastOneList.Many samplePdfFiles ->
                let targetPdfPath (entityOrSample: EntityOrSample) = 
                    match newEntityPdfFileOptions with 
                    | None -> 
                        Path.GetTempFileName() |> Path.changeExtension ".pdf"
                        |> PdfPath

                    | Some entityOptions ->
                        match entityOptions with 
                        | NewEntityPdfFileOptions.FullPath pdfPath -> pdfPath
                        | NewEntityPdfFileOptions.FileName fileName ->
                            let dir = Path.GetTempFileName()
                            Directory.ensure dir
                            dir </> fileName
                            |> PdfPath
                    |> entityOrSample.RedirectPdfPath


                let entityPdfFile = 
                    lazy 
                        let pdfFiles =
                            samplePdfFiles
                            |> AtLeastTwoList.map(fun m -> m.EntityPdfFile.Value)

                        PdfRunner.MergeDocuments(
                            pdfFiles, 
                            (fun args ->
                                { args with 
                                    Override = true
                                    TargetDocumentPath = (targetPdfPath EntityOrSample.Entity).Path
                                } 
                            ),
                            config = samplePdfFiles.Head.Configuration(EntityOrSample.Entity)
                        )

                let samplePdfFile = 
                    let pdfFiles =
                        samplePdfFiles
                        |> AtLeastTwoList.map(fun m -> m.SamplePdfFile)

                    PdfRunner.MergeDocuments(
                        pdfFiles, 
                        (fun args ->
                            { args with 
                                Override = true
                                TargetDocumentPath = 
                                    (targetPdfPath EntityOrSample.Sample).Path
                            }
                        ),
                        config = samplePdfFiles.Head.Configuration(EntityOrSample.Sample)
                    )
                

                { EntityPdfFile = entityPdfFile
                  SamplePdfFile = samplePdfFile
                  UseSampleFile = inputs.Head.UseSampleFile
                  SampleFileConfiguration = inputs.Head.SampleFileConfiguration }


        static member SplitDocumentToMany_Sampled (inputPdfFile: SampledPdfFile, ?fArgs) =
            let flow =
                FileOperations.splitDocumentToMany (defaultArg fArgs id)
                |> Flow.FileOperation

            inputPdfFile.Binds(fun pdfFile ->
                let config = inputPdfFile.Configuration(pdfFile.EntityOrSample)
                let flowModel =  
                    { PdfFile = pdfFile.File 
                      UserState = ()
                      Configuration = config }
                runWithFlowModel flowModel flow
                |> List.map (fun m -> m.PdfFile)
                
            )


        static member SplitDocumentBySequences_Sampled (inputPdfFile: SampledPdfFile, fSequenceTargets, ?isOverride) =
            inputPdfFile.Binds(fun pdfFile ->
                let flow =
                    FileOperations.splitDocumentBySequences(fSequenceTargets, defaultArg isOverride false)
                    |> Flow.FileOperation

                let flowModel =  
                    let config = inputPdfFile.Configuration(pdfFile.EntityOrSample)
                    {PdfFile = pdfFile.File; UserState = (); Configuration = config }

                runWithFlowModel flowModel flow
                |> List.map (fun m -> m.PdfFile)
            )


        static member SplitDocumentByPageCounts_Sampled (inputPdfFile: SampledPdfFile, fPageCounts, ?isOverride) =
            
            inputPdfFile.Binds(fun pdfFile ->
                let flow =
                    FileOperations.splitDocumentByPageCounts(fPageCounts, defaultArg isOverride false)
                    |> Flow.FileOperation

                let flowModel =  
                    let config = inputPdfFile.Configuration(pdfFile.EntityOrSample)
                    {PdfFile = pdfFile.File; UserState = (); Configuration = config }

                runWithFlowModel flowModel flow
                |> List.map (fun m -> m.PdfFile)
            )


        /// default useBleed: false
        static member OneColumn_Sampled(?fNewEntityPdfFileOptions, ?margin, ?useBleed, ?spaces, ?isForce) =
            fun pdfFile ->
                let reuse =
                    fun _-> 
                        Reuses.OneColumn(?margin = margin, ?useBleed = useBleed, ?spaces = spaces, ?isForce = isForce)

                PdfRunner.Reuse_Sampled(pdfFile,?fNewEntityPdfFileOptions = fNewEntityPdfFileOptions) reuse
    
        /// default useBleed: false
        static member OneRow(?fNewEntityPdfFileOptions, ?margin, ?useBleed, ?spaces, ?isForce) =
            fun pdfFile ->
                let reuse =
                    fun _ -> Reuses.OneRow(?margin = margin, ?useBleed = useBleed, ?spaces = spaces, ?isForce = isForce)

                PdfRunner.Reuse_Sampled(pdfFile, ?fNewEntityPdfFileOptions = fNewEntityPdfFileOptions) reuse


    