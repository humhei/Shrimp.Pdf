namespace Shrimp.Pdf
open iText.Kernel.Geom
open Akka.Configuration
open System.Reflection
open System.IO
open Shrimp.FSharp.Plus
open System
open Shrimp.Akkling.Cluster.Intergraction.Configuration
open iText.Kernel.Pdf
[<AutoOpen>]
module _ShpLayer =
    
    [<RequireQualifiedAccess>]
    module PdfName =
        let ShpLayer = PdfName "ShpLayer" 

    [<System.Flags>]
    type CuttingDieShpLayerInfosEnum =
        | CuttingDie = 1
        | CuttingDieDashLine = 2
        | TagInfos = 4

    [<RequireQualifiedAccess>]
    type ShpLayer =
        | Bk_XObjectOnly of string
        | Fr_XObjectOnly of string
        | BK             of string
        | Foreground     of string
        | Content        of string option        
        | Pixel          of string
        | CuttingDie     of CuttingDieShpLayerInfosEnum
        | BkSolid
        | CompoundPath
        | ClippingPath
    with 
        static member CreateContent(?name) =
            ShpLayer.Content name


        static member OfPdfObject(pdfObject: PdfObject) =
            
            let name, addtionalContent = 
                match pdfObject with 
                | :? PdfArray as pdfArray  -> 
                    let addtionalContent = 
                        match pdfArray.Size() with 
                        | 1 -> None
                        | _ -> pdfArray.Get(1) |> Some

                    pdfArray.GetAsString(0), addtionalContent
                | :? PdfString as pdfString -> pdfString, None
                | _ -> failwithf "Invalid token, cannot create ShpLayer from %A" (pdfObject.GetType())

            let getAddtionalContentAsStringOption() =
                match addtionalContent with 
                | None -> None
                | Some addtionalContent -> (addtionalContent :?> PdfString).GetValue() |> Some

            let getAddtionalContentAsString() =
                match addtionalContent with 
                | None -> failwithf "addtionalContent Cannot be empty None here"
                | Some addtionalContent -> (addtionalContent :?> PdfString).GetValue()
                    
            let getAddtionalContentAsInt() =
                match addtionalContent with 
                | None -> failwithf "addtionalContent Cannot be empty None here"
                | Some addtionalContent -> (addtionalContent :?> PdfNumber).GetValue() |> int
                    

            match name.GetValue() with 
            | nameof(ShpLayer.Bk_XObjectOnly) -> 
                ShpLayer.Bk_XObjectOnly (getAddtionalContentAsString())

            | nameof Fr_XObjectOnly -> Fr_XObjectOnly(getAddtionalContentAsString())
            | nameof BK             -> BK            (getAddtionalContentAsString())
            | nameof Foreground     -> Foreground    (getAddtionalContentAsString())
            | nameof Content        -> Content       (getAddtionalContentAsStringOption())
            | nameof Pixel          -> Pixel         (getAddtionalContentAsString())
            | nameof CuttingDie     ->  
                let enum_int = getAddtionalContentAsInt()
                let enum = enum enum_int
                CuttingDie  enum

            | nameof BkSolid        -> BkSolid       
            | nameof CompoundPath   -> CompoundPath  
            | nameof ClippingPath   -> ClippingPath  
            | _ -> failwithf "Cannot convert %A to ShpLayer" pdfObject

        member x.AsPdfObject() =
            let createPdfArray (text1: string) (text2: string) =
                let pdfArray = PdfArray()
                pdfArray.Add(PdfString text1)
                pdfArray.Add(PdfString text2)
                pdfArray :> PdfObject

            let createPdfArrayOp (text1: string) (text2: string option) =
                let pdfArray = PdfArray()
                pdfArray.Add(PdfString text1)
                match text2 with 
                | None -> ()
                | Some text2 -> pdfArray.Add(PdfString text2)
                pdfArray :> PdfObject

            match x with 
            | Bk_XObjectOnly v -> 
                let name = nameof(ShpLayer.Bk_XObjectOnly)
                createPdfArray name v
                
            | Fr_XObjectOnly v -> 
                let name = nameof(ShpLayer.Fr_XObjectOnly)
                createPdfArray name v

            | BK v -> 
                let name = nameof(ShpLayer.BK)
                createPdfArray name v

            | Foreground v -> 
                let name = nameof(ShpLayer.Foreground)
                createPdfArray name v

            | Content v -> 
                let name = nameof(ShpLayer.Content)
                createPdfArrayOp name v

            | Pixel v -> 
                let name = nameof(ShpLayer.Pixel)
                createPdfArray name v

            | CuttingDie enum -> 
                let pdfArray = PdfArray()
                let name = nameof(ShpLayer.CuttingDie)
                pdfArray.Add(PdfString name)
                pdfArray.Add(PdfNumber (int enum))
                pdfArray

            | BkSolid -> 
                let name = nameof(ShpLayer.BkSolid)
                PdfString name

            | CompoundPath -> 
                let name = nameof(ShpLayer.CompoundPath)
                PdfString name

            | ClippingPath -> 
                let name = nameof(ShpLayer.ClippingPath)
                PdfString name




    type PdfDictionary with 
        member x.PutShpLayer(shpLayer: ShpLayer) =
            x.Put(PdfName.ShpLayer, shpLayer.AsPdfObject())


module Constants =

    type private AssemblyFinder = AssemblyFinder

    let private config = 
        ConfigurationFactory
            .FromResource<AssemblyFinder>("Shrimp.Pdf.Extensions.reference.conf")
        |> Configuration.fallBackByApplicationConf

    /// default is 0.1
    let tolerance = 
        config.GetDouble("shrimp.pdf.tolerance")


    let textInfoHeightRedirectPercentage = 
        config.GetDouble("shrimp.pdf.textInfoHeightRedirectPercentage")

    let [<Literal>] MAXIMUM_MM_WIDTH = 5080.

    let [<Literal>] UNTITLED = "untitled"


    module Operators =

        let (|EQ|_|) a b =
            if a = b then Some ()
            else None


        ///closepath, fill, stroke
        ///
        ///Close,   fill,   and   stroke   path   using   nonzero   winding number rule
        let [<Literal>] b = "b"
        
        ///fill, stroke
        ///
        ///Fill and stroke path using nonzero winding number rule
        let [<Literal>] B = "B"
        
        ///closepath, eofill, stroke
        ///
        ///Close, fill, and stroke path using even-odd rule
        let [<Literal>] ``b*`` = "b*"
        
        ///eofill, stroke
        ///
        ///Fill and stroke path using even-odd rule
        let [<Literal>] ``B*`` = "B*"
        
        ///
        ///
        ///(PDF 1.2) Begin marked-content sequence with property list
        let [<Literal>] BDC = "BDC"
        
        ///
        ///
        ///Begin inline image object
        let [<Literal>] BI = "BI"
        
        ///
        ///
        ///(PDF 1.2) Begin marked-content sequence
        let [<Literal>] BMC = "BMC"
        
        ///
        ///
        ///Begin text object
        let [<Literal>] BT = "BT"
        
        ///
        ///
        ///(PDF 1.1) Begin compatibility section
        let [<Literal>] BX = "BX"
        
        ///curveto
        ///
        ///Append curved segment to path (three control points)
        let [<Literal>] c = "c"
        
        ///concat
        ///
        ///Concatenate matrix to current transformation matrix
        let [<Literal>] cm = "cm"
        
        ///setcolorspace
        ///
        ///(PDF 1.1) Set color space for stroking operations
        let [<Literal>] CS = "CS"
        
        ///setcolorspace
        ///
        ///(PDF 1.1) Set color space for nonstroking operations
        let [<Literal>] cs = "cs"
        
        ///setdash
        ///
        ///Set line dash pattern
        let [<Literal>] d = "d"
        
        ///setcharwidth
        ///
        ///Set glyph width in Type 3 font
        let [<Literal>] d0 = "d0"
        
        ///setcachedevice
        ///
        ///Set glyph width and bounding box in Type 3 font
        let [<Literal>] d1 = "d1"
        
        ///
        ///
        ///Invoke named XObject
        let [<Literal>] Do = "Do"
        
        ///
        ///
        ///(PDF 1.2) Define marked-content point with property list
        let [<Literal>] DP = "DP"
        
        ///
        ///
        ///End inline image object
        let [<Literal>] EI = "EI"
        
        ///
        ///
        ///(PDF 1.2) End marked-content sequence
        let [<Literal>] EMC = "EMC"
        
        ///
        ///
        ///End text object
        let [<Literal>] ET = "ET"
        
        ///
        ///
        ///(PDF 1.1) End compatibility section
        let [<Literal>] EX = "EX"
        
        ///fill
        ///
        ///Fill path using nonzero winding number rule
        let [<Literal>] f = "f"
        
        ///fill
        ///
        ///Fill path using nonzero winding number rule (obsolete)
        let [<Literal>] F = "F"
        
        ///eofill
        ///
        ///Fill path using even-odd rule
        let [<Literal>] ``f*`` = "f*"
        
        ///setgray
        ///
        ///Set gray level for stroking operations
        let [<Literal>] G = "G"
        
        ///setgray
        ///
        ///Set gray level for nonstroking operations
        let [<Literal>] g = "g"
        
        ///
        ///
        ///(PDF 1.2) Set parameters from graphics state parameter dictionary
        let [<Literal>] gs = "gs"
        
        ///closepath
        ///
        ///Close subpath
        let [<Literal>] h = "h"
        
        ///setflat
        ///
        ///Set flatness tolerance
        let [<Literal>] i = "i"
        
        ///
        ///
        ///Begin inline image data
        let [<Literal>] ID = "ID"
        
        ///setlinejoin
        ///
        ///Set line join style
        let [<Literal>] j = "j"
        
        ///setlinecap
        ///
        ///Set line cap style
        let [<Literal>] J = "J"
        
        ///setcmykcolor
        ///
        ///Set CMYK color for stroking operations
        let [<Literal>] K = "K"
        
        ///setcmykcolor
        ///
        ///Set CMYK color for nonstroking operations
        let [<Literal>] k = "k"
        
        ///lineto
        ///
        ///Append straight line segment to path
        let [<Literal>] l = "l"
        
        ///moveto
        ///
        ///Begin new subpath
        let [<Literal>] m = "m"
        
        ///setmiterlimit
        ///
        ///Set miter limit
        let [<Literal>] M = "M"
        
        ///
        ///
        ///(PDF 1.2) Define marked-content point
        let [<Literal>] MP = "MP"
        
        ///
        ///
        ///End path without filling or stroking
        let [<Literal>] n = "n"
        
        ///gsave
        ///
        ///Save graphics state
        let [<Literal>] q = "q"
        
        ///grestore
        ///
        ///Restore graphics state
        let [<Literal>] Q = "Q"
        
        ///
        ///
        ///Append rectangle to path
        let [<Literal>] re = "re"
        
        ///setrgbcolor
        ///
        ///Set RGB color for stroking operations
        let [<Literal>] RG = "RG"
        
        ///setrgbcolor
        ///
        ///Set RGB color for nonstroking operations
        let [<Literal>] rg = "rg"
        
        ///
        ///
        ///Set color rendering intent
        let [<Literal>] ri = "ri"
        
        ///closepath, stroke
        ///
        ///Close and stroke path
        let [<Literal>] s = "s"
        
        ///stroke
        ///
        ///Stroke path
        let [<Literal>] S = "S"
        
        ///setcolor
        ///
        ///(PDF 1.1) Set color for stroking operations
        let [<Literal>] SC = "SC"
        
        ///setcolor
        ///
        ///(PDF 1.1) Set color for nonstroking operations
        let [<Literal>] sc = "sc"
        
        ///setcolor
        ///
        ///(PDF 1.2)  Set  color  for stroking  operations  (ICCBased and special colour spaces)
        let [<Literal>] SCN = "SCN"
        
        ///setcolor
        ///
        ///(PDF    1.2)    Set    color    for    nonstroking    operations (ICCBased and special colour spaces)
        let [<Literal>] scn = "scn"
        
        ///shfill
        ///
        ///(PDF 1.3) Paint area defined by shading pattern
        let [<Literal>] sh = "sh"
        
        ///
        ///
        ///Move to start of next text line
        let [<Literal>] ``T*`` = "T*"
        
        ///
        ///
        ///Set character spacing
        let [<Literal>] Tc = "Tc"
        
        ///
        ///
        ///Move text position
        let [<Literal>] Td = "Td"
        
        ///
        ///
        ///Move text position and set leading
        let [<Literal>] TD = "TD"
        
        ///selectfont
        ///
        ///Set text font and size
        let [<Literal>] Tf = "Tf"
        
        ///show
        ///
        ///Show text
        let [<Literal>] Tj = "Tj"
        
        ///
        ///
        ///Show text, allowing individual glyph positioning
        let [<Literal>] TJ = "TJ"
        
        ///
        ///
        ///Set text leading
        let [<Literal>] TL = "TL"
        
        ///
        ///
        ///Set text matrix and text line matrix
        let [<Literal>] Tm = "Tm"
        
        ///
        ///
        ///Set text rendering mode
        let [<Literal>] Tr = "Tr"
        
        ///
        ///
        ///Set text rise
        let [<Literal>] Ts = "Ts"
        
        ///
        ///
        ///Set word spacing
        let [<Literal>] Tw = "Tw"
        
        ///
        ///
        ///Set horizontal text scaling
        let [<Literal>] Tz = "Tz"
        
        ///curveto
        ///
        ///Append curved segment to path (initial point replicated)
        let [<Literal>] v = "v"
        
        ///setlinewidth
        ///
        ///Set line width
        let [<Literal>] w = "w"
        
        ///clip
        ///
        ///Set clipping path using nonzero winding number rule
        let [<Literal>] W = "W"
        
        ///eoclip
        ///
        ///Set clipping path using even-odd rule
        let [<Literal>] ``W*`` = "W*"
        
        ///curveto
        ///
        ///Append curved segment to path (final point replicated)
        let [<Literal>] y = "y"
        
        

[<AutoOpen>]
module Operators =
    open Constants



    type FsPdfObjectID =
        { ObjNumber: int 
          GenNumber: int }

    type SpawnablePdfObjectID =
        { ObjNumber: int 
          GenNumber: int
          IsSpawned: bool }
    with 
        static member OfPdfObjectID(id: FsPdfObjectID, ?isSpawnded) =
            { ObjNumber = id.ObjNumber
              GenNumber = id.GenNumber 
              IsSpawned = defaultArg isSpawnded false }

    let hashNumberOfPdfIndirectReference(pdfIndirectReference: PdfIndirectReference) =
        { ObjNumber = pdfIndirectReference.GetObjNumber() 
          GenNumber = pdfIndirectReference.GetGenNumber() }


    /// approximately equal to 
    /// benchmark by (CONFIG: shrimp.pdf.tolerance (default is 0.1))
    let (@=) a b =
        (abs (a - b)) < tolerance

    ///// approximately bigger or equal to 
    ///// benchmark by (CONFIG: shrimp.pdf.tolerance (default is 0.1))
    //let (>=@) a b =
    //    a > b
    //    || (abs (a - b)) < tolerance

    /// unSerializable
    type NearbyPX (v: float, tolerance) =
        inherit CustomComparableBase<float>(v, fun a b ->
            if (abs (a-b)) <= tolerance
            then 0
            else compare a b
        )

        member x.Value = v

        new (v) =
            NearbyPX(v, tolerance)
            

    /// defaultConversion: mm to user unit
    let mm (mm: float) =
        mm / 25.4 * 72.


    let CM (cm: float) =
        mm cm * 10.

    let mmZ (mm: float) =
        mm / 25.4 * 72.
        |> ``ufloat>0``

    /// defaultConversion: userUnit to mm
    let userUnitToMM (userUnit: float) =
        userUnit / 72. * 25.4



    let inchToMM (inch: float) =
        inch * 25.4

    let inch (inch: float) =
        inch * 72.