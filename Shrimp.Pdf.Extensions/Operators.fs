namespace Shrimp.Pdf
open iText.Kernel.Geom
open Akka.Configuration
open System.Reflection
open System.IO
open Shrimp.FSharp.Plus
open System
open Shrimp.Akkling.Cluster.Intergraction.Configuration

module Constants =
    
    type private AssemblyFinder = AssemblyFinder

    let private config = 
        lazy
            ConfigurationFactory
                .FromResource<AssemblyFinder>("Shrimp.Pdf.Extensions.reference.conf")
            |> Configuration.fallBackByApplicationConf

    let tolerance = 
        lazy
            config.Value.GetDouble("shrimp.pdf.tolerance")


    let textInfoHeightRedirectPercentage = 
        lazy
            config.Value.GetDouble("shrimp.pdf.textInfoHeightRedirectPercentage")

    let [<Literal>] MAXIMUM_MM_WIDTH = 5080.


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
    /// approximately equal to 
    /// benchmark by (CONFIG: shrimp.pdf.tolerance (default is 0.1))
    let (@=) a b =
        (abs (a - b)) < tolerance.Value

    ///// approximately bigger or equal to 
    ///// benchmark by (CONFIG: shrimp.pdf.tolerance (default is 0.1))
    //let (>=@) a b =
    //    a > b
    //    || (abs (a - b)) < tolerance.Value

    /// unSerializable
    type NearbyPX(v: float) =
        inherit CustomEquatable<float>(v, fun a b ->
            (abs (a-b)) <= tolerance.Value
        )

        member x.Value = v


    /// defaultConversion: mm to user unit
    let mm (mm: float) =
        mm / 25.4 * 72.

    let mmZ (mm: float) =
        mm / 25.4 * 72.
        |> ``ufloat>0``

    /// defaultConversion: userUnit to mm
    let userUnitToMM (userUnit: float) =
        userUnit / 72. * 25.4

