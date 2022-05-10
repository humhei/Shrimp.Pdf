namespace Shrimp.Pdf.js.shared

type jpegOptions =
    { colorTransform: int 
      decodeTransform: int [] }

type jpegInput =
    {   rawFile: string
        width: int
        height: int
        jpegOptions: jpegOptions
    }

module Shared =
    let port = 9580
    let route = "/getJpegCmykValues"    
    let remoteAddress_withRoute = 
        sprintf "http://localhost:%i%s" port route

