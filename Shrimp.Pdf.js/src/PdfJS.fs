// ts2fable 0.8.0-build.658
module rec PdfJS

#nowarn "3390" // disable warnings for invalid XML comments

open System
open Fable.Core
open Fable.Core.JS
open Browser.Types

let [<Import("version","../node_modules/pdfjs-dist/lib/pdf.js")>] version: string = jsNative
let [<Import("GlobalWorkerOptions","../node_modules/pdfjs-dist/lib/pdf.js")>] GlobalWorkerOptions: GlobalWorkerOptions = jsNative
let [<Import("PDFJS","../node_modules/pdfjs-dist/lib/pdf.js")>] PDFJS: PDFJSStatic = jsNative
let [<Import("Util","../node_modules/pdfjs-dist/lib/pdf.js")>] Util: PDFJSUtilStatic = jsNative

type [<AllowNullLiteral>] IExports =
    abstract GlobalWorkerOptions: GlobalWorkerOptions
    abstract PDFDataRangeTransport: PDFDataRangeTransportStatic
    abstract PDFWorker: PDFWorkerStatic
    abstract CMapReaderFactory: CMapReaderFactoryStatic
    /// <summary>
    /// This is the main entry point for loading a PDF and interacting with it.
    /// NOTE: If a URL is used to fetch the PDF data a standard XMLHttpRequest(XHR)
    /// is used, which means it must follow the same origin rules that any XHR does
    /// e.g. No corss domain requests without CORS.
    /// </summary>
    /// <param name="source" />
    /// <param name="pdfDataRangeTransport">Used if you want to manually server range requests for data in the PDF.</param>
    /// <param name="passwordCallback">Used to request a password if wrong or no password was provided.  The callback receives two parameters: function that needs to be called with new password and the reason.</param>
    /// <param name="progressCallback">Progress callback.</param>
    /// <returns>A promise that is resolved with PDFDocumentProxy object.</returns>
    abstract getDocument: url: string * ?pdfDataRangeTransport: PDFDataRangeTransport * ?passwordCallback: ((string -> unit) -> string -> string) * ?progressCallback: (PDFProgressData -> unit) -> PDFLoadingTask<PDFDocumentProxy>
    abstract getDocument: data: U2<Uint8Array, obj> * ?pdfDataRangeTransport: PDFDataRangeTransport * ?passwordCallback: ((string -> unit) -> string -> string) * ?progressCallback: (PDFProgressData -> unit) -> PDFLoadingTask<PDFDocumentProxy>
    abstract getDocument: source: PDFSource * ?pdfDataRangeTransport: PDFDataRangeTransport * ?passwordCallback: ((string -> unit) -> string -> string) * ?progressCallback: (PDFProgressData -> unit) -> PDFLoadingTask<PDFDocumentProxy>

type [<AllowNullLiteral>] GlobalWorkerOptions =
    abstract workerSrc: string with get, set
    abstract workerPort: PDFWorker option with get, set

type [<AllowNullLiteral>] PDFTreeNode =
    abstract title: string with get, set
    abstract bold: bool with get, set
    abstract italic: bool with get, set
    abstract color: ResizeArray<float> with get, set
    abstract dest: obj option with get, set
    abstract items: ResizeArray<PDFTreeNode> with get, set

type [<AllowNullLiteral>] PDFInfo =
    abstract PDFFormatVersion: string with get, set
    abstract IsAcroFormPresent: bool with get, set
    abstract IsXFAPresent: bool with get, set
    [<EmitIndexer>] abstract Item: key: string -> obj option with get, set

type [<AllowNullLiteral>] PDFMetadata =
    abstract parse: unit -> unit
    abstract get: name: string -> string
    abstract has: name: string -> bool

type [<AllowNullLiteral>] PDFDataRangeTransportListener =
    [<Emit("$0($1...)")>] abstract Invoke: loaded: float * total: float -> unit

type [<RequireQualifiedAccess>] VerbosityLevel =
    | ERRORS = 0
    | WARNINGS = 1
    | INFOS = 5

type [<AllowNullLiteral>] PDFDataRangeTransport =
    abstract addRangeListener: listener: PDFDataRangeTransportListener -> unit
    abstract addProgressListener: listener: PDFDataRangeTransportListener -> unit
    abstract addProgressiveReadListener: listener: PDFDataRangeTransportListener -> unit
    abstract addProgressiveDoneListener: listener: PDFDataRangeTransportListener -> unit
    abstract onDataRange: ``begin``: float * chunk: obj -> unit
    abstract onDataProgress: loaded: float * total: float -> unit
    abstract onDataProgressiveRead: chunk: obj -> unit
    abstract onDataProgressiveDone: unit -> unit
    abstract transportReady: unit -> unit
    abstract requestDataRange: ``begin``: float * ``end``: float -> unit
    abstract abort: unit -> unit

type [<AllowNullLiteral>] PDFDataRangeTransportStatic =
    [<EmitConstructor>] abstract Create: length: float * initialData: U2<Uint8Array, obj> * ?progressiveDone: bool -> PDFDataRangeTransport

type [<AllowNullLiteral>] PDFWorkerParameters =
    abstract name: string option with get, set
    abstract port: obj option with get, set
    abstract verbosity: VerbosityLevel option with get, set

type [<AllowNullLiteral>] PDFWorker =
    abstract promise: Promise<obj>
    abstract port: obj option option
    abstract messageHandler: obj option
    abstract destroy: unit -> unit

type [<AllowNullLiteral>] PDFWorkerStatic =
    [<EmitConstructor>] abstract Create: ?``params``: PDFWorkerParameters -> PDFWorker
    abstract fromPort: ?``params``: PDFWorkerParameters -> PDFWorker
    abstract getWorkerSrc: unit -> string

type [<RequireQualifiedAccess>] CMapCompressionType =
    | NONE = 0
    | BINARY = 1
    | STREAM = 2

type [<AllowNullLiteral>] CMapReaderFactory =
    interface end

type [<AllowNullLiteral>] CMapReaderFactoryStatic =
    [<EmitConstructor>] abstract Create: ``params``: {| baseUrl: string; isCompressed: bool |} -> CMapReaderFactory

type [<AllowNullLiteral>] CMapReader =
    abstract fetch: ``params``: {| name: string |} -> Promise<{| cMapData: obj option; compressionType: CMapCompressionType |}>

type [<AllowNullLiteral>] PDFSource =
    /// The URL of the PDF.
    abstract url: string option with get, set
    /// Binary PDF data. Use typed arrays
    /// (Uint8Array) to improve the memory usage. If PDF data is BASE64-encoded,
    /// use atob() to convert it to a binary string first.
    abstract data: U3<Uint8Array, obj, string> option with get, set
    /// Basic authentication headers.
    abstract httpHeaders: PDFSourceHttpHeaders option with get, set
    /// For decrypting password-protected PDFs.
    abstract password: string option with get, set
    /// Indicates whether or not cross-site
    /// Access-Control requests should be made using credentials such as cookies
    /// or authorization headers. The default is false.
    abstract withCredentials: bool option with get, set
    abstract initialData: U2<Uint8Array, obj> option with get, set
    abstract length: float option with get, set
    /// range
    abstract range: PDFDataRangeTransport option with get, set
    /// Optional parameter to specify
    /// maximum number of bytes fetched per range request. The default value is
    /// 2^16 = 65536.
    abstract rangeChunkSize: float option with get, set
    /// The worker that will be used for
    /// the loading and parsing of the PDF data.
    abstract worker: PDFWorker option with get, set
    /// Controls the logging level; the
    /// constants from {VerbosityLevel} should be used.
    abstract verbosity: float option with get, set
    /// The base URL of the document,
    /// used when attempting to recover valid absolute URLs for annotations, and
    /// outline items, that (incorrectly) only specify relative URLs.
    abstract docBaseUrl: string option with get, set
    /// Strategy for
    /// decoding certain (simple) JPEG images in the browser. This is useful for
    /// environments without DOM image and canvas support, such as e.g. Node.js.
    /// Valid values are 'decode', 'display' or 'none'; where 'decode' is intended
    /// for browsers with full image/canvas support, 'display' for environments
    /// with limited image support through stubs (useful for SVG conversion),
    /// and 'none' where JPEG images will be decoded entirely by PDF.js.
    /// The default value is 'decode'.
    abstract nativeImageDecoderSupport: PDFSourceNativeImageDecoderSupport option with get, set
    /// The URL where the predefined
    /// Adobe CMaps are located. Include trailing slash.
    abstract cMapUrl: string option with get, set
    /// Specifies if the Adobe CMaps are
    /// binary packed.
    abstract cMapPacked: bool option with get, set
    /// <summary>
    /// The factory that will be
    /// used when reading built-in CMap files. Providing a custom factory is useful
    /// for environments without <c>XMLHttpRequest</c> support, such as e.g. Node.js.
    /// The default value is {DOMCMapReaderFactory}.
    /// </summary>
    abstract CMapReaderFactory: obj option with get, set
    /// <summary>
    /// Reject certain promises, e.g.
    /// <c>getOperatorList</c>, <c>getTextContent</c>, and <c>RenderTask</c>, when the associated
    /// PDF data cannot be successfully parsed, instead of attempting to recover
    /// whatever possible of the data. The default value is <c>false</c>.
    /// </summary>
    abstract stopAtErrors: bool option with get, set
    /// The maximum allowed image size
    /// in total pixels, i.e. width * height. Images above this value will not be
    /// rendered. Use -1 for no limit, which is also the default value.
    abstract maxImageSize: float option with get, set
    /// <summary>
    /// Determines if we can eval
    /// strings as JS. Primarily used to improve performance of font rendering,
    /// and when parsing PDF functions. The default value is <c>true</c>.
    /// </summary>
    abstract isEvalSupported: bool option with get, set
    /// <summary>
    /// By default fonts are
    ///    converted to OpenType fonts and loaded via font face rules. If disabled,
    ///    fonts will be rendered using a built-in font renderer that constructs the
    ///    glyphs with primitive path commands. The default value is <c>false</c>.
    /// </summary>
    abstract disableFontFace: bool option with get, set
    /// <summary>
    /// Disable range request loading
    ///    of PDF files. When enabled, and if the server supports partial content
    ///    requests, then the PDF will be fetched in chunks.
    ///    The default value is <c>false</c>.
    /// </summary>
    abstract disableRange: bool option with get, set
    /// <summary>
    /// Disable streaming of PDF file
    ///    data. By default PDF.js attempts to load PDFs in chunks.
    ///    The default value is <c>false</c>.
    /// </summary>
    abstract disableStream: bool option with get, set
    /// <summary>
    /// Disable pre-fetching of PDF
    ///    file data. When range requests are enabled PDF.js will automatically keep
    ///    fetching more data even if it isn't needed to display the current page.
    ///    The default value is <c>false</c>.
    ///    NOTE: It is also necessary to disable streaming, see above,
    ///          in order for disabling of pre-fetching to work correctly.
    /// </summary>
    abstract disableAutoFetch: bool option with get, set
    /// <summary>
    /// Disable the use of
    ///    <c>URL.createObjectURL</c>, for compatibility with older browsers.
    ///    The default value is <c>false</c>.
    /// </summary>
    abstract disableCreateObjectURL: bool option with get, set
    /// <summary>
    /// Enables special hooks for debugging
    /// PDF.js (see <c>web/debugger.js</c>). The default value is <c>false</c>.
    /// </summary>
    abstract pdfBug: bool option with get, set

type [<AllowNullLiteral>] PDFProgressData =
    abstract loaded: float with get, set
    abstract total: float with get, set

type [<AllowNullLiteral>] PDFDocumentProxy =
    /// Total number of pages the PDF contains.
    abstract numPages: float with get, set
    /// A unique ID to identify a PDF.  Not guaranteed to be unique.  [jbaldwin: haha what]
    abstract fingerprint: string with get, set
    /// True if embedded document fonts are in use.  Will be set during rendering of the pages.
    abstract embeddedFontsUsed: unit -> bool
    /// <param name="number">The page number to get.  The first page is 1.</param>
    /// <returns>A promise that is resolved with a PDFPageProxy.</returns>
    abstract getPage: number: float -> Promise<PDFPageProxy>
    /// TODO: return type of Promise<???>
    ///   A promise that is resolved with a lookup table for mapping named destinations to reference numbers.
    abstract getDestinations: unit -> Promise<ResizeArray<obj option>>
    /// A promise that is resolved with an array of all the JavaScript strings in the name tree.
    abstract getJavaScript: unit -> Promise<ResizeArray<string>>
    /// A promise that is resolved with an array that is a tree outline (if it has one) of the PDF.  @see PDFTreeNode
    abstract getOutline: unit -> Promise<ResizeArray<PDFTreeNode>>
    /// A promise that is resolved with an array that contains the permission flags for the PDF document.
    abstract getPermissions: unit -> Promise<ResizeArray<float>>
    /// A promise that is resolved with the info and metadata of the PDF.
    abstract getMetadata: unit -> Promise<{| info: PDFInfo; metadata: PDFMetadata |}>
    /// Is the PDF encrypted?
    abstract isEncrypted: unit -> Promise<bool>
    /// A promise that is resolved with Uint8Array that has the raw PDF data.
    abstract getData: unit -> Promise<Uint8Array>
    /// TODO: return type of Promise<???>
    /// A promise that is resolved when the document's data is loaded.
    abstract dataLoaded: unit -> Promise<ResizeArray<obj option>>
    abstract destroy: unit -> unit

type [<AllowNullLiteral>] PDFRef =
    abstract num: float with get, set
    abstract gen: obj option with get, set

type [<AllowNullLiteral>] PDFPageViewportOptions =
    abstract viewBox: obj option with get, set
    abstract scale: float with get, set
    abstract rotation: float with get, set
    abstract offsetX: float with get, set
    abstract offsetY: float with get, set
    abstract dontFlip: bool with get, set

type [<AllowNullLiteral>] PDFPageViewport =
    abstract width: float with get, set
    abstract height: float with get, set
    abstract scale: float with get, set
    abstract transforms: ResizeArray<float> with get, set
    abstract clone: options: PDFPageViewportOptions -> PDFPageViewport
    abstract convertToViewportPoint: x: float * y: float -> ResizeArray<float>
    abstract convertToViewportRectangle: rect: ResizeArray<float> -> ResizeArray<float>
    abstract convertToPdfPoint: x: float * y: float -> ResizeArray<float>

type [<AllowNullLiteral>] ViewportParameters =
    abstract scale: float with get, set
    abstract rotation: float option with get, set
    abstract offsetX: float option with get, set
    abstract offsetY: float option with get, set
    abstract dontFlip: bool option with get, set

type [<AllowNullLiteral>] PDFAnnotationData =
    abstract subtype: string with get, set
    abstract rect: ResizeArray<float> with get, set
    abstract annotationFlags: obj option with get, set
    abstract color: ResizeArray<float> with get, set
    abstract borderWidth: float with get, set
    abstract hasAppearance: bool with get, set

type [<AllowNullLiteral>] PDFAnnotations =
    abstract getData: unit -> PDFAnnotationData
    abstract hasHtml: unit -> bool
    abstract getHtmlElement: commonOjbs: obj option -> obj
    abstract getEmptyContainer: tagName: string * rect: ResizeArray<float> -> obj
    abstract isViewable: unit -> bool
    abstract loadResources: keys: obj option -> Promise<obj option>
    abstract getOperatorList: evaluator: obj option -> Promise<obj option>

type [<AllowNullLiteral>] PDFRenderTextLayer =
    abstract beginLayout: unit -> unit
    abstract endLayout: unit -> unit
    abstract appendText: unit -> unit

type [<AllowNullLiteral>] PDFRenderImageLayer =
    abstract beginLayout: unit -> unit
    abstract endLayout: unit -> unit
    abstract appendImage: unit -> unit

type [<AllowNullLiteral>] PDFRenderParams =
    abstract canvasContext: obj with get, set
    abstract viewport: PDFPageViewport option with get, set
    abstract textLayer: PDFRenderTextLayer option with get, set
    abstract imageLayer: PDFRenderImageLayer option with get, set
    abstract renderInteractiveForms: bool option with get, set
    abstract continueCallback: ((unit -> unit) -> unit) option with get, set

type [<AllowNullLiteral>] PDFViewerParams =
    abstract container: obj with get, set
    abstract viewer: obj option with get, set

type [<AllowNullLiteral>] GetTextContentParams =
    abstract normalizeWhitespace: bool option with get, set
    abstract disableCombineTextItems: bool option with get, set
    abstract includeMarkedContent: bool option with get, set

/// RenderTask is basically a promise but adds a cancel function to termiate it.
type [<AllowNullLiteral>] PDFRenderTask =
    inherit PDFLoadingTask<PDFPageProxy>
    /// Cancel the rendering task.  If the task is currently rendering it will not be cancelled until graphics pauses with a timeout.  The promise that this object extends will resolve when cancelled.
    abstract cancel: unit -> unit

type [<AllowNullLiteral>] PDFPageProxy =
    /// Page index of the page.  First page is 0.
    abstract pageIndex: float with get, set
    /// Page number of the page.  First page is 1.
    abstract pageNumber: float with get, set
    /// The number of degrees the page is rotated clockwise.
    abstract rotate: float with get, set
    /// The reference that points to this page.
    abstract ref: PDFRef with get, set
    /// <returns>An array of the visible portion of the PDF page in the user space units - [x1, y1, x2, y2].</returns>
    abstract view: ResizeArray<float> with get, set
    /// <param name="params">viewport options</param>
    /// <returns />
    abstract getViewport: ``params``: ViewportParameters -> PDFPageViewport
    /// A promise that is resolved with an array of the annotation objects.
    abstract getAnnotations: unit -> Promise<PDFAnnotations>
    /// <summary>Begins the process of rendering a page to the desired context.</summary>
    /// <param name="params">Rendering options.</param>
    /// <returns>An extended promise that is resolved when the page finishes rendering.</returns>
    abstract render: ``params``: PDFRenderParams -> PDFRenderTask
    /// A promise that is resolved with the string that is the text content frm the page.
    abstract getTextContent: ?``params``: GetTextContentParams -> Promise<TextContent>
    /// Destroyes resources allocated by the page.
    abstract destroy: unit -> unit

type [<AllowNullLiteral>] TextContentItem =
    abstract str: string with get, set
    abstract transform: ResizeArray<float> with get, set
    abstract width: float with get, set
    abstract height: float with get, set
    abstract dir: string with get, set
    abstract fontName: string with get, set
    abstract hasEOL: bool with get, set

type [<AllowNullLiteral>] TextContent =
    abstract items: ResizeArray<TextContentItem> with get, set
    abstract styles: obj option with get, set

/// <summary>A PDF document and page is built of many objects.  E.g. there are objects for fonts, images, rendering code and such.  These objects might get processed inside of a worker.  The <c>PDFObjects</c> implements some basic functions to manage these objects.</summary>
type [<AllowNullLiteral>] PDFObjects =
    abstract get: objId: float * ?callback: obj -> obj option
    abstract resolve: objId: float * data: obj option -> obj option
    abstract isResolved: objId: float -> bool
    abstract hasData: objId: float -> bool
    abstract getData: objId: float -> obj option
    abstract clear: unit -> unit

type [<AllowNullLiteral>] PDFJSUtilStatic =
    /// <summary>Normalize rectangle so that (x1,y1) &lt; (x2,y2)</summary>
    /// <param name="rect">
    /// the rectangle with [x1,y1,x2,y2]
    /// 
    /// For coordinate systems whose origin lies in the bottom-left, this
    /// means normalization to (BL,TR) ordering. For systems with origin in the
    /// top-left, this means (TL,BR) ordering.
    /// </param>
    abstract normalizeRect: rect: ResizeArray<float> -> ResizeArray<float>

type [<AllowNullLiteral>] PDFJSStatic =
    /// The maximum allowed image size in total pixels e.g. width * height.  Images above this value will not be drawn.  Use -1 for no limit.
    abstract maxImageSize: float with get, set
    /// The url of where the predefined Adobe CMaps are located. Include trailing
    /// slash.
    abstract cMapUrl: string with get, set
    /// Specifies if CMaps are binary packed.
    abstract cMapPacked: bool with get, set
    /// By default fonts are converted to OpenType fonts and loaded via font face rules.  If disabled, the font will be rendered using a built in font renderer that constructs the glyphs with primitive path commands.
    abstract disableFontFace: bool with get, set
    /// Path for image resources, mainly for annotation icons. Include trailing
    /// slash.
    abstract imageResourcesPath: string with get, set
    /// Disable the web worker and run all code on the main thread. This will happen
    /// automatically if the browser doesn't support workers or sending typed arrays
    /// to workers.
    abstract disableWorker: bool with get, set
    /// Path and filename of the worker file. Required when the worker is enabled in
    /// development mode. If unspecified in the production build, the worker will be
    /// loaded based on the location of the pdf.js file.
    abstract workerSrc: string with get, set
    /// Disable range request loading of PDF files. When enabled and if the server
    /// supports partial content requests then the PDF will be fetched in chunks.
    /// Enabled (false) by default.
    abstract disableRange: bool with get, set
    /// Disable streaming of PDF file data. By default PDF.js attempts to load PDF
    /// in chunks. This default behavior can be disabled.
    abstract disableStream: bool with get, set
    /// Disable pre-fetching of PDF file data. When range requests are enabled PDF.js
    /// will automatically keep fetching more data even if it isn't needed to display
    /// the current page. This default behavior can be disabled.
    /// 
    /// NOTE: It is also necessary to disable streaming, see above,
    ///        in order for disabling of pre-fetching to work correctly.
    abstract disableAutoFetch: bool with get, set
    /// Enables special hooks for debugging PDF.js.
    abstract pdfBug: bool with get, set
    /// Enables transfer usage in postMessage for ArrayBuffers.
    abstract postMessageTransfers: bool with get, set
    /// Disables URL.createObjectURL usage.
    abstract disableCreateObjectURL: bool with get, set
    /// Disables WebGL usage.
    abstract disableWebGL: bool with get, set
    /// Disables fullscreen support, and by extension Presentation Mode,
    /// in browsers which support the fullscreen API.
    abstract disableFullscreen: bool with get, set
    /// Disable the text layer of PDF when used PDF.js renders a canvas instead of div elements
    abstract disableTextLayer: bool with get, set
    /// Enables CSS only zooming.
    abstract useOnlyCssZoom: bool with get, set
    /// Controls the logging level.
    /// The constants from PDFJS.VERBOSITY_LEVELS should be used:
    /// - errors
    /// - warnings [default]
    /// - infos
    abstract verbosity: float with get, set
    /// The maximum supported canvas size in total pixels e.g. width * height.
    /// The default value is 4096 * 4096. Use -1 for no limit.
    abstract maxCanvasPixels: float with get, set
    /// Opens external links in a new window if enabled. The default behavior opens
    /// external links in the PDF.js window.
    abstract openExternalLinksInNewWindow: bool with get, set
    /// Determines if we can eval strings as JS. Primarily used to improve
    /// performance for font rendering.
    abstract isEvalSupported: bool with get, set
    abstract PDFViewer: ``params``: PDFViewerParams -> unit
    /// <summary>
    /// yet another viewer, this will render only one page at the time, reducing rendering time
    /// very important for mobile development
    /// </summary>
    abstract PDFSinglePageViewer: ``params``: PDFViewerParams -> unit

type [<AllowNullLiteral>] PDFLoadingTask<'T> =
    abstract promise: Promise<'T> with get, set

type [<AllowNullLiteral>] PDFSourceHttpHeaders =
    [<EmitIndexer>] abstract Item: key: string -> string with get, set

type [<StringEnum>] [<RequireQualifiedAccess>] PDFSourceNativeImageDecoderSupport =
    | Decode
    | Display
    | None
