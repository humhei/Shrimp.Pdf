using iText.Kernel.Pdf;
using iText.Kernel.Pdf.Canvas;
using System;
using System.Runtime.CompilerServices;




[assembly: InternalsVisibleTo("Shrimp.Pdf.Parser")]
namespace Shrimp.Pdf.Parser.Helper
{
    /// <summary>
    /// fsharp has limited internal protected support in release mode?
    /// field access exception will be threw
    /// </summary>
    internal class CanvasGraphicsStateSettablePdfCanvas : PdfCanvas
    {
        public CanvasGraphicsStateSettablePdfCanvas(PdfStream contentStream, PdfResources resources, PdfDocument document) : base(contentStream, resources, document)
        {
        }

        public void SetCanvasGraphicsState(CanvasGraphicsState gs)
        {
            this.currentGs = gs;
        }
    }

}
