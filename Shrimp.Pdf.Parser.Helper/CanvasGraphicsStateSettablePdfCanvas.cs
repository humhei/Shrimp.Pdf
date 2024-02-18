using iText.Kernel.Pdf;
using iText.Kernel.Pdf.Canvas;
using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: InternalsVisibleTo("Shrimp.Pdf.Parser")]
namespace Shrimp.Pdf.Parser.Helper
{


    public abstract class FsPdfObjectWrapper<T> : PdfObjectWrapper<T> where T : PdfObject
    {
        private bool isWrappedObjectMustBeIndirect;

        public FsPdfObjectWrapper(T pdfObject, bool isWrappedObjectMustBeIndirect) : base(pdfObject)
        {
            this.isWrappedObjectMustBeIndirect = isWrappedObjectMustBeIndirect;

        }

        protected override bool IsWrappedObjectMustBeIndirect()
        {
            return isWrappedObjectMustBeIndirect;
        }



    }


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
