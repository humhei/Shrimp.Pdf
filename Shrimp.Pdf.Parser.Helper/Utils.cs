using iText.Kernel.Geom;
using iText.Kernel.Pdf;
using iText.Kernel.Pdf.Canvas;
using System;
using System.Drawing.Imaging;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: InternalsVisibleTo("Shrimp.Pdf.Parser")]
namespace Shrimp.Pdf.Parser.Helper
{


    public abstract class PdfCanvasStatic
    {
        private static float[] CalculateTransformationMatrix(Vector expectedMin, Vector expectedMax, Vector actualMin
            , Vector actualMax)
        {
            // Calculates a matrix such that if you multiply the actual vertices by it, you get the expected vertices
            float[] result = new float[6];
            result[0] = (expectedMin.Get(Vector.I1) - expectedMax.Get(Vector.I1)) / (actualMin.Get(Vector.I1) - actualMax
                .Get(Vector.I1));
            result[1] = 0;
            result[2] = 0;
            result[3] = (expectedMin.Get(Vector.I2) - expectedMax.Get(Vector.I2)) / (actualMin.Get(Vector.I2) - actualMax
                .Get(Vector.I2));
            result[4] = expectedMin.Get(Vector.I1) - actualMin.Get(Vector.I1) * result[0];
            result[5] = expectedMin.Get(Vector.I2) - actualMin.Get(Vector.I2) * result[3];
            return result;
        }

        public static float[] CalculateTransformationMatrix(Rectangle originRect, Rectangle targetRect)
        {
            Vector bBoxMin = new Vector(originRect.GetLeft(), originRect.GetBottom(), 1);
            Vector bBoxMax = new Vector(originRect.GetRight(), originRect.GetTop(), 1);
            Vector rectMin = new Vector(targetRect.GetLeft(), targetRect.GetBottom(), 1);
            Vector rectMax = new Vector(targetRect.GetRight(), targetRect.GetTop(), 1);
            float[] result = CalculateTransformationMatrix(rectMin, rectMax, bBoxMin
            , bBoxMax);

            return result;

        }

    }


}
