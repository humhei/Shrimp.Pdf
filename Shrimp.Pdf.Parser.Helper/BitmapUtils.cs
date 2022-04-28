using iText.Kernel.Pdf;
using iText.Kernel.Pdf.Canvas;
using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace System.Drawing
{

    public class BitmapUtils
    {

        public static BitmapColorValues ReadColorValues(Bitmap bmp)
        {

            // Lock the bitmap's bits.  
            Rectangle rect = new Rectangle(0, 0, bmp.Width, bmp.Height);
            System.Drawing.Imaging.BitmapData bmpData =
                bmp.LockBits(rect, System.Drawing.Imaging.ImageLockMode.ReadWrite,
                bmp.PixelFormat);

            // Get the address of the first line.
            IntPtr ptr = bmpData.Scan0;

            // Declare an array to hold the bytes of the bitmap.
            int length = Math.Abs(bmpData.Stride) * bmp.Height;
            byte[] colorValues = new byte[length];

            // Copy the RGB values into the array.
            System.Runtime.InteropServices.Marshal.Copy(ptr, colorValues, 0, length);

            // Unlock the bits.
            bmp.UnlockBits(bmpData);
            return new BitmapColorValues(colorValues);

        }
   
    }

    public class BitmapColorValues
    {
        public BitmapColorValues(byte[] values)
        {
            this.Values = values;
        }

        public byte[] Values { get; }
    }
}
