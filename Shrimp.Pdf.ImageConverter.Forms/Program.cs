using System;
using System.Windows.Forms;
using Shrimp.Pdf.ImageConverter.Core;
using Shrimp.Pdf.ImageConverter.Server;
namespace Shrimp.LiteDB.Forms
{
    static class Program
    {
        /// <summary>
        ///  The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            RemoteServer.createServer();
            Application.SetHighDpiMode(HighDpiMode.SystemAware);
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new Form1());
        }
    }
}
