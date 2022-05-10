using System;
using System.Windows.Forms;
using static Shrimp.Pdf.icms2.Core;
namespace Shrimp.Pdf.icms2.Background.Win.Runner.Winform
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void Exit_Click(object sender, EventArgs e)
        {
            //StopExpressServer();

            System.Windows.Forms.Application.Exit();
        }

        //private void StopExpressServer()
        //{
        //    var path = System.IO.Path.Combine(ExpressConfig.nodeExpressBuildDir, ExpressConfig.nodeExpressAppFileName);
        //    var parameters = new[]
        //    {
        //        "stop",
        //        path
        //    };

        //    var parameters2 = string.Join(" ", parameters);
        //    consoleControl1.StartProcess(ExpressConfig.foreverExePath, parameters2);
        //}

        private void Form1_Load(object sender, EventArgs e)
        {
            //var parameters = new[]
            //{
            //    "start",
            //    "--sourceDir",
            //    ExpressConfig.nodeExpressBuildDir,
            //    ExpressConfig.nodeExpressAppFileName,

            //};

            //var parameters2 = string.Join(" ", parameters);
            //consoleControl1.StartProcess(ExpressConfig.foreverExePath, parameters2);
        }
    }
}
