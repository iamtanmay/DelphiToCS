using System;
using System.IO;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace DelphiToCSTranslator
{
    public partial class GUI : Form
    {
        public GUI()
        {
            InitializeComponent();
        }

        private void GUI_Load(object sender, EventArgs e)
        {
            //Get Folder
            DialogResult result = folderBrowserDialog1.ShowDialog();
            if (result == DialogResult.OK)
            {
                AnalyzeFolder(folderBrowserDialog1.SelectedPath);
            }
        }

        private void AnalyzeFolder(string iPath)
        {
            string FolderPath = iPath;

            //Get files in folder
            string[] files = Directory.GetFiles(iPath);
            string[] directories = Directory.GetDirectories(iPath);

            //Filter for files to convert
            for (int i = 0; i < files.GetLength(0); i++)
            {
                string tstring = files[i];
                string[] tstrarray = tstring.Split('.');

                switch (tstrarray[1])
                {
                    //Parse VCL file (The dialog is manually recreated as WinForm, so nothing is done here)
                    case "dfm": break;

                    //Parse unit
                    case "pas": Translate.DelphiToCS(tstring); break;

                    //Parse Project files
                    case "dpk": Translate.DPK2Vcproj(tstring); break;
                    case "dproj": Translate.Dproj2Vcproj(tstring); break;

                    default: break;
                }
            }

            for (int i = 0; i < directories.GetLength(0); i++)
            {
                AnalyzeFolder(iPath + "\\" + directories[i]);
            }
        }
    }
}
