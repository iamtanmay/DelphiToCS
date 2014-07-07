using System;
using System.IO;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Threading;
using System.Windows.Forms;
using System.Collections.ObjectModel;

namespace Translator
{
    public partial class s : Form
    {
        private int index;
        private ObservableCollection<LogEntry> LogEntries { get; set; }

        public s()
        {
            InitializeComponent();
        }

        public delegate void LogDelegate(string imessage);

        //Logging callback
        public void Log(string imessage)
        {
            //Dispatcher is needed because Threads cannot change Main UI data. 
            //Dispatcher transfers data to main thread to apply to UI
            Dispatcher.CurrentDispatcher.BeginInvoke((Action)(() => LogEntries.Add(new LogEntry(DateTime.Now, index++, imessage))));
        } 

        private void GUI_Load(object sender, EventArgs e)
        {
        }

        private void AnalyzeFolder(string iPath, string ioutpath)
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
                    case "pas": Translate.DelphiToCS(tstring, ioutpath, Log); break;

                    //Parse Project files
                    case "dpk": Translate.DPK2Vcproj(tstring); break;
                    case "dproj": Translate.Dproj2Vcproj(tstring); break;

                    default: break;
                }
            }

            for (int i = 0; i < directories.GetLength(0); i++)
            {
                string[] tpath_elements = directories[i].Split('\\');
                AnalyzeFolder(directories[i], ioutpath + "\\" + tpath_elements[tpath_elements.GetLength(0) - 1]);
            }
        }

        private void BtnSource_Click(object sender, EventArgs e)
        {
            //Get Folder
            DialogResult result = folderBrowserDialog1.ShowDialog();

            string tstring = folderBrowserDialog1.SelectedPath;

            if (result == DialogResult.OK)
                BoxSource.Text = tstring;
        }

        private void BtnDest_Click(object sender, EventArgs e)
        {
            //Get Folder
            DialogResult result = folderBrowserDialog1.ShowDialog();

            string tstring = folderBrowserDialog1.SelectedPath;

            if (result == DialogResult.OK)
                BoxDest.Text = tstring;
        }

        private void BtnRun_Click(object sender, EventArgs e)
        {
            AnalyzeFolder(BoxSource.Text, BoxDest.Text);
        }        
    }
}