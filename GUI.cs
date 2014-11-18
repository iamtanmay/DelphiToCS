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
    public struct ReferenceStruct
    {
        public string name;
        public List<string> globals, locals;
    }

    public partial class s : Form
    {
        private int index;
        public string logtext = "";
        private List<LogEntry> LogEntries { get; set; }
        public bool readIL = false, writeIL = false, genProj = false;

        List<string> standardDelphiReferences = new List<string>();
        List<string> standardCSReferences = new List<string>();
        List<string> standardReferences = new List<string>();

        DelphiToCSConversion delphiToCSConversion;

        public s()
        {
            InitializeComponent();
            LogEntries = new List<LogEntry>();
        }

        public delegate void LogDelegate(string imessage);

        //Logging callback
        public void Log(string imessage)
        {
            //Dispatcher is needed because Threads cannot change Main UI data. 
            //Dispatcher transfers data to main thread to apply to UI
            //LogEntry tlogEntry = new LogEntry(index++, imessage);

            listBox1.Text += DateTime.Now + Indent(4) + imessage + Environment.NewLine;
            //if(Thread.CurrentThread.IsThreadPoolThread)
            //{
            //    listBox1.Text = logtext;
            //    logtext = "";
            //}
            //Dispatcher.CurrentDispatcher.BeginInvoke((Action)(() => LogEntries.Add(tlogEntry)));
        }

        public string Indent(int isize)
        {
            return new string(' ', isize);
        }

        private void GUI_Load(object sender, EventArgs e)
        {
            standardDelphiReferences = new List<string> { "+DelphiStandardWrapper", "-Example_DelphiLibrary_To_Remove","Generics.Collections / System.Collections.Generic", "SysUtils / String System", "System / System", "System.Generics.Collections / System.Collections.Generic", "Windows / System.Windows", "Forms / System.Windows.Forms" };
            
            //Load last used settings
            if (DelphiToCSTranslator.Properties.Settings.Default.InPath != "")
                BoxSource.Text = DelphiToCSTranslator.Properties.Settings.Default.InPath;

            if (DelphiToCSTranslator.Properties.Settings.Default.OutPath != "")
                BoxDest.Text = DelphiToCSTranslator.Properties.Settings.Default.OutPath;

            if (DelphiToCSTranslator.Properties.Settings.Default.PatchPath != "")
                BoxPatch.Text = DelphiToCSTranslator.Properties.Settings.Default.PatchPath;

            if (DelphiToCSTranslator.Properties.Settings.Default.StdLibraries != "")
                richTextBox1.Text = DelphiToCSTranslator.Properties.Settings.Default.StdLibraries;
            else
                standardDelphiReferences.ForEach(s => {
                    richTextBox1.Text += s;
                    richTextBox1.Text += Environment.NewLine;
                });
        }

        private void BtnSource_Click(object sender, EventArgs e)
        {
            //Get Folder
            DialogResult result = folderBrowserDialog1.ShowDialog();

            string tstring = folderBrowserDialog1.SelectedPath;

            if (result == DialogResult.OK)
            {
                BoxSource.Text = tstring;
                BoxPatch.Text = System.IO.Directory.GetParent(tstring) + "\\Patch";
                BoxOverride.Text = System.IO.Directory.GetParent(tstring) + "\\Override"; ;
                BoxDest.Text = System.IO.Directory.GetParent(tstring) + "\\Output"; ;
                BoxIL.Text = System.IO.Directory.GetParent(tstring) + "\\IL"; ;
            }
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
            int tmaxthreads = 1;
            int tboxthreads = int.Parse(BoxThreads.Text);

            readIL = BoxReadIL.Checked;
            writeIL = BoxWriteIL.Checked;
            genProj = BoxGenerateProjects.Checked;

            if (tboxthreads > tmaxthreads)
                tmaxthreads = tboxthreads;

            //Save last used settings
            if (BoxSource.Text != "")
                DelphiToCSTranslator.Properties.Settings.Default.InPath = BoxSource.Text;
            if (BoxDest.Text != "")
                DelphiToCSTranslator.Properties.Settings.Default.OutPath = BoxDest.Text;
            if (BoxPatch.Text != "")
                DelphiToCSTranslator.Properties.Settings.Default.PatchPath = BoxPatch.Text;
            if (BoxOverride.Text != "")
                DelphiToCSTranslator.Properties.Settings.Default.OverridePath = BoxOverride.Text;
            if (BoxIL.Text != "")
                DelphiToCSTranslator.Properties.Settings.Default.ILPath = BoxIL.Text;
            if (richTextBox1.Text != "")
                DelphiToCSTranslator.Properties.Settings.Default.StdLibraries = richTextBox1.Text;

            string[] tstrArr = richTextBox1.Text.Split(Environment.NewLine.ToCharArray());
            standardDelphiReferences = new List<string>();
            standardCSReferences = new List<string>();

            //Get a list of Standard Delphi libraries, and their CS substitutes
            for (int i = 0; i < tstrArr.Length; i++ )
            {
                string[] tarr = tstrArr[i].Split("//".ToCharArray());
                if (tarr.Length == 2 && tarr[0] != "")
                {
                    standardDelphiReferences.Add(tarr[0]);
                    standardCSReferences.Add(tarr[1]);
                }
                else if (tarr.Length == 1 && tarr[0] != "")
                {
                    if (tarr[0].IndexOf('-') != -1)
                    {
                        standardDelphiReferences.Add(tarr[0].Replace("-",""));
                        standardCSReferences.Add("");
                    }
                    else if (tarr[0].IndexOf('+') != -1)
                    {
                        standardReferences.Add(tarr[0].Replace("+",""));
                    }
                    else
                    {
                        //Unknown ??
                    }
                }
                else
                {

                }
            }

            List<List<string>> tStandardCSReferences = new List<List<string>>();

            //Organize the replacement references from Delphi to CS
            for (int i = 0; i < standardCSReferences.Count; i++)
            {               
                List<string> tlist = new List<string>();
                string[] tarr = standardCSReferences[i].Split(' ');

                for (int j = 0; j < tarr.Length; j++)
                {
                    if (tarr[j] != "")
                       tlist.Add(tarr[j]);
                }
                tStandardCSReferences.Add(tlist);
            }

            delphiToCSConversion = new DelphiToCSConversion(BoxSource.Text, BoxDest.Text, BoxPatch.Text, BoxOverride.Text, BoxIL.Text, Log, ref standardReferences, ref standardDelphiReferences, ref tStandardCSReferences, tmaxthreads, this, BoxThreadingEnabled.Checked);
        }
        
        private void BtnOverride_Click(object sender, EventArgs e)
        {
            //Get Folder
            DialogResult result = folderBrowserDialog1.ShowDialog();

            string tstring = folderBrowserDialog1.SelectedPath;

            if (result == DialogResult.OK)
                BoxOverride.Text = tstring;
        }

        private void BtnPatch_Click(object sender, EventArgs e)
        {
            //Get Folder
            DialogResult result = folderBrowserDialog1.ShowDialog();

            string tstring = folderBrowserDialog1.SelectedPath;

            if (result == DialogResult.OK)
                BoxPatch.Text = tstring;
        }

        private void label1_Click(object sender, EventArgs e)
        {

        }

        private void label2_Click(object sender, EventArgs e)
        {

        }

        private void label3_Click(object sender, EventArgs e)
        {

        }
    }
}