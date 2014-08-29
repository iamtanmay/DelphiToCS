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
        private ObservableCollection<LogEntry> LogEntries { get; set; }

        List<string> standardDelphiReferences;
        DelphiToCSConversion delphiToCSConversion;

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
            standardDelphiReferences = new List<string> { "SysUtils", "System", "System.Generics.Collections", "Windows", "Forms" };
            richTextBox1.Text = standardDelphiReferences.ToString();
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
            //standardDelphiReferences.AddRange( = richTextBox1.Text.Split(' ');
            delphiToCSConversion = new DelphiToCSConversion(BoxSource.Text, BoxDest.Text, Log, standardDelphiReferences);
        }

        private void label1_Click(object sender, EventArgs e)
        {

        }        
    }
}