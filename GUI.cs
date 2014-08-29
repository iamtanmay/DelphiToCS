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
    struct ReferenceStruct
    {
        public string name;
        public List<string> globals, locals;
    }

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

        public string Indent(int isize)
        {
            return new string(' ', isize);
        }

        private void AnalyzeFolder(string iPath, string iOutPath, ref List<ReferenceStruct> oreferences, ref List<Delphi> odelphi)
        {
            string FolderPath = iPath;
            string tdirectory = "";

            List<string> globals = new List<string>(), locals = new List<string>();
            List<string> global_names = new List<string>(), local_names = new List<string>();

            //Get files in folder
            string[] files = Directory.GetFiles(iPath);
            string[] directories = Directory.GetDirectories(iPath);
            string tstring = "";
            bool pasFileFound = false;

            //Filter for files to convert
            for (int i = 0; i < files.GetLength(0); i++)
            {
                tstring = files[i];
                string[] tstrarray = tstring.Split('.');

                switch (tstrarray[1])
                {
                    //Parse VCL file (The dialog is manually recreated as WinForm, so nothing is done here)
                    case "dfm": break;

                    //Parse unit
                    case "pas": pasFileFound = true;  
                                odelphi.Add(Translate.DelphiToCS(tstring, iOutPath, Log, ref global_names, ref globals, ref local_names, ref locals)); 
                                break;

                    //Parse Project files
                    case "dpk": Translate.DPK2Vcproj(tstring); break;
                    case "dproj": Translate.Dproj2Vcproj(tstring); break;

                    default: break;
                }
            }

            
            if (pasFileFound)
            {
                string[] tfilename_elements = tstring.Split('\\');
                string tfilename = tfilename_elements[tfilename_elements.GetLength(0) - 1];
                tdirectory = tfilename_elements[tfilename_elements.GetLength(0) - 2].Replace(" ", "_");

                //Create Global and Local classes file
                List<string> globalsFile = new List<string>();

                globalsFile.Add("using " + "System" + ";");
                globalsFile.Add("using " + "System.Windows" + ";");
                globalsFile.Add("using " + "System.String" + ";");
                globalsFile.Add("using " + "System.Collections.Generic" + ";");
                globalsFile.Add("");

                globalsFile.Add("namespace " + tdirectory);
                globalsFile.Add("{");
                globalsFile.Add(Indent(4) + "public class " + tdirectory + "_Globals");
                globalsFile.Add(Indent(4) + "{");

                globalsFile.AddRange(globals);
                globalsFile.Add(Indent(4) + "}");

                globalsFile.Add(Indent(4) + "public class " + tdirectory + "_Locals");
                globalsFile.Add(Indent(4) + "{");
                globalsFile.AddRange(locals);
                globalsFile.Add(Indent(4) + "}");
                globalsFile.Add("}");

                File.WriteAllLines(iOutPath + "\\" + "NamespaceGlobals.cs", globalsFile, Encoding.UTF8);

                //Save Global and Local element names
                ReferenceStruct treference = new ReferenceStruct();
                treference.name = tdirectory;
                treference.globals = new List<string>();
                treference.locals = new List<string>();
                treference.globals = global_names;
                treference.locals = local_names;
                oreferences.Add(treference);

                //String replace the local globals used across all files in the directory
                files = Directory.GetFiles(iPath);

                for (int i = 0; i < files.GetLength(0); i++)
                {
                    tstring = files[i];
                    string[] tstrarray = tstring.Split('.');

                    switch (tstrarray[1])
                    {
                        //String replace in *.cs files
                        case "cs":
                            List<string> ttext = Utilities.TextFileReader(tstring);
                            Translate.GlobalsRename(tdirectory + "_Globals", ref ttext, ref global_names);
                            File.WriteAllLines(tstring, ttext, Encoding.UTF8);
                            break;

                        default: break;
                    }
                }
            }

            for (int i = 0; i < directories.GetLength(0); i++)
            {
                string[] tpath_elements = directories[i].Split('\\');
                AnalyzeFolder(directories[i], iOutPath + "\\" + tpath_elements[tpath_elements.GetLength(0) - 1], ref oreferences, ref odelphi);
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