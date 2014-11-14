using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Xml.Serialization;
using System.Xml;

namespace Translator
{
    public class DelphiToCSConversion
    {
        public int maxthreads = 1;
        public bool threadingEnabled = false;
        public List<Translate> delphiParsedFiles;
        public List<ReferenceStruct> delphiReferences;
        public List<string> delphiStandardReferences;
        public List<List<string>> standardCSReferences;
        public LogDelegate Log;

        public DelphiToCSConversion(string iPath, string iOutPath, string iPatchPath, string iOverridePath, string iILPath, LogDelegate ilog, ref List<string> iStandardReferences, ref List<string> idelphiStandardReferences, ref List<List<string>> istandardCSReferences, int imaxthreads, s iform, bool ithreadingEnabled)
        {
            threadingEnabled = ithreadingEnabled;
            delphiParsedFiles = new List<Translate>();
            delphiReferences = new List<ReferenceStruct>();
            delphiStandardReferences = new List<string>();
            standardCSReferences = new List<List<string>>();

            delphiStandardReferences = idelphiStandardReferences;
            standardCSReferences = istandardCSReferences;

            Log = ilog;

            maxthreads = imaxthreads;
            //Do a first run to convert individual files to C#, and gather list of references
            AnalyzeFolder(iPath, iOutPath, iPatchPath, iOverridePath, iILPath, ref delphiReferences, ref delphiParsedFiles, ref iStandardReferences, ref delphiStandardReferences, ref standardCSReferences, iform);

            //Replace global strings from references in files
            ResolveReferences(ref delphiParsedFiles, ref delphiReferences);
        }

        private void ResolveReferences( ref List<Translate> iDelphi, ref List<ReferenceStruct> iReferences)
        {
            //Loop through all files and check their references
            for (int i=0; i < iDelphi.Count; i++)
            {                
                //Loop through all references in this file
                for (int j = 0; j < iDelphi[i].IL.script.includes.Count; j++)
                {
                    //If it is a reference to a project in the solution
                    if (isSolutionReference(iDelphi[i].IL.script.includes[j]))
                        ReplaceReferenceGlobalStringsInFile(iDelphi[i].IL.outPath, iDelphi[i].IL.script.includes[j], ref iReferences);
                }
            }
        }

        public bool isSolutionReference(string iReference)
        {
            for (int i = 0; i < delphiStandardReferences.Count; i++)
            {
                if (delphiStandardReferences[i] == iReference)
                    return false;
            }
            return true;
        }

        public void ReplaceReferenceGlobalStringsInFile(string iFilepath, string iReferenceName, ref List<ReferenceStruct> iReferences)
        {
            List<string> itext = Utilities.TextFileReader(iFilepath);
            List<string> globals = new List<string>();

            for (int i = 0; i < iReferences.Count; i++)
            {
                if (iReferences[i].name == iReferenceName)
                {
                    Translate.GlobalsRename(iReferenceName + "_Globals.", ref itext, iReferences[i].globals);
                }
            }
            File.WriteAllLines(iFilepath, itext, Encoding.UTF8);
        }

        public string Indent(int iSize)
        {
            return new string(' ', iSize);
        }

        private void AnalyzeFolder(string iPath, string iOutPath, string iPatchPath, string iOverridePath, string iILPath, ref List<ReferenceStruct> oReferences, ref List<Translate> oDelphi, ref List<string> iStandardReferences, ref List<string> iDelphiStandardReferences, ref List<List<string>> iStandardCSReferences, s iform)
        {
            string FolderPath = iPath;
            string tdirectory = "";

            List<string> globals = new List<string>(), locals = new List<string>();
            List<string> global_names = new List<string>(), local_names = new List<string>();

            //Get files in folder
            string[] files = Directory.GetFiles(iPath);
            
            string[] patchfiles = new string[0];

            if (iPatchPath != "")
                patchfiles = Directory.GetFiles(iPatchPath);

            string[] overridefiles = new string[0];
            if (iOverridePath != "") 
                overridefiles = Directory.GetFiles(iOverridePath);

            for (int i = 0; i < patchfiles.GetLength(0); i++)
                patchfiles[i] = patchfiles[i].Split('.')[0];

            for (int i = 0; i < patchfiles.GetLength(0); i++)
                overridefiles[i] = overridefiles[i].Split('.')[0];

            string[] directories = new string[0];

            if (iPath != "")
                directories = Directory.GetDirectories(iPath);

            string[] patchdirectories = new string[0];

            if (iPatchPath != "") 
                patchdirectories = Directory.GetDirectories(iPatchPath);

            string[] overridedirectories = new string[0];

            if (iOverridePath != "")
                overridedirectories  = Directory.GetDirectories(iOverridePath); 
            
            string tstring = "";
            bool pasFileFound = false;

            string[] tfilename_elements = iPath.Split('\\');
            //string tfilename = tfilename_elements[tfilename_elements.GetLength(0) - 1];
            tdirectory = tfilename_elements[tfilename_elements.GetLength(0) - 1];//.Replace(" ", "_");

            //Filter for files to convert
            for (int i = 0; i < files.GetLength(0); i++)
            {
                tstring = files[i];
                string[] tstrarray = tstring.Split('.');
                string tfilename = tstrarray[0];
                string tfileextension = tstrarray[1];

                tstrarray = tfilename.Split("\\".ToCharArray());
                tfilename = tstrarray[tstrarray.Length - 1];

                string tpatchfile = "", toverridefile = "";

                for (int j = 0; j < patchfiles.Length; j++)
                {
                    patchfiles[j] = patchfiles[j].Replace(".txt", "");
                    tstrarray = patchfiles[j].Split("\\".ToCharArray());
                    patchfiles[j] = tstrarray[tstrarray.Length - 1];

                    if (patchfiles[j] == tfilename)
                        tpatchfile = patchfiles[j];
                }

                for (int j = 0; j < overridefiles.Length; j++)
                {
                    overridefiles[j] = overridefiles[j].Replace(".cs", "");
                    tstrarray = overridefiles[j].Split("\\".ToCharArray());
                    overridefiles[j] = tstrarray[tstrarray.Length - 1];

                    if (overridefiles[j] == tfilename)
                        toverridefile = overridefiles[j];
                }

                switch (tfileextension)
                {
                    //Parse VCL file (The dialog is manually recreated as WinForm, so nothing is done here)
                    case "dfm": break;

                    //Parse unit
                    case "pas": pasFileFound = true;
                            Translate ttranslate; 
                            
                            if (iform.writeIL)
                                ttranslate = new Translate(tstring, tdirectory, iILPath, iPatchPath, tpatchfile, iOverridePath, toverridefile, Log, iStandardReferences, iDelphiStandardReferences, standardCSReferences, Log, ref iform);
                            else
                                ttranslate = new Translate(tstring, tdirectory, iOutPath, iPatchPath, tpatchfile, iOverridePath, toverridefile, Log, iStandardReferences, iDelphiStandardReferences, standardCSReferences, Log, ref iform);

                            oDelphi.Add(ttranslate);                        
                            break;

                    //Parse Project files
                    case "dpk": //Translate.DPK2Vcproj(tstring); 
                        break;

                    case "dproj": //Translate.Dproj2Vcproj(tstring); 
                        break;

                    default: break;
                }
            }

            if (threadingEnabled)
                Parallel.ForEach(oDelphi, new ParallelOptions { MaxDegreeOfParallelism = maxthreads }, currentFile =>
                {
                    Object tobject = null;
                    currentFile.ReadDelphi(tobject);
                });
            else
                for (int i = 0; i < oDelphi.Count; i++)
                {
                    Object tobject = null;
                    oDelphi[i].ReadDelphi(tobject);
                }

            if (pasFileFound)
            {
                //Create Global and Local classes file
                List<string> globalsFile = new List<string>();
                string tnamespace = tdirectory.Replace(" ", "_");
                globalsFile.Add("using " + "System" + ";");
                globalsFile.Add("using " + "System.Windows" + ";");
                globalsFile.Add("using " + "System.String" + ";");
                globalsFile.Add("using " + "System.Collections.Generic" + ";");
                globalsFile.Add("");

                globalsFile.Add("namespace " + tnamespace);
                globalsFile.Add("{");
                globalsFile.Add(Indent(4) + "public class " + tnamespace + "_Globals");
                globalsFile.Add(Indent(4) + "{");

                globalsFile.AddRange(globals);
                globalsFile.Add(Indent(4) + "}");

                globalsFile.Add(Indent(4) + "public class " + tnamespace + "_Locals");
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
                oReferences.Add(treference);

                ////String replace the local globals used across all files in the directory
                //files = Directory.GetFiles(iPath);

                //for (int i = 0; i < files.GetLength(0); i++)
                //{
                //    tstring = files[i];
                //    string[] tstrarray = tstring.Split('.');

                //    switch (tstrarray[1])
                //    {
                //        //String replace in *.cs files
                //        case "cs":
                //            List<string> ttext = Utilities.TextFileReader(tstring);
                //            Translate.GlobalsRename(tdirectory + "_Locals.", ref ttext, local_names);
                //            File.WriteAllLines(tstring, ttext, Encoding.UTF8);
                //            break;

                //        default: break;
                //    }
                //}
            }

            for (int i = 0; i < directories.GetLength(0); i++)
            {
                string[] tpath_elements = directories[i].Split('\\');

                string[] tpatchpath_elements = new string[0];
                string tcurr_patch_path = "";

                if (patchdirectories.Length > 0)
                {
                    tpatchpath_elements = patchdirectories[i].Split('\\');
                    tcurr_patch_path = iPatchPath + "\\" + tpatchpath_elements[tpatchpath_elements.GetLength(0) - 1];
                }

                string[] toverridepath_elements = new string[0];
                string tcurr_override_path = "";

                if (overridedirectories.Length > 0)
                {
                    toverridepath_elements = overridedirectories[i].Split('\\');
                    tcurr_override_path = iOverridePath + "\\" + toverridepath_elements[toverridepath_elements.GetLength(0) - 1];
                }

                AnalyzeFolder(directories[i], iOutPath + "\\" + tpath_elements[tpath_elements.GetLength(0) - 1], tcurr_patch_path, tcurr_override_path, iILPath + "\\" + tpath_elements[tpath_elements.GetLength(0) - 1], ref oReferences, ref oDelphi, ref iStandardReferences, ref iDelphiStandardReferences, ref standardCSReferences, iform);
            }
        }
    }

    public class Translate
    {
        public Delphi IL;
        public CSharp CS;
        public string filename;
        public string iPath, idirectory, iOutPath, iPatchPath, iPatchFile, iOverridePath, iOverrideFile;
        public LogDelegate ilog;
        public List<string> oglobal_names, oglobals, olocal_names, olocals, iStandardReferences, iDelphiStandardReferences;
        public List<List<string>> iStandardCSReferences;
        public LogDelegate iLog;
        public s form;

        //public Translate()
        //{ 
        //}

        public Translate(string tPath, string tdirectory, string tOutPath, string tPatchPath, string tPatchFile, string tOverridePath, string tOverrideFile, LogDelegate tlog, List<string> tStandardReferences, List<string> tDelphiStandardReferences, List<List<string>> tStandardCSReferences, LogDelegate tLog, ref s iform)
        {
            form = iform;
            iPath = tPath;
            idirectory = tdirectory; 
            iOutPath = tOutPath; 
            iPatchPath = tPatchPath; 
            iPatchFile = tPatchFile; 
            iOverridePath = tOverridePath; 
            iOverrideFile = tOverrideFile; 
            ilog = tlog;
            oglobal_names = new List<string>();//tglobal_names; 
            oglobals = new List<string>(); //tglobals; 
            olocal_names = new List<string>(); //tlocal_names; 
            olocals = new List<string>(); //tlocals; 
            iStandardReferences = tStandardReferences; 
            iDelphiStandardReferences = tDelphiStandardReferences; 
            iStandardCSReferences = tStandardCSReferences; 
            iLog = tLog;
        }

        public void ReadDelphi(Object threadContext)
        {
            Delphi tdelphi = new Delphi();
            List<string> tdelphitext = Utilities.TextFileReader(iPath);
            string[] tpath_elements = iOutPath.Split('\\');
            string[] tfilename_elements = iPath.Split('\\');
            string tfilename = tfilename_elements[tfilename_elements.GetLength(0) - 1];
            tfilename = tfilename.Replace(".pas", "");
            filename = tfilename;
            //string tdirectory = tfilename_elements[tfilename_elements.GetLength(0) - 2].Replace(" ", "_");
            //Embed path information
            tdelphi.directory = idirectory;
            tdelphi.logsingle = iLog;
            tdelphi.outPath = iOutPath + "\\" + tfilename + ".cs";

            if (iOverrideFile != "")
            {
                Directory.CreateDirectory(iOutPath);
                File.Copy(iOverridePath + "\\" + iOverrideFile + ".cs", iOutPath + "\\" + tfilename + ".cs", true);
            }
            else
            {   
                tdelphi.Read(ref tdelphitext, true, "{ --", "-- }", ilog, ref form);
                //Write to file
                Directory.CreateDirectory(iOutPath);

                XmlSerializer x;
                
                x = new XmlSerializer(typeof(Translator.Delphi));

                using (var writer = XmlWriter.Create(iOutPath + "\\" + filename + ".xml"))
                {
                    x.Serialize(writer, tdelphi);
                }

                //File.WriteAllLines(iOutPath + "\\" + filename + ".txt", tout, Encoding.UTF8);
            }
            IL = tdelphi;
        }

        public void WriteCS(Object threadContext)
        {
            CS = new CSharp();
            CS.file_path = IL.outPath;
            CS.standard_references = iStandardReferences;

            string[] tout = CS.Write(ref IL.script, idirectory.Replace(" ", "_"), ref oglobal_names, ref oglobals, ref olocal_names, ref olocals, ref iDelphiStandardReferences, ref iStandardCSReferences).ToArray();

            //Write to file
            Directory.CreateDirectory(iOutPath);
            File.WriteAllLines(iOutPath + "\\" + filename + ".cs", tout, Encoding.UTF8);
        }

        public void DPK2Vcproj(string iPath)
        {
        }

        public void Dproj2Vcproj(string iPath)
        {
        }

        //Append a string to a list of words if found in text
        public static void GlobalsRename(string iappendName, ref List<string> itext, List<string> isearchWords)
        {
            for (int i = 0; i < itext.Count; i++)
                for (int j = 0; j < isearchWords.Count; j++)
                    itext[i] = itext[i].Replace(isearchWords[j], iappendName + isearchWords[j]);
        }
    }
}
