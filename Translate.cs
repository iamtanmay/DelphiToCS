using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Translator
{
    public class DelphiToCSConversion
    {
        public List<Delphi> delphiParsedFiles;
        public List<ReferenceStruct> delphiReferences;
        public List<string> delphiStandardReferences;
        public List<List<string>> standardCSReferences;
        public LogDelegate Log;

        //TestComment
        //Testcomment2

        public DelphiToCSConversion(string iPath, string iOutPath, LogDelegate ilog, ref List<string> idelphiStandardReferences, ref List<List<string>> istandardCSReferences)
        {
            delphiParsedFiles = new List<Delphi>();
            delphiReferences = new List<ReferenceStruct>();
            delphiStandardReferences = new List<string>();
            standardCSReferences = new List<List<string>>();

            delphiStandardReferences = idelphiStandardReferences;
            standardCSReferences = istandardCSReferences;

            Log = ilog;

            //Do a first run to convert individual files to C#, and gather list of references
            AnalyzeFolder(iPath, iOutPath, ref delphiReferences, ref delphiParsedFiles, ref delphiStandardReferences, ref standardCSReferences);

            //Replace global strings from references in files
            ResolveReferences(ref delphiParsedFiles, ref delphiReferences);
        }

        private void ResolveReferences( ref List<Delphi> iDelphi, ref List<ReferenceStruct> iReferences)
        {
            //Loop through all files and check their references
            for (int i=0; i < iDelphi.Count; i++)
            {                
                //Loop through all references in this file
                for (int j = 0; j < iDelphi[i].script.includes.Count; j++)
                {
                    //If it is a reference to a project in the solution
                    if (isSolutionReference(iDelphi[i].script.includes[j]))
                        ReplaceReferenceGlobalStringsInFile(iDelphi[i].outPath, iDelphi[i].script.includes[j], ref iReferences);
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

        private void AnalyzeFolder(string iPath, string iOutPath, ref List<ReferenceStruct> oReferences, ref List<Delphi> oDelphi, ref List<string> iStandardReferences, ref List<List<string>> iStandardCSReferences)
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

            string[] tfilename_elements = iPath.Split('\\');
            //string tfilename = tfilename_elements[tfilename_elements.GetLength(0) - 1];
            tdirectory = tfilename_elements[tfilename_elements.GetLength(0) - 1];//.Replace(" ", "_");

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
                        oDelphi.Add(Translate.DelphiToCS(tstring, tdirectory, iOutPath, Log, ref global_names, ref globals, ref local_names, ref locals, ref iStandardReferences, ref standardCSReferences, Log));
                        break;

                    //Parse Project files
                    case "dpk": Translate.DPK2Vcproj(tstring); break;
                    case "dproj": Translate.Dproj2Vcproj(tstring); break;

                    default: break;
                }
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
                AnalyzeFolder(directories[i], iOutPath + "\\" + tpath_elements[tpath_elements.GetLength(0) - 1], ref oReferences, ref oDelphi, ref iStandardReferences, ref standardCSReferences);
            }
        }
    }

    struct Translate
    {
        public static Delphi DelphiToCS(string iPath, string idirectory, string iOutPath, LogDelegate ilog, ref List<string> oglobal_names, ref List<string> oglobals, ref List<string> olocal_names, ref List<string> olocals, ref List<string> iStandardReferences, ref List<List<string>> iStandardCSReferences, LogDelegate iLog)
        {
            List<string> tdelphitext = Utilities.TextFileReader(iPath);
            string[] tpath_elements = iOutPath.Split('\\');
            string[] tfilename_elements = iPath.Split('\\');

            string tfilename = tfilename_elements[tfilename_elements.GetLength(0) - 1];
            //string tdirectory = tfilename_elements[tfilename_elements.GetLength(0) - 2].Replace(" ", "_");
            tfilename = tfilename.Replace(".pas", "");

            Delphi tdelphi= new Delphi();

            //Embed path information
            tdelphi.directory = idirectory;
            tdelphi.logsingle = iLog;
            tdelphi.outPath = iOutPath + "\\" + tfilename + ".cs";

            tdelphi.Read(ref tdelphitext, true, "{ --", "-- }", ilog);

            CSharp tcsharp = new CSharp();
            string[] tout = tcsharp.Write(ref tdelphi.script, idirectory.Replace(" ", "_"), ref oglobal_names, ref oglobals, ref olocal_names, ref olocals, ref iStandardReferences, ref iStandardCSReferences).ToArray();

            //Write to file
            File.WriteAllLines(iOutPath + "\\" + tfilename + ".cs", tout, Encoding.UTF8);

            return tdelphi;
        }

        public static void DPK2Vcproj(string iPath)
        {
        }

        public static void Dproj2Vcproj(string iPath)
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
