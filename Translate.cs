using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Xml.Serialization;
using System.Text.RegularExpressions;
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

        public Dictionary<string, string> projPaths = new Dictionary<string, string>();
        public Dictionary<string, string> projGUIDs = new Dictionary<string, string>();
        public List<List<string>> projRefs = new List<List<string>>();
        public Dictionary<string, XmlDocument> projects = new Dictionary<string, XmlDocument>();

        //The position to insert the references is hardcoded in the template
        int CSProj_XMLTemplate_IncludeChildRank = 5;

        //Source folder
        public string source_path;

        public DelphiToCSConversion(string iPath, string iOutPath, string iPatchPath, string iOverridePath, string iILPath, string iGroupProjPath, LogDelegate ilog, ref List<string> iStandardReferences, ref List<string> idelphiStandardReferences, ref List<List<string>> istandardCSReferences, ref List<string> idelphiIgnoreReferences, int imaxthreads, s iform, bool ithreadingEnabled)
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
            source_path = iPath;

            //Cache all the relative Project paths
            //Read raw XML
            StreamReader tdprojstream = new StreamReader(iGroupProjPath);
            //Add <xml> tag
            string tline = "<?xml version=\"1.0\" encoding=\"utf-8\"?>" + tdprojstream.ReadToEnd();

            //Read as XML
            XmlDocument tcsproj = new XmlDocument();
            tcsproj.LoadXml(tline);

            int tsource_childnodecount = tcsproj.ChildNodes[1].ChildNodes.Count;
            
            //Add default Delphi Units (to be ignored)
            for (int i = 0; i < idelphiIgnoreReferences.Count; i++)
            {
                string tnewGUID = "{" + Guid.NewGuid().ToString() + "}";
                projPaths.Add(idelphiIgnoreReferences[i], "\\..\\" + idelphiIgnoreReferences[i] + "\\");
                projGUIDs.Add(idelphiIgnoreReferences[i], tnewGUID);
                projects.Add(idelphiIgnoreReferences[i], CreateEmptyProj(idelphiIgnoreReferences[i], tnewGUID, iform.csproj));
            }

            //Get all relative project paths
            for (int i = 0; i < tsource_childnodecount; i++)
            {
                XmlNode tnode = tcsproj.ChildNodes[1].ChildNodes[i];
                if (tnode.Name == "ItemGroup")
                {
                    for (int j = 0; j < tnode.ChildNodes.Count; j++)
                    {
                        XmlNode tsubnode = tnode.ChildNodes[j];

                        if (tsubnode.Name == "Projects")
                        {
                            string tprojpath = tsubnode.Attributes["Include"].Value;
                            string[] tprojpatharr = tprojpath.Split('\\');
                            string tprojname = tprojpatharr[tprojpatharr.Length-1].Split('.')[0];
                            List<string> tstrlist = new List<string>(tprojpatharr);

                            tprojpath = "";
                            for (int jj = 0; jj < tstrlist.Count - 1;jj++ ) 
                                tprojpath = tprojpath + '\\' + tstrlist[jj];

                            projPaths.Add(tprojname, tprojpath);
                        }
                    }
                }
            }

            //Convert individual files to C#, and gather list of references
            AnalyzeFolder(iPath, iOutPath, iPatchPath, iOverridePath, iILPath, ref delphiReferences, ref delphiParsedFiles, ref iStandardReferences, ref delphiStandardReferences, ref standardCSReferences, iform);

            //Add project paths and GUIDs to csproj files and write them out
            foreach (KeyValuePair<string, XmlDocument> pair in projects)
            {
                tcsproj = pair.Value;
                int tcompile_childnodecount = tcsproj.ChildNodes[1].ChildNodes[CSProj_XMLTemplate_IncludeChildRank].ChildNodes.Count;
                int treference_childnodecount = tcsproj.ChildNodes[1].ChildNodes[CSProj_XMLTemplate_IncludeChildRank + 1].ChildNodes.Count;

                //Path and GUID of current project
                string tprojectpath, tprojectGUID;

                //Upper case, lower case bpl and library names
                try
                {
                    tprojectpath = projPaths[pair.Key];
                    tprojectGUID = projGUIDs[pair.Key];
                }
                catch
                {
                    tprojectpath = projPaths[pair.Key.ToLower()];
                    tprojectGUID = projGUIDs[pair.Key.ToLower()];
                }

                //Add project path to filename in compile list
                for (int i = 0; i < tcompile_childnodecount; i++)
                {
                    string tstringval = tcsproj.ChildNodes[1].ChildNodes[CSProj_XMLTemplate_IncludeChildRank].ChildNodes[i].Attributes["Include"].Value;
                    tstringval = tprojectpath + '\\' + tstringval;
                    tcsproj.ChildNodes[1].ChildNodes[CSProj_XMLTemplate_IncludeChildRank].ChildNodes[i].Attributes["Include"].Value = tstringval;
                }

                //Add project path and GUID to filename in project references list
                for (int i = 0; i < treference_childnodecount; i++)
                {
                    string tstringval = tcsproj.ChildNodes[1].ChildNodes[CSProj_XMLTemplate_IncludeChildRank + 1].ChildNodes[i].Attributes["Include"].Value;
                    string tref_projectpath, tref_projectGUID;

                    try
                    {
                        tref_projectpath = projPaths[tstringval];
                        tref_projectGUID = projGUIDs[tstringval];
                    }
                    catch
                    {
                        tref_projectpath = projPaths[tstringval.ToLower()];
                        tref_projectGUID = projGUIDs[tstringval.ToLower()];
                    }

                    tref_projectpath = tref_projectpath + '\\' + tstringval + ".csproj";
                    tcsproj.ChildNodes[1].ChildNodes[CSProj_XMLTemplate_IncludeChildRank + 1].ChildNodes[i].ChildNodes[0].InnerText = tref_projectGUID;
                    tcsproj.ChildNodes[1].ChildNodes[CSProj_XMLTemplate_IncludeChildRank + 1].ChildNodes[i].ChildNodes[1].InnerText = tref_projectpath;
                }

                //Write .csproj XMLDocuments to disk
                string toutfolder = iOutPath + tprojectpath.Substring(3);
                string txml_outpath = toutfolder + '\\' + pair.Key + ".csproj";

                //Create directory
                if (!System.IO.Directory.Exists(toutfolder))
                    System.IO.Directory.CreateDirectory(toutfolder);

                //Write out file
                tcsproj.Save(txml_outpath);
            }

            //Write .sln to disk

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
                            
                            //if (iform.writeIL)

                            ttranslate = new Translate(tstring, tdirectory, iILPath, iOutPath, iPatchPath, tpatchfile, iOverridePath, toverridefile, Log, iStandardReferences, iDelphiStandardReferences, standardCSReferences, Log, ref iform);
                            
                            if (toverridefile != "")
                            {
                                Directory.CreateDirectory(iOutPath);
                                File.Copy(iOverridePath + "\\" + toverridefile + ".cs", iOutPath + "\\" + tfilename + ".cs", true);
                            }

                            oDelphi.Add(ttranslate);                        
                            break;

                    //Parse Project files
                    case "dpk": //Translate.DPK2Vcproj(tstring); 
                        break;

                    case "dproj":   if (iform.genProj) //Translate.Dproj2Vcproj(tstring); 
                                    {
                                        ReadProj(tstring, iOutPath, iform.csproj);
                                    }
                        break;

                    default: break;
                }
            }



            if (threadingEnabled)
                Parallel.ForEach(oDelphi, new ParallelOptions { MaxDegreeOfParallelism = maxthreads }, currentFile =>
                {
                    Object tobject = null;

                    if (currentFile.overriden == false)
                    {
                        if (iform.writeIL)
                            currentFile.ReadDelphi(tobject);
                        else
                            currentFile.ReadIL(tobject);

                        if (currentFile.overriden == false)
                            currentFile.WriteCS(tobject);
                    }
                });
            else
                for (int i = 0; i < oDelphi.Count; i++)
                {
                    Object tobject = null;

                    if (oDelphi[i].overriden == false)
                    {
                        if (iform.writeIL)
                            oDelphi[i].ReadDelphi(tobject);
                        else
                            oDelphi[i].ReadIL(tobject);

                        if (oDelphi[i].overriden == false)
                            oDelphi[i].WriteCS(tobject);
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

                //Create directory
                if (!System.IO.Directory.Exists(iOutPath))
                    System.IO.Directory.CreateDirectory(iOutPath);

                File.WriteAllLines(iOutPath + "\\" + "NamespaceGlobals.cs", globalsFile, Encoding.UTF8);

                //Save Global and Local element names
                ReferenceStruct treference = new ReferenceStruct();
                treference.name = tdirectory;
                treference.globals = new List<string>();
                treference.locals = new List<string>();
                treference.globals = global_names;
                treference.locals = local_names;
                oReferences.Add(treference);
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

        public void ReadProj(string ipath, string ioutpath, XmlDocument icsproj)
        {
            //Get output path
            string[] tpath_elements = ioutpath.Split('\\');
            string[] tfilename_elements = ipath.Split('\\');
            string tfilename = tfilename_elements[tfilename_elements.GetLength(0) - 1];
            tfilename = tfilename.Replace(".dproj", "");
            ioutpath = ioutpath + "\\" + tfilename + ".csproj";

            //If Project path is not included, add project path
            string trelativepath = ipath.Replace(source_path, "");
            trelativepath = trelativepath.Replace("\\" + tfilename + ".dproj", "");
            trelativepath = "\\.." + trelativepath;

            if (!projPaths.ContainsKey(tfilename))
                projPaths.Add(tfilename, trelativepath);

            //Read raw XML
            StreamReader tdprojstream = new StreamReader(ipath);
            //Add <xml> tag
            string tline = "<?xml version=\"1.0\" encoding=\"utf-8\"?>" + tdprojstream.ReadToEnd();

            //Read as XML
            XmlDocument tdproj = new XmlDocument();
            tdproj.LoadXml(tline);

            XmlDocument tcsproj = new XmlDocument();
            string txmlstring = "";
            using (var stringWriter = new StringWriter())
            using (var xmlTextWriter = XmlWriter.Create(stringWriter))
            {
                icsproj.WriteTo(xmlTextWriter);
                xmlTextWriter.Flush();
                txmlstring = stringWriter.GetStringBuilder().ToString();
            }
            tcsproj.LoadXml(txmlstring);

            int tsource_childnodecount = tdproj.ChildNodes[1].ChildNodes.Count;
            int tchildnodecount = tcsproj.ChildNodes[1].ChildNodes.Count;

            //Get all includes from .dproj
            List<XmlNode> tincludes = new List<XmlNode>();

            XmlNode tnode = tdproj.ChildNodes[1].ChildNodes[0].ChildNodes[7];
            string tprojtype = "Library";

            try
            {
                tprojtype = tnode.InnerText;
            }
            catch
            {
            }

            //Project type
            if (tprojtype == "Application")
            {
                tprojtype = "WinExe";

                XmlElement tnewnode = tcsproj.CreateElement("ProjectTypeGuids", "http://schemas.microsoft.com/developer/msbuild/2003");
                tnewnode.InnerText = "{60dc8134-eba5-43b8-bcc9-bb4bc16c2548};{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}";
                tcsproj.ChildNodes[1].AppendChild(tnewnode);
            }
            else
                tprojtype = "Library";

            tcsproj.ChildNodes[1].ChildNodes[0].ChildNodes[3].InnerText = tprojtype;

            //Project GUID
            tnode = tdproj.ChildNodes[1].ChildNodes[0].ChildNodes[0];
            tcsproj.ChildNodes[1].ChildNodes[0].ChildNodes[2].InnerText = tnode.InnerText;
            projGUIDs.Add(tfilename, tnode.InnerText);

            //AssemblyName
            tcsproj.ChildNodes[1].ChildNodes[0].ChildNodes[5].InnerText = tfilename;

            for (int i = 0; i < tsource_childnodecount; i++)
            {   
                tnode = tdproj.ChildNodes[1].ChildNodes[i];
                
                if (tnode.Name == "ItemGroup")
                {
                    for (int j = 0; j < tnode.ChildNodes.Count; j++)
                    {
                        XmlNode tsubnode = tnode.ChildNodes[j];

                        //Write all includes to new .csproj
                        if (tsubnode.Name == "DCCReference")
                        {
                            string tinclude = tsubnode.Attributes["Include"].Value;
                            string[] tincludearr = tinclude.Split('.');
                            string tincludeextension = tincludearr[1];
                            if (tincludeextension == "dcp")
                            {
                                XmlElement tnewnode = tcsproj.CreateElement("ProjectReference", "http://schemas.microsoft.com/developer/msbuild/2003");
                                tnewnode.SetAttribute("Include", tincludearr[0]);
                                XmlElement tname = tcsproj.CreateElement("Name", "http://schemas.microsoft.com/developer/msbuild/2003");
                                tname.InnerText = tincludearr[0];
                                XmlElement tproject = tcsproj.CreateElement("Project", "http://schemas.microsoft.com/developer/msbuild/2003");
                                tproject.InnerText = "";
                                tnewnode.AppendChild(tproject);
                                tnewnode.AppendChild(tname);

                                tcsproj.ChildNodes[1].ChildNodes[CSProj_XMLTemplate_IncludeChildRank + 1].AppendChild(tnewnode);//tcsproj.ImportNode(tdproj.ChildNodes[1].ChildNodes[i].ChildNodes[j], true));
                            }
                            if (tincludeextension == "pas")
                            {
                                XmlElement tnewnode = tcsproj.CreateElement("Compile", "http://schemas.microsoft.com/developer/msbuild/2003");
                                tnewnode.SetAttribute("Include", tincludearr[0]+".cs");
                                tcsproj.ChildNodes[1].ChildNodes[CSProj_XMLTemplate_IncludeChildRank].AppendChild(tnewnode);//tcsproj.ImportNode(tdproj.ChildNodes[1].ChildNodes[i].ChildNodes[j], true));
                            }
                        }
                    }
                }
            }


            //Convert XML to string
            txmlstring = "";
            using (var stringWriter = new StringWriter())
            using (var xmlTextWriter = XmlWriter.Create(stringWriter))
            {
                tcsproj.WriteTo(xmlTextWriter);
                xmlTextWriter.Flush();
                txmlstring = stringWriter.GetStringBuilder().ToString();
            }

            //Replace Delphi specific text to CS
            string pattern = "\\b" + "DCCReference" + "\\b";
            txmlstring = Regex.Replace(txmlstring, pattern, "Compile");

            pattern = "\\b" + ".dcp" + "\\b";
            txmlstring = Regex.Replace(txmlstring, pattern, ".proj");

            pattern = "\\b" + ".pas" + "\\b";
            txmlstring = Regex.Replace(txmlstring, pattern, ".cs");

            //pattern = "\\b" + "xmlns=\"\"" + "\\b";
            //txmlstring = Regex.Replace(txmlstring, pattern, "");

            //Save new .csproj out
            tcsproj.LoadXml(txmlstring);
            projects.Add(tfilename, tcsproj);
            //tcsproj.Save(ioutpath);
        }

        public static XmlDocument CreateEmptyProj(string iName, string iGUID, XmlDocument icsproj)
        {
            XmlDocument tcsproj = new XmlDocument();
            string txmlstring = "";
            using (var stringWriter = new StringWriter())
            using (var xmlTextWriter = XmlWriter.Create(stringWriter))
            {
                icsproj.WriteTo(xmlTextWriter);
                xmlTextWriter.Flush();
                txmlstring = stringWriter.GetStringBuilder().ToString();
            }
            tcsproj.LoadXml(txmlstring);

            //Get all includes from .dproj
            List<XmlNode> tincludes = new List<XmlNode>();
            
            //AssemblyName
            tcsproj.ChildNodes[1].ChildNodes[0].ChildNodes[5].InnerText = iName;

            //Project type
            string tprojtype = "Library";
            tcsproj.ChildNodes[1].ChildNodes[0].ChildNodes[3].InnerText = tprojtype;

            //Project GUID
            tcsproj.ChildNodes[1].ChildNodes[0].ChildNodes[2].InnerText = iGUID;

            return tcsproj;
        }

        public static Stream GenerateStreamFromString(string s)
        {
            MemoryStream stream = new MemoryStream();
            StreamWriter writer = new StreamWriter(stream);
            writer.Write(s);
            writer.Flush();
            stream.Position = 0;
            return stream;
        }
    }

    public class Translate
    {
        public Delphi IL;
        public CSharp CS;
        public string filename;
        public string path, directory, outPath, patchPath, patchFile, overridePath, overrideFile, outPathCS;
        public LogDelegate log;
        public List<string> oglobal_names, oglobals, olocal_names, olocals, StandardReferences, DelphiStandardReferences;
        public List<List<string>> StandardCSReferences;
        public LogDelegate Log;
        public bool overriden = false;
        public s form;

        public Translate(string iPath, string idirectory, string iOutPath, string ioutPathCS, string iPatchPath, string iPatchFile, string iOverridePath, string iOverrideFile, LogDelegate ilog, List<string> iStandardReferences, List<string> iDelphiStandardReferences, List<List<string>> iStandardCSReferences, LogDelegate iLog, ref s iform)
        {
            form = iform;
            path = iPath;
            directory = idirectory; 
            outPath = iOutPath;
            outPathCS = ioutPathCS;
            patchPath = iPatchPath; 
            patchFile = iPatchFile; 
            overridePath = iOverridePath; 
            overrideFile = iOverrideFile; 
            log = ilog;
            oglobal_names = new List<string>();//tglobal_names; 
            oglobals = new List<string>(); //tglobals; 
            olocal_names = new List<string>(); //tlocal_names; 
            olocals = new List<string>(); //tlocals; 
            StandardReferences = iStandardReferences; 
            DelphiStandardReferences = iDelphiStandardReferences; 
            StandardCSReferences = iStandardCSReferences; 
            Log = iLog;
        }

        public void ReadDelphi(Object threadContext)
        {
            Delphi tdelphi = new Delphi();
            List<string> tdelphitext = Utilities.TextFileReader(path);
            string[] tpath_elements = outPath.Split('\\');
            string[] tfilename_elements = path.Split('\\');
            string tfilename = tfilename_elements[tfilename_elements.GetLength(0) - 1];
            tfilename = tfilename.Replace(".pas", "");
            filename = tfilename;

            //Embed path information
            tdelphi.directory = directory;
            tdelphi.logsingle = Log;
            tdelphi.outPath = outPath + "\\" + tfilename + ".cs";

            if (overrideFile == "")
            {
                tdelphi.Read(ref tdelphitext, true, "{ --", "-- }", log, ref form);
                //Write to file
                Directory.CreateDirectory(outPath);

                XmlSerializer x;

                x = new XmlSerializer(typeof(Translator.Delphi));
                XmlSerializerNamespaces ns = new XmlSerializerNamespaces();
                ns.Add("", "");

                using (var writer = XmlWriter.Create(outPath + "\\" + filename + ".xml"))
                    x.Serialize(writer, tdelphi, ns);

                IL = tdelphi;
            }
            else
                overriden = true;
        }

        public void ReadIL(Object threadContext)
        {
            Delphi tdelphi = new Delphi();
            string[] tpath_elements = outPath.Split('\\');
            string[] tfilename_elements = path.Split('\\');
            string tfilename = tfilename_elements[tfilename_elements.GetLength(0) - 1];
            tfilename = tfilename.Replace(".pas", "");
            filename = tfilename;

            //Embed path information
            tdelphi.directory = directory;
            tdelphi.logsingle = Log;
            tdelphi.outPath = outPath + "\\" + tfilename + ".cs";

            if (overrideFile == "")
            {
                XmlSerializer x = new XmlSerializer(typeof(Translator.Delphi));

                using (var reader = XmlReader.Create(outPath + "\\" + filename + ".xml"))
                    tdelphi = (Translator.Delphi) x.Deserialize(reader);

                IL = tdelphi;
            }
            else
                overriden = true;
        }

        public void WriteCS(Object threadContext)
        {
            CS = new CSharp();
            CS.file_path = outPathCS;
            CS.standard_references = StandardReferences;

            string[] tout = CS.Write(ref IL.script, directory.Replace(" ", "_"), ref oglobal_names, ref oglobals, ref olocal_names, ref olocals, ref DelphiStandardReferences, ref StandardCSReferences).ToArray();

            //Write to file
            Directory.CreateDirectory(outPathCS);
            File.WriteAllLines(outPathCS + "\\" + filename + ".cs", tout, Encoding.UTF8);
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
