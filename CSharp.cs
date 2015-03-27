using System;
using System.Linq;
using System.Collections.Generic;

namespace Translator {
    public class CSharp {

        List<string> project_references = new List<string>();
        public List<string> standard_references = new List<string>();

        bool isForm = false;
        //Log 
        public LogDelegate logsingle;
        public string file_path = "";

        //Substitute standard C# reference for standard reference of other language
        public void ProcessReference(string ilibrary, ref List<string> iStandardReferences, ref List<List<string>> iStandardCSReferences)
        {
            bool isStandardRef = false;

            for (int i = 0; i < iStandardReferences.Count; i++)
            {
                if (ilibrary.Trim().ToLower() == iStandardReferences[i].Trim().ToLower())
                {
                    project_references.AddRange(iStandardCSReferences[i]);
                    isStandardRef = true;
                    break;
                }
            }

            if (!isStandardRef)
                project_references.Add(ilibrary);

            //Remove duplicates
            project_references = project_references.Distinct().ToList();
        }

        public string Indent(int isize)
        {
            return new string(' ', isize);
        }

        //Convert a script to C#
        public List<string> Write(Script iscript, string inamespace, ref List<string> oglobal_names, ref List<string> oglobals, ref List<string> olocal_names, ref List<string> olocals, ref List<string> iStandardReferences, ref List<List<string>> iStandardCSReferences)
		{
			List<string> tout = new List<string>();

            //Header
            tout.Add("/**Header Start");
            for (int i = 0; i < iscript.header.Count; i++) 
            {
                tout.Add(iscript.header[i]);
            }
            tout.Add("Header End**/");            
            tout.Add("");

            //References
            tout.Add("");
            tout.Add("/*References Start*/");

            //Add standard references
            for (int i = 0; i < standard_references.Count; i++)
            {
                tout.Add("using " + standard_references[i] + ";");
            }

            //Process rest
            for( int i=0; i < iscript.includes.Count; i++)
			{
                ProcessReference(iscript.includes[i], ref iStandardReferences, ref iStandardCSReferences);
            }

            for (int i = 0; i < project_references.Count; i++)
            {
                tout.Add("using " + project_references[i] + ";");
            }

            tout.Add("/*References End*/");
            tout.Add("");

            //Types
            tout.Add("/*Types Start*/");
            for( int i=0; i < iscript.classes[0].types.Count; i++)
                tout.Add(Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(TypeToString(iscript.classes[0].types[i])).Replace(";;", ";")).Replace("==", "=").Trim());

            for (int i = 0; i < iscript.classes[1].types.Count; i++)
                tout.Add(Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(TypeToString(iscript.classes[1].types[i])).Replace(";;", ";")).Replace("==", "=").Trim());

            tout.Add("/*Types End*/");
            tout.Add("");

            //Namespace
            tout.Add("namespace " + inamespace);
            tout.Add("{");

            //Global Constants, Variables and Enums
            ProcessGlobals(iscript.classes[0], ref oglobals, ref oglobal_names);
            
            //Local Constants, Variables and Enums
            ProcessGlobals(iscript.classes[1], ref olocals, ref olocal_names);

            List<string> telement_names = new List<string>();
            string classtype = "";
            classtype = "class";
            tout.Add("");
            tout.Add("//Class GlobalVars");
            int tGlobalVarInsert = tout.Count;

            List<string> tGlobalVarClass = new List<string>();
            tGlobalVarClass.Add(Indent(4) + "public " + classtype + " GlobalVars");
            tGlobalVarClass.Add(Indent(4) + "{");

            tGlobalVarClass.AddRange(WriteClassBody(iscript.classes[0], ref telement_names));
            tGlobalVarClass.AddRange(WriteClassBody(iscript.classes[1], ref telement_names));

            tGlobalVarClass.Add(Indent(4) + "}");

            //Classes
            for (int i = 2; i < iscript.classes.Count; i++)
            {
                bool tHasBaseclass = true;
                if ((iscript.classes[i].baseclass == null) || (iscript.classes[i].baseclass == "") || (iscript.classes[i].baseclass == "null"))
                    tHasBaseclass = false;

                classtype = "";
                if ((iscript.classes[i].type == "c"))
                    classtype = "class";
                else if ((iscript.classes[i].type == "r"))
                    classtype = "struct";
                else
                    classtype = "interface";

                //Handle constructor

                tout.Add("");
                tout.Add("//Class " + iscript.classes[i].name);

                //Classes / Interfaces
                //Inheritance
                if (!tHasBaseclass)
                    tout.Add(Indent(4) + "public " + classtype + " " + iscript.classes[i].name);
                else
                    tout.Add(Indent(4) + "public " + classtype + " " + iscript.classes[i].name + " : " + iscript.classes[i].baseclass);

                tout.Add(Indent(4) + "{");

                telement_names = new List<string>();
                tout.AddRange(WriteClassBody(iscript.classes[i], ref telement_names));

                tout.Add(Indent(4) + "}");
            }

            tout.Add("}");

            List<string> temptytypeslist = new List<string>();

            //Replace the local Globals used in the text
            Translate.GlobalsRename(inamespace + ".GlobalVars.", ref tout, olocal_names, temptytypeslist);
            tout.InsertRange(tGlobalVarInsert, tGlobalVarClass);
            return tout;
        }        

        public void ProcessGlobals(Class iclass, ref List<string> obody, ref List<string> onames)
        {
            obody.AddRange(WriteClassBody(iclass, ref onames));
        }

        public string GetElementName(string iElement)
        {
            string tout = iElement;
            string[] tarr = tout.Split('=');
            tarr = tarr[0].Trim().Split(' ');
            tout = tarr[tarr.Length-1].Replace(";","");
            return tout;
        }

        public List<string> WriteClassBody(Class iclass, ref List<string> oelement_names)
        {
            List<string> tout = new List<string>();

            for (int i = 0; i < iclass.constants.Count; i++)
            {
                string tconststatic = "";
                if (iclass.constants[i].isStatic)
                    tconststatic = "static ";
                tout.Add(Indent(4) + Indent(4) + "public " + tconststatic + Utilities.Beautify_Delphi2CS(ConstantToString(iclass.constants[i])).Replace("==", "=").Replace(":", ""));
                oelement_names.Add(GetElementName(tout[tout.Count - 1]));
            }

            for (int i = 0; i < iclass.enums.Count; i++)
            {
                tout.Add(Indent(4) + Indent(4) + "public " + EnumToString(iclass.enums[i]));
                oelement_names.Add(GetElementName(tout[tout.Count - 1]));
            }

            for (int i = 0; i < iclass.variables.Count; i++)
            {
                if (iclass.variables[i].isStatic)
                    tout.Add(Indent(4) + Indent(4) + "public static " + Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(VarToString(iclass.variables[i]))).Replace("==", "=").Replace(";;", ";"));
                else
                    tout.Add(Indent(4) + Indent(4) + "public " + Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(VarToString(iclass.variables[i]))).Replace("==", "=").Replace(";;", ";"));
                oelement_names.Add(GetElementName(tout[tout.Count - 1]));
            }

            for (int i = 0; i < iclass.properties.Count; i++)
            {
                string tproperty = "";
                if (iclass.properties[i].isStatic)
                    tproperty = (Indent(4) + Indent(4) + "public static " + Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(PropertyToString(iclass.properties[i]))));
                else
                    tproperty = (Indent(4) + Indent(4) + "public " + Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(PropertyToString(iclass.properties[i]))));
                tout.Add(tproperty.Replace("/*", "{").Replace("*/", "}"));
                oelement_names.Add(GetElementName(tout[tout.Count - 1]));
            }

            //for (int i = 0; i < iclass.types.Count; i++)
            //{
            //    oelement_names.Add(iclass.types[i].name); 
            //    tout.Add(Indent(4) + Indent(4) + (Utilities.Delphi2CSRules(TypeToString(iclass.types[i]))));
            //}

            for (int i = 0; i < iclass.functions.Count; i++)
            {
                tout.Add("");

                Function tfunc = iclass.functions[i];

                List<string> tconvertedfunc = ConvertFunction(tfunc, ref iclass, i, false);
                if (iclass.type == "i")
                {
                    tconvertedfunc.RemoveAt(tconvertedfunc.Count - 1);
                    tconvertedfunc[tconvertedfunc.Count - 1] = tconvertedfunc[tconvertedfunc.Count - 1] + ";";
                }
                tout.AddRange(tconvertedfunc);
            }

            //Actions
            for (int i = 0; i < iclass.actions.Count; i++)
            {
                for (int j = 0; j < iclass.actions[i].Count; j++)
                {
                    List<string> tconvertedaction = new List<string>();
                    tconvertedaction = ConvertFunction(iclass.actions[i][j], ref iclass, i, true);
                    tout.AddRange(tconvertedaction);
                }
            }

            return tout;
        }

        public List<string> ConvertFunction(Function ifunction, ref Class iclass, int icounter, bool isaction)
        {
            List<string> tout = new List<string>();
            string tfunction_name = ifunction.name;
            string treturn_type = Utilities.Beautify_Delphi2CS(ifunction.returnType);

            //Method body
            List<string> tbody = new List<string>();


            bool tHasBaseclass = true;
            string tinheritance = "";

            if ((iclass.baseclass == null) || (iclass.baseclass == "") || (iclass.baseclass == "null"))
                tHasBaseclass = false;

            bool tisConstructor = false;
            //Special Constructor rules
            if (ifunction.name == "Create")
            {
                tisConstructor = true;

                //Change Create to Class name in C#
                tfunction_name = iclass.name;

                int ttemplatestart = tfunction_name.IndexOf('<');
                if (ttemplatestart != -1)
                {
                    int ttemplateend = tfunction_name.IndexOf('>');
                    if (ttemplateend != tfunction_name.Length - 1)
                        tfunction_name = tfunction_name.Substring(0, ttemplatestart) + tfunction_name.Substring(ttemplateend, tfunction_name.Length - 1);
                    else
                        tfunction_name = tfunction_name.Substring(0, ttemplatestart);
                }

                ifunction.isStatic = false;
                ifunction.returnType = "";
                treturn_type = "";
            }

            for (int j = 0; j < ifunction.commands.Count; j++)
            {
                string tstr = ifunction.commands[j].Trim();

                if (tstr != "end.")
                {
                    //Remove all empty lines
                    if ((tstr != "") && (tstr != "\n"))
                    {
                        //Preprocessing
                        //Check for return statements
                        if (tstr.IndexOf("EXIT;") != -1)
                            if (ifunction.returnType != "")
                                tstr = tstr.Replace("EXIT;", "return result;");

                        if (tstr.IndexOf("while") != -1)
                            if (tstr.IndexOf("(") == -1)
                                tstr = tstr.Replace("while", "while(");

                        //Processing
                        tstr = Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(tstr));

                        //Postprocessing
                        if (tstr != "")
                        {
                            if ((tstr.IndexOf("inherited") == -1) & (tstr.IndexOf("Inherited") == -1))
                                tstr = tstr.Replace("Create(", tfunction_name + "(");

                            if ((tstr.IndexOf("default") != -1) & (tstr.IndexOf(":") == -1))
                                tstr = tstr.Replace("default", "new").Replace("(", "").Replace(")", "()") + "//TODO";

                            //Check for StringReplace
                            if ((tstr.IndexOf("StringReplace") != -1))
                            {
                                tstr = tstr.Replace("StringReplace", "WrapperUtilities.StringReplace").Replace("]", "}").Replace("[", "new string[] {").Replace("rfReplaceAll", "\"rfReplaceAll\"").Replace("rfIgnoreCase", "\"rfIgnoreCase\"");
                            }
                            else if ((tstr.IndexOf("SetLength") != -1))
                            {
                                tstr = tstr.Replace("SetLength", "WrapperUtilities.SetLength").Replace("(", "( ref ");
                            }
                            else if ((tstr.IndexOf("Inc(") != -1))
                            {
                                tstr = tstr.Replace("Inc(", "WrapperUtilities.Inc(").Replace("(", "( ref ");
                            }
                            else if ((tstr.IndexOf("FillChar") != -1))
                            {
                                tstr = tstr.Replace("(", "( ref ");
                            }
                            else if ((tstr.IndexOf("SHGetSpecialFolderPath") != -1))
                            {
                                tstr = tstr.Replace(",", ",ref").Replace("ref false", "false").Replace("ref true", "true");
                            }                                
                            else
                            {
                                //Add Semicolons
                                if ((tstr.IndexOf("=") != -1) & (tstr[tstr.Length - 1] != ';') & (tstr.IndexOf("==") == -1) & ((tstr.IndexOf("for (") == -1) & (tstr.IndexOf("for(") == -1)))
                                    tstr = tstr + ';';
                                //Check for loops
                                else if ((tstr.IndexOf("for (") != -1) || (tstr.IndexOf("for(") != -1))
                                {
                                    string ttempstr = tstr.Trim();
                                    string[] tfor_elements = ttempstr.Split(' ');
                                    string tfor_counter = tfor_elements[1].Split('(')[1];
                                    tstr = tstr.Replace("to", "; " + tfor_counter + " <=");
                                    tstr = tstr.Substring(0, tstr.Length - 1) + "; " + tfor_counter + "++ )";
                                }
                                //Switch cases
                                else if (((tstr.IndexOf("case ") != -1) || (tstr.IndexOf("case (") != -1) || (tstr.IndexOf("case(") != -1)) && (tstr.IndexOf(":") == -1) )
                                {
                                    if ((tstr.IndexOf("case ") != -1) && (tstr.IndexOf(" of") != -1) && (tstr.IndexOf("case (") == -1))
                                    {
                                        tstr = tstr.Replace("case", "switch(").Replace("of", "){");
                                    }
                                    else if ((tstr.IndexOf("case (") != -1) || (tstr.IndexOf("case(") != -1))
                                    {
                                        tstr = tstr.Replace("case", "switch").Replace("of", "{");
                                    }
                                    int tswitchbegincounter = 0;
                                    int tswitchcounter = j + 1;
                                    int tswitchstart = j;
                                    while (tswitchbegincounter > -1 & tswitchcounter < ifunction.commands.Count)
                                    {
                                        string tswitchcommand = ifunction.commands[tswitchcounter].Trim();

                                        if (tswitchcommand != "")
                                        {
                                            if (tswitchcommand[0] == '/')
                                            {
                                                ifunction.commands[tswitchcounter] = "";
                                            }
                                            else if (tswitchcommand == "begin")
                                            {
                                                tswitchbegincounter++;
                                                //ifunction.commands[tswitchcounter] = "{";
                                            }
                                            else if (tswitchcommand == "end;" || tswitchcommand == "end")
                                            {
                                                tswitchbegincounter--;
                                                ifunction.commands[tswitchcounter] = "End;";
                                            }
                                            //Add default case
                                            else if (tswitchcommand == "else")
                                            {
                                                ifunction.commands[tswitchcounter] = "default:";
                                            }
                                        }
                                        tswitchcounter++;
                                    }
                                    ifunction.commands.Insert(tswitchcounter - 1, "break;");
                                    tswitchcounter++;

                                    int toldk = tswitchstart + 1;
                                    int k = 0;
                                    for (k = tswitchstart + 1; k < tswitchcounter && k < ifunction.commands.Count; k++)
                                    {
                                        string tswitchcommand = ifunction.commands[k];
                                        string tcasename = "";
                                        List<string> tcasebody = new List<string>();
                                        //Add all the cases
                                        if (tswitchcommand.IndexOf(':') != -1 & tswitchcommand.IndexOf(":=") == -1)
                                        {
                                            tcasename = tswitchcommand;
                                            k++;
                                            string tnextcase = ifunction.commands[k];
                                            int tcurrcommandcounter = k;

                                            while (k < tswitchcounter & (tnextcase.IndexOf(':') == -1 || tnextcase.IndexOf(":=") != -1))
                                            {
                                                tcasebody.Add(tnextcase);
                                                k++;
                                                tnextcase = ifunction.commands[k];
                                            }

                                            //Check for multiple cases
                                            string[] tcasesarr = tcasename.Split(',');
                                            List<string> tconvertedswitchcommands = new List<string>();

                                            for (int l = 0; l < tcasesarr.Length; l++)
                                            {
                                                string tcasestring = tcasesarr[l].Trim().Replace(':', ' ');

                                                if (tcasestring.IndexOf("default") != -1)
                                                {
                                                    tconvertedswitchcommands.Add(tcasestring + ":    ");
                                                }
                                                else
                                                {
                                                    tconvertedswitchcommands.Add("case " + tcasestring + ":    ");
                                                }

                                                tconvertedswitchcommands.AddRange(tcasebody);
                                                tconvertedswitchcommands.Add("break;");
                                            }
                                            ifunction.commands.RemoveRange(toldk, k - toldk);
                                            tswitchcounter = tswitchcounter + tconvertedswitchcommands.Count - k + toldk;
                                            k = toldk + tconvertedswitchcommands.Count - 1;
                                            ifunction.commands.InsertRange(toldk, tconvertedswitchcommands);
                                            toldk = toldk + tconvertedswitchcommands.Count;
                                        }
                                        //j = tswitchcounter;
                                    }
                                    ifunction.commands.RemoveRange(toldk - 1, 1);
                                }
                            }
                        }
                    }

                    //Check if output is empty
                    if ((tstr != "") && (tstr != "\n"))
                    {
                        if (tstr.IndexOf("return result;") != -1)
                            if (treturn_type.Trim() == "void")
                            {
                                tbody.Add(Indent(4) + Indent(4) + Indent(4) + "return;");
                                tstr = "";
                            }

                        //If line is only "Exit;", check for type to return
                        if (tstr.IndexOf("Exit;") != -1)
                        {
                            if (treturn_type != "void")
                                tbody.Add(Indent(4) + Indent(4) + Indent(4) + "return result;");
                            else
                                tbody.Add(Indent(4) + Indent(4) + Indent(4) + "return;");
                        }
                        else
                            tbody.Add(Indent(4) + Indent(4) + Indent(4) + tstr);
                    }
                }
            }

            string tspecialconstructor = "";
            tfunction_name = tfunction_name.Trim();
            //Special Constructor rules
            if (tisConstructor)
            {
                //Remove "inherited Create();"
                int tfound = Delphi.FindStringInList("inherited Create", ref tbody, 0, true);
                if (tfound != -1)
                    tbody.RemoveAt(tfound);

                for (int j = 0; j < tbody.Count; j++)
                {
                    string tstr = tbody[j].Trim();
                    if (tstr.IndexOf(tfunction_name) != -1)
                    {
                        tspecialconstructor = tbody[j].Trim().Replace(tfunction_name, " : this").Replace(';', ' ');
                        tbody[j] = "";
                        break;
                    }
                }

                //If the constructor is inherited
                if (tHasBaseclass)
                {
                    tinheritance = ": base(";

                    if (ifunction.parameters.Count > 0)
                        tinheritance = tinheritance + ifunction.parameters[0].name;

                    if (ifunction.parameters.Count > 1)
                        for (int j = 1; j < ifunction.parameters.Count - 1; j++)
                            tinheritance = tinheritance + ", " + ifunction.parameters[j].name;

                    tinheritance = tinheritance + ")";

                    //If there are additional commands to modify the inherited constructor, Flag in Log, to inspect manually
                    if (tbody.Count > 0)
                        logsingle("Complex Constructor detected in Class:" + iclass.name + " in File" + file_path);
                }
            }

            //Method Parameters
            string tparam_string = "";

            for (int j = 0; j < ifunction.parameters.Count - 1; j++)
                tparam_string = tparam_string + VarToString(ifunction.parameters[j]);

            if (ifunction.parameters.Count > 0)
                tparam_string = tparam_string + VarToString(ifunction.parameters[ifunction.parameters.Count - 1]).Replace(";", "");

            tparam_string = tparam_string.Replace(";", ", ").Replace(":", "").Replace("const", "").Replace("  ", " ");

            //Method Attributes
            string tattributes = "";

            if (ifunction.isStatic)
                tattributes += "static ";

            if (ifunction.isVirtual)
                tattributes += "virtual ";

            if (ifunction.isAbstract)
                tattributes += "abstract ";

            //Replace "array of" with "[]"
            string[] ttempparamarr = tparam_string.Split(' ');
            string ttempparamstr = "";

            for (int k = 0; k < ttempparamarr.Length; k++)
            {
                if (ttempparamarr[k] == "array")
                {
                    ttempparamarr[k] = "";
                    k++;
                    ttempparamarr[k] = "";
                    k++;
                    ttempparamarr[k] = ttempparamarr[k] + "[]";
                }
                ttempparamstr = ttempparamstr + " " + ttempparamarr[k];
            }
            tparam_string = ttempparamstr.Trim();

            ttempparamarr = treturn_type.Split(' ');
            ttempparamstr = "";
            for (int k = 0; k < ttempparamarr.Length; k++)
            {
                if (ttempparamarr[k] == "array")
                {
                    ttempparamarr[k] = "";
                    k++;
                    ttempparamarr[k] = "";
                    k++;
                    ttempparamarr[k] = ttempparamarr[k] + "[]";
                }
                ttempparamstr = ttempparamstr + " " + ttempparamarr[k];
            }
            treturn_type = ttempparamstr.Trim();


            //Method Definition (attributes + return type + name + parameters + inheritance)
            tout.Add(Indent(4) + Indent(4) + "public " + tattributes + Utilities.Beautify_Delphi2CS(treturn_type) + " " + tfunction_name + "(" + Utilities.Beautify_Delphi2CS(tparam_string) + ")" + tinheritance + tspecialconstructor);

            tout.Add(Indent(4) + Indent(4) + "{");

            treturn_type = treturn_type.Trim();

            //Add 'result'
            if (treturn_type != "void" & treturn_type != "" & iclass.type != "i")
            {
                if ((treturn_type == "ansistring") || (treturn_type == "widestring") || (treturn_type == "unicodestring") || (treturn_type == "string") || (treturn_type == "delphistring"))
                    tout.Add(Indent(4) + Indent(4) + Indent(4) + VarToString(new Variable("result", treturn_type, "\"" + "\"", "", false)));
                else if ((treturn_type.IndexOf("TArray") != -1 || (treturn_type.IndexOf("TList") != -1)))
                    tout.Add(Indent(4) + Indent(4) + Indent(4) + VarToString(new Variable("result", treturn_type, "null", "", false)));
                else
                    tout.Add(Indent(4) + Indent(4) + Indent(4) + VarToString(new Variable("result", treturn_type, "", "", false)));
            }

            //Method Constants
            for (int j = 0; j < ifunction.constants.Count; j++)
            {
                string ttemptype = ifunction.constants[j].type.Replace(';', ' ').Trim();

                if ((ttemptype == "ansistring") || (ttemptype == "widestring") || (ttemptype == "unicodestring") || (ttemptype == "string") || (ttemptype == "delphistring"))
                    if (ifunction.constants[j].value == "")
                        ifunction.constants[j].value = "\"" + "\"";

                tout.Add(Indent(4) + Indent(4) + Indent(4) + ConstantToString(ifunction.constants[j]));
            }

            //Method Variables
            for (int j = 0; j < ifunction.variables.Count; j++)
            {
                string ttemptype = ifunction.variables[j].type.Replace(';',' ').Trim();

                if ((ttemptype == "ansistring") || (ttemptype == "widestring") || (ttemptype == "unicodestring") || (ttemptype == "string") || (ttemptype == "delphistring") || (ttemptype == "PWideChar"))
                    ifunction.variables[j].value = "\"" + "\"";
                else if ((ttemptype.IndexOf("TArray") != -1 || (ttemptype.IndexOf("TList") != -1)))
                    ifunction.variables[j].value =  "null";

                tout.Add(Indent(4) + Indent(4) + Indent(4) + VarToString(ifunction.variables[j]));
            }

            if (tbody.Count > 0)
            {
                tbody.RemoveAt(0);
                //tbody.RemoveAt(tbody.Count - 1);
            }

            //Insert return statement
            if (treturn_type != "void" & tbody.Count != 0 & treturn_type != "")
            {
                for (int j = tbody.Count - 1; j > 0; j--)
                {
                    if (tbody[j].Trim() == "}")
                    {
                        tbody.Insert(j, Indent(4) + Indent(4) + Indent(4) + "return result;");
                        goto return_over;
                    }
                }
            }
        return_over:

            tout.AddRange(tbody);
            tout[tout.Count - 1] = Indent(4) + Indent(4) + tout[tout.Count - 1].Trim();
            //tout.Add(Indent(4) + Indent(4) + "}");
            
            return tout;
        }

        public string ConstantToString(Constant iconst)
        {
            return Utilities.Beautify_Delphi2CS(iconst.type) + " " + iconst.name + " = " + iconst.value + ";";
        }

        public string EnumToString(Enum ienum)
        {
            string tout = "enum " + ienum.name + " {";

            for (int i = 0; i < ienum.enums.Count - 1; i++)
            {
                if (ienum.enums[i].name != null)
                    tout = tout + ienum.enums[i].name + "=" + ienum.enums[i].value.Replace(";","") + ", ";
            }

            if (ienum.enums[ienum.enums.Count - 1].name != null)
                tout = tout + ienum.enums[ienum.enums.Count - 1].name + "=" + ienum.enums[ienum.enums.Count - 1].value.Replace(";", "") + " };";

            return tout;
        }

        public string VarToString(Variable ivar)
        {
            ////Search for comments in 'type' and replace with /**/ style comment
            //int tindex = ivar.type.IndexOf(@"//");
            //string tcomment = "";
            //if (tindex != -1)
            //{
            //    tcomment = ivar.type.Substring(tindex);
            //    ivar.type = ivar.type.Substring(0, tindex);
            //}

            //Remove type name from ivar.name
            ivar.name = ivar.name.Replace("class", "").Replace("var", "");
            ivar.type = Utilities.Beautify_Delphi2CS(ivar.type.Replace(";", "").Trim());

            //For 'var' types
            if (ivar.type.Trim() == "")
                ivar.type = "object";

            string tout = "";

            if (ivar.value != "")
                tout = ivar.type + " " + ivar.name + " = " + ivar.value + ";" + ivar.comment;
            else
                tout = ivar.type + " " + ivar.name + ";" + ivar.comment;

            return tout.Replace("  ", " ").Replace("  ", " ");
        }

        public string PropertyToString(Property iprop)
        {
            string tcomment = "";
            string tread = "", twrite = "";
            if (iprop.read == "null")
                tread = "";
            else
            {
                string tstr = iprop.read;
                tstr = tstr.Replace(@"//", "~");
                string[] tarr = tstr.Split('~');
                if (tarr.Length > 1)
                    tcomment += tarr[1];

                char[] tchar_arr = tarr[0].ToCharArray();
                if (tchar_arr[0] != 'f')
                    tarr[0] = tarr[0] + "()";

                tread = "return " + tarr[0] + ";";
            }

            if (iprop.write == "null")
                twrite = "";
            else
            {
                string tstr = iprop.write;

                tstr = tstr.Replace(@"//", "~");

                string[] tarr = tstr.Split('~');

                if (tarr.Length > 1)
                    tcomment += tarr[1];

                twrite = "value := " + tarr[0] + ";";
            }
            string treturnstr = iprop.type + " " + iprop.name + " { get { " + tread + "} set { " + twrite + "} }";

            if (tcomment != "")
                treturnstr = treturnstr + @"//" + tcomment;

            return treturnstr;
        }

        public string TypeToString(TypeAlias itype)
        {
            return "using " + itype.name + " = " + itype.type + ";";
        }
    }
}