using System;
using System.Linq;
using System.Collections.Generic;

namespace Translator {
    public class CSharp {

        List<string> project_references = new List<string>();

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
        public List<string> Write(ref Script iscript, string inamespace, ref List<string> oglobal_names, ref List<string> oglobals, ref List<string> olocal_names, ref List<string> olocals, ref List<string> iStandardReferences, ref List<List<string>> iStandardCSReferences)
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
            tout.Add("");

            for( int i=0; i < iscript.includes.Count; i++)
			{
                ProcessReference(iscript.includes[i], ref iStandardReferences, ref iStandardCSReferences);
            }

            for (int i = 0; i < project_references.Count; i++)
            {
                tout.Add("using " + project_references[i] + ";");
            }

            tout.Add("");
            tout.Add("/*References End*/");
            tout.Add("");

            //Types
            tout.Add("");
            tout.Add("/*Types Start*/");
            tout.Add("");
            //for( int i=0; i < iscript.includes.Count; i++)
            //{
            //    tout.Add("using " + ConvertToStandardLibrary(iscript.includes[i]));
            //}
            tout.Add("");
            tout.Add("/*Types End*/");
            tout.Add("");

            //Namespace
            tout.Add("");
            tout.Add("namespace " + inamespace);
            tout.Add("{");

            //Global Constants, Variables and Enums
            ProcessGlobals(iscript.classes[0], ref oglobals, ref oglobal_names);
            
            //Local Constants, Variables and Enums
            ProcessGlobals(iscript.classes[1], ref olocals, ref olocal_names);

            //Classes
            for (int i = 2; i < iscript.classes.Count; i++)
            {
                bool tHasBaseclass = true;
                if ((iscript.classes[i].baseclass == null) || (iscript.classes[i].baseclass == "") || (iscript.classes[i].baseclass == "null"))
                    tHasBaseclass = false;

                string classtype = "";
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

                List<string> telement_names = new List<string>();
                tout.AddRange(WriteClassBody(iscript.classes[i], ref telement_names));

                tout.Add(Indent(4) + "}");
            }

            tout.Add("}");

            //Replace the local Globals used in the text
            Translate.GlobalsRename(inamespace + "_Locals.", ref tout, olocals);
            return tout;
        }        

        public void ProcessGlobals(Class iclass, ref List<string> obody, ref List<string> onames)
        {
            obody = WriteClassBody(iclass, ref onames);
        }

        public List<string> WriteClassBody(Class iclass, ref List<string> oelement_names)
        {
            List<string> tout = new List<string>();

            for (int i = 0; i < iclass.constants.Count; i++)
            {
                oelement_names.Add(iclass.constants[i].name);
                tout.Add(Indent(4) + Indent(4) + "public " + Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(ConstantToString(iclass.constants[i]))));
            }

            for (int i = 0; i < iclass.enums.Count; i++)
            {
                oelement_names.Add(iclass.enums[i].name);
                tout.Add(Indent(4) + Indent(4) + Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(EnumToString(iclass.enums[i]))));
            }

            for (int i = 0; i < iclass.variables.Count; i++)
            {
                oelement_names.Add(iclass.variables[i].name);
                if (iclass.variables[i].isStatic)
                    tout.Add(Indent(4) + Indent(4) + "public static " + Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(VarToString(iclass.variables[i]))));
                else
                    tout.Add(Indent(4) + Indent(4) + "public " + Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(VarToString(iclass.variables[i]))));
            }

            for (int i = 0; i < iclass.properties.Count; i++)
            {
                oelement_names.Add(iclass.properties[i].name); 
                tout.Add(Indent(4) + Indent(4) + "public " + Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(PropertyToString(iclass.properties[i]))));
            }

            for (int i = 0; i < iclass.types.Count; i++)
            {
                oelement_names.Add(iclass.types[i].name); 
                tout.Add(Indent(4) + Indent(4) + Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(TypeToString(iclass.types[i]))));
            }

            for (int i = 0; i < iclass.functions.Count; i++)
            {
                tout.Add("");

                Function tfunc = iclass.functions[i];
                string tfunction_name = tfunc.name;
                string treturn_type = Utilities.Beautify_Delphi2CS(tfunc.returnType);

                //Method body
                List<string> tbody = new List<string>();

                for (int j = 0; j < tfunc.commands.Count; j++)
                {
                    string tstr = tfunc.commands[j].Trim();
                    //Remove all empty lines
                    if (( tstr!= "") && (tstr != "\n"))
                        tstr = Utilities.Beautify_Delphi2CS(Utilities.Delphi2CSRules(tfunc.commands[j]));
                    
                    //Check if output is empty
                    if ((tstr != "") && (tstr != "\n"))
                    {
                        //If line is only "Exit;", check for type to return
                        if (tstr.IndexOf("Exit;") != -1)
                        {
                            if (treturn_type != "void")
                                tbody.Add(Indent(4) + Indent(4)  + Indent(4) + "return result;");
                            else
                                tbody.Add(Indent(4) + Indent(4)  + Indent(4) + "return;");
                        }
                        else
                            tbody.Add(Indent(4) + Indent(4) + tstr);
                    }
                }

                bool tHasBaseclass = true;
                string tinheritance = ""; 

                if ((iclass.baseclass == null) || (iclass.baseclass == "") || (iclass.baseclass == "null"))
                    tHasBaseclass = false;

                //Special Constructor rules
                if (tfunc.name == "Create")
                {
                    //Change Create to Class name in C#
                    tfunction_name = iclass.name;

                    //Remove "inherited Create();"
                    int tfound = Delphi.FindStringInList("inherited Create", ref tbody, 0, true);
                    if (tfound != -1)
                        tbody.RemoveAt(tfound);

                    //If the constructor is inherited
                    if (tHasBaseclass)
                    {
                        tinheritance = ": base(";

                        if (tfunc.parameters.Count > 0)
                            tinheritance = tinheritance + tfunc.parameters[0].name;

                        if (tfunc.parameters.Count > 1)
                            for (int j = 1; j < tfunc.parameters.Count - 1; j++)
                                tinheritance = tinheritance + ", " + tfunc.parameters[j].name;
                        
                        tinheritance = tinheritance + ")";

                        //If there are additional commands to modify the inherited constructor, Flag in Log, to inspect manually
                        if (tbody.Count > 0)
                            logsingle("Complex Constructor detected in Class:" + iclass.name + " in File" + file_path);
                    }
                }

                //Method Parameters
                string tparam_string = "";

                for (int j = 0; j < tfunc.parameters.Count - 1; j++)
                    tparam_string = tparam_string + VarToString(tfunc.parameters[j]);

                if (tfunc.parameters.Count > 0)
                    tparam_string = tparam_string + VarToString(tfunc.parameters[tfunc.parameters.Count-1]).Replace(";","");

                tparam_string = tparam_string.Replace(";", ",").Replace(":", "").Replace("const","").Replace("  "," ");

                //Method Attributes
                string tattributes = "";

                if (tfunc.isStatic)
                    tattributes += "static ";

                if (tfunc.isVirtual)
                    tattributes += "virtual ";

                if (tfunc.isAbstract)
                    tattributes += "abstract ";

                //Method Definition (attributes + return type + name + parameters + inheritance)
                tout.Add(Indent(4) + Indent(4) + "public " + tattributes + treturn_type + " " + tfunction_name + "(" + Utilities.Beautify_Delphi2CS(tparam_string) + ")" + tinheritance);

                tout.Add(Indent(4) + Indent(4) + "{");
                //Add 'result'
                if (treturn_type != "void")
                    tout.Add(Indent(4) + Indent(4) + Indent(4) + treturn_type + " result;");

                //Method Constants
                for (int j = 0; j < tfunc.constants.Count; j++)
                    tout.Add(Indent(4) + Indent(4) + Indent(4) + Utilities.Beautify_Delphi2CS(ConstantToString(tfunc.constants[j])));

                //Method Variables
                for (int j = 0; j < tfunc.variables.Count; j++)
                    tout.Add(Indent(4) + Indent(4) + Indent(4) + Utilities.Beautify_Delphi2CS(VarToString(tfunc.variables[j])));

                if (tbody.Count > 0)
                {
                    tbody.RemoveAt(0);
                    tbody.RemoveAt(tbody.Count - 1);
                }

                tout.AddRange(tbody);

                if (treturn_type != "void")
                    tout.Add(Indent(4) + Indent(4) + Indent(4) + "return result;");

                tout.Add(Indent(4) + Indent(4) + "}");
            }

            return tout;
        }

        public string ConstantToString(Constant iconst)
        {
            return iconst.type + " " + iconst.name + " = " + iconst.value + ";";
        }

        public string EnumToString(Enum ienum)
        {
            string tout = "enum " + ienum.name + " {";

            for (int i = 0; i < ienum.enums.Count - 1; i++)
            {
                tout = tout + ConstantToString(ienum.enums[i]) + ", ";
            }

            tout = tout + ConstantToString(ienum.enums[ienum.enums.Count - 1]) + " };";
            return tout;
        }

        public string VarToString(Variable ivar)
        {
            return ivar.type + " " + ivar.name + ";";
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