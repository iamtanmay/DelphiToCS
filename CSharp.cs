using System;
using System.Linq;
using System.Collections.Generic;

namespace Translator {
    public class CSharp {

        List<string> project_references = new List<string>();

        bool isForm = false;

        //Substitute standard C# reference for standard reference of other language
        public void ProcessReference(string ilibrary, ref List<string> iStandardReferences, ref List<List<string>> iStandardCSReferences)
        {
            for (int i = 0; i < iStandardReferences.Count; i++)
            {
                if (ilibrary == iStandardReferences[i])
                {
                    project_references.AddRange(iStandardCSReferences[i]);
                }
            }

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

            tout.Add("using " + "System" + ";");
            tout.Add("using " + "System.Windows" + ";");
            tout.Add("using " + "System.String" + ";");
            tout.Add("using " + "System.Collections.Generic" + ";");

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
                tout.Add("");
                tout.Add("//Class " + iscript.classes[i].name);

                //Classes / Interfaces
                //Inheritance
                if ((iscript.classes[i].baseclass == null) || (iscript.classes[i].baseclass == "") || (iscript.classes[i].baseclass == "null"))
                    tout.Add(Indent(4) + "public class " + iscript.classes[i].name);
                else
                    tout.Add(Indent(4) + "public class " + iscript.classes[i].name + " : " + iscript.classes[i].baseclass);

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
                tout.Add(Indent(4) + Indent(4) + "public " + ConstantToString(iclass.constants[i]));
            }

            for (int i = 0; i < iclass.enums.Count; i++)
            {
                oelement_names.Add(iclass.enums[i].name);
                tout.Add(Indent(4) + Indent(4) + EnumToString(iclass.enums[i]));
            }

            for (int i = 0; i < iclass.variables.Count; i++)
            {
                oelement_names.Add(iclass.variables[i].name); 
                tout.Add(Indent(4) + Indent(4) + "public " + VarToString(iclass.variables[i]));
            }

            for (int i = 0; i < iclass.properties.Count; i++)
            {
                oelement_names.Add(iclass.properties[i].name); 
                tout.Add(Indent(4) + Indent(4) + "public " + PropertyToString(iclass.properties[i]));
            }

            for (int i = 0; i < iclass.types.Count; i++)
            {
                oelement_names.Add(iclass.types[i].name); 
                tout.Add(Indent(4) + Indent(4) + TypeToString(iclass.types[i]));
            }

            for (int i = 0; i < iclass.functions.Count; i++)
            {
                Function tfunc = iclass.functions[i];
                string tparam_string = "";

                for (int j = 0; j < tfunc.parameters.Count - 1; j++)
                    tparam_string = tparam_string + VarToString(tfunc.parameters[j]);

                if (tfunc.parameters.Count > 0)
                    tparam_string = tparam_string + VarToString(tfunc.parameters[tfunc.parameters.Count-1]).Replace(";","");

                tparam_string.Replace(';', ',');

                //Add method definition
                tout.Add(Indent(4) + Indent(4) + "public " + tfunc.isStatic + " " + tfunc.isVirtual + " " + tfunc.isAbstract + " " + tfunc.returnType + " " + tfunc.name + "(" + tparam_string + ")");
                //tout.Add(Indent(4) + Indent(4) + "{");
                //Add all method variables
                for (int j = 0; j < tfunc.variables.Count; j++)
                    tout.Add(Indent(4) + Indent(4) + VarToString(tfunc.variables[j]));
                //Add method body
                for (int j = 0; j < tfunc.commands.Count; j++)
                    tout.Add(Indent(4) + Indent(4) + Utilities.Beautify_Delphi2CS(tfunc.commands[j]));
                //tout.AddRange(tfunc.commands);
                //tout.Add(Indent(4) + Indent(4) + "}");
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
            return iprop.type + " " + iprop.name + " { get { return " + iprop.read + ";} set { return " + iprop.write + ";} }";
        }

        public string TypeToString(TypeAlias itype)
        {
            return "using " + itype.name + " = " + itype.type + ";";
        }
    }
}