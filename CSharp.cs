using System;
using System.Collections.Generic;

namespace Translator {
    public class CSharp {
        //Substitute standard C# reference for standard reference of other language
        public string ConvertToStandardLibrary(string ilibrary) {
            string tout = ilibrary;
            switch (tout) {
                case "System": return "System";
                case "System.Generics.Collections": return "System.Collections.Generic";
                case "Windows": return "System.Windows";
                case "Forms": return "System.Windows.Forms";
                default: break;
            }
            return tout;
        }

        //Convert a script to C#
        public List<string> Write(ref Script iscript, string inamespace)
		{
			List<string> tout = new List<string>();
			
            //Header
            tout.Add("/**");
            for (int i = 0; i < iscript.header.Count; i++) 
            {
                tout.Add(iscript.header[i]);
            }
            tout.Add("**/");

			//References
			for( int i=0; i < iscript.includes.Count; i++)
			{
				tout.Add("using " + ConvertToStandardLibrary(iscript.includes[i]) + ";");
			}
			
			tout.Add("/*References End*/");
			
            ////Write out types
            //for( int i=0; i < iscript.includes.Count; i++)
            //{
            //    tout.Add("using " + ConvertToStandardLibrary(iscript.includes[i]));
            //}
			
			//tout.Add("/*Type Aliases End*/");

            tout.Add("namespace " + inamespace);
            tout.Add("{");

            for (int i = 0; i < iscript.classes.Count; i++)
            {
                tout.AddRange(WriteClass(iscript.classes[i]));
            }

            tout.Add("}");
            return tout;
        }        

        public List<string> WriteClass(Class iclass)
        {
            List<string> tout = new List<string>();

            //Classes / Interfaces
            //Inheritance

            if (iclass.baseclass == "")
                tout.Add("public class " + iclass.name);
            else
                tout.Add("public class " + iclass.name + " : " + iclass.baseclass);

            tout.Add("{");

            for (int i = 0; i < iclass.constants.Count; i++)
            {
                tout.Add("public " + ConstantToString(iclass.constants[i]));
            }

            for (int i = 0; i < iclass.enums.Count; i++)
            {
                tout.Add(EnumToString(iclass.enums[i]));
            }

            for (int i = 0; i < iclass.variables.Count; i++)
            {
                tout.Add("public " + VarToString(iclass.variables[i]));
            }

            for (int i = 0; i < iclass.properties.Count; i++)
            {
                tout.Add("public " + PropertyToString(iclass.properties[i]));
            }

            for (int i = 0; i < iclass.types.Count; i++)
            {
                tout.Add(TypeToString(iclass.types[i]));
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
                tout.Add("public " + tfunc.isStatic + " " + tfunc.isVirtual + " " + tfunc.isAbstract + " " + tfunc.returnType + " " + tfunc.name + "(" + tparam_string + ")");
                tout.Add("{");
                //Add all method variables
                for (int j = 0; j < tfunc.variables.Count; j++)
                    tout.Add(VarToString(tfunc.variables[j]));
                //Add method body
                tout.AddRange(tfunc.commands);
                tout.Add("}");
            }

            tout.Add("}");
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

        public string TypeToString(Type itype)
        {
            return "using " + itype.name + " = " + itype.type + ";";
        }
    }
}