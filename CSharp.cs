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
        public List<string> Write(Script iscript, string inamespace)
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
			
			//Write out types
			for( int i=0; i < iscript.includes.Count; i++)
			{
				tout.Add("using " + ConvertToStandardLibrary(iscript.includes[i]));
			}
			
			tout.Add("/*Type Aliases End*/");
			
            //Namespace
			//Global class
				//Consts and enums
			//Local class
				//Consts and enums
			//Classes / Interfaces
				//Inheritance
				//Class variables
				//Properties
		        //Functions
                    //Return type and parameters
                    //Variables
                    //Commands
            return tout;
		}
    }
}