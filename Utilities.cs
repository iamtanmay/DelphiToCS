using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;


namespace Translator
{
    struct Utilities
    {
        public static List<string> TextFileReader(string iPath)
        {
            string[] ttext = new string[0];
            try
            {
                ttext = System.IO.File.ReadAllLines(iPath);
            }
            catch
            {
            }
            return ttext.ToList();
        }

        public static string Beautify_Delphi2CS(string istring)
        {
            istring = istring.Replace("#", "(char)"); //ASCII usage
            istring = istring.Replace("{", "/*");//Comments
            istring = istring.Replace("}", "*/"); 
            istring = istring.Replace("begin", "{");//Scope
            istring = istring.Replace("end;", "}");
            istring = istring.Replace("end.", "}");
            istring = istring.Replace("end", "}");
            istring = istring.Replace("'", "\"");//String quotes
            istring = istring.Replace(" and ", " && ");//Logical
            istring = istring.Replace(" or ", " || ");
            istring = istring.Replace("xor", "^");
            istring = istring.Replace("not", "!");
            istring = istring.Replace("<>", "!=");
            istring = istring.Replace("=", "==");
            istring = istring.Replace("low(Integer)", " 0 * (");//Lower bound
            istring = istring.Replace(":==", "=");//Assignment
            istring = istring.Replace("shr", ">>");
            istring = istring.Replace("shl", "<<");
            istring = istring.Replace("\n", "");//Carriage return
            istring = istring.Replace("boolean ", "bool");//Types
            istring = istring.Replace("Boolean ", "bool");
            istring = istring.Replace("integer ", "int");
            istring = istring.Replace("Integer ", "int");
            istring = istring.Replace("word ", "uint");
            istring = istring.Replace("Word ", "uint");
            istring = istring.Replace("dword ", "uint");
            istring = istring.Replace("Dword ", "uint");
            istring = istring.Replace("shortstring ", "string");
            istring = istring.Replace("Shortstring ", "string");
            istring = istring.Replace("Real ", "double");
            istring = istring.Replace("real ", "double");

            return istring;
        }
    }
}
