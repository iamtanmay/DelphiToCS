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

        public static string Beautify(string istring)
        {
            istring = istring.Replace(" and ", " && ");
            istring = istring.Replace(" or ", " || ");
            istring = istring.Replace("xor", "^");
            istring = istring.Replace("=", "==");
            istring = istring.Replace("<>", "!=");
            istring = istring.Replace("shr", ">>");
            istring = istring.Replace("shl", "<<");
            istring = istring.Replace("not", "!");
            istring = istring.Replace("\n", "");
            istring = istring.Replace("boolean", "bool");
            istring = istring.Replace("integer", "int");
            istring = istring.Replace("word", "uint");
            istring = istring.Replace("dword", "uint");
            istring = istring.Replace("shortstring", "string");
            istring = istring.Replace("{", "/*");
            istring = istring.Replace("}", "*/");
            return istring;
        }
    }
}
