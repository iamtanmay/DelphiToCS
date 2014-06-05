using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Translator
{
    struct Translate
    {
        public static void DelphiToCS(string iPath)
        {
            string[] tdelphitext = Utilities.TextFileReader(iPath);
            Script tunit = new Script();
            tunit = Delphi.Read(tdelphitext);
        }

        public static void DPK2Vcproj(string iPath)
        {
        }

        public static void Dproj2Vcproj(string iPath)
        {
        }
    }

    struct Utilities
    {
        public static string[] TextFileReader(string iPath)
        {
            string[] ttext = new string[0];
            try
            {
                ttext = System.IO.File.ReadAllLines(iPath); 
            }
            catch
            {
            }
            return ttext;
        }

        private string Beautify(string istring)
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
            return istring;
        }
    }
}
