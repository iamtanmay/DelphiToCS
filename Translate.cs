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
            List<string> tdelphitext = Utilities.TextFileReader(iPath);
            Script tunit = new Script();
            Delphi tdelphi= new Delphi();
            tdelphi.Read(ref tdelphitext, true, "{--", "--}");
            tunit = tdelphi.script;
        }

        public static void DPK2Vcproj(string iPath)
        {
        }

        public static void Dproj2Vcproj(string iPath)
        {
        }
    }


}
