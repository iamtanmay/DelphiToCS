using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace Translator
{
    struct Translate
    {
        public static void DelphiToCS(string iPath, string iOutPath, LogDelegate ilog)
        {
            List<string> tdelphitext = Utilities.TextFileReader(iPath);
            Delphi tdelphi= new Delphi();
            tdelphi.Read(ref tdelphitext, true, "{ --", "-- }", ilog);

            string[] tpath_elements = iOutPath.Split('\\');
            string[] tfilename_elements = iPath.Split('\\');

            string tfilename = tfilename_elements[tfilename_elements.GetLength(0) - 1];

            tfilename = tfilename.Replace(".pas", "");

            string tdirectory = tpath_elements[tpath_elements.GetLength(0)];

            CSharp tcsharp = new CSharp();
            string[] tout = tcsharp.Write(ref tdelphi.script, tdirectory).ToArray();

            //Write to file
            File.WriteAllLines(iOutPath + "\\" + tfilename + ".cs", tout, Encoding.UTF8);
        }

        public static void DPK2Vcproj(string iPath)
        {
        }

        public static void Dproj2Vcproj(string iPath)
        {
        }
    }


}
