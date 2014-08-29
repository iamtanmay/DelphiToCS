using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace Translator
{
    struct Translate
    {

        public static Delphi DelphiToCS(string iPath, string iOutPath, LogDelegate ilog, ref List<string> oglobal_names, ref List<string> oglobals, ref List<string> olocal_names, ref List<string> olocals )
        {
            List<string> tdelphitext = Utilities.TextFileReader(iPath);
            Delphi tdelphi= new Delphi();
            tdelphi.Read(ref tdelphitext, true, "{ --", "-- }", ilog);

            string[] tpath_elements = iOutPath.Split('\\');
            string[] tfilename_elements = iPath.Split('\\');

            string tfilename = tfilename_elements[tfilename_elements.GetLength(0) - 1];

            tfilename = tfilename.Replace(".pas", "");

            string tdirectory = tfilename_elements[tfilename_elements.GetLength(0) - 2].Replace(" ", "_");

            CSharp tcsharp = new CSharp();
            string[] tout = tcsharp.Write(ref tdelphi.script, tdirectory, ref oglobal_names, ref oglobals, ref olocal_names, ref olocals).ToArray();

            //Write to file
            File.WriteAllLines(iOutPath + "\\" + tfilename + ".cs", tout, Encoding.UTF8);

            return tdelphi;
        }

        public static void DPK2Vcproj(string iPath)
        {
        }

        public static void Dproj2Vcproj(string iPath)
        {
        }

        //Append a string to a list of words if found in text
        public static void GlobalsRename(string iappendName, ref List<string> itext, ref List<string> isearchWords)
        {
            for (int i = 0; i < itext.Count; i++)
                for (int j = 0; j < isearchWords.Count; j++)
                    itext[i] = itext[i].Replace(isearchWords[j], iappendName + isearchWords[j]);
        }
    }
}
