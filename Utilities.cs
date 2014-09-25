using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using System.Text.RegularExpressions;


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

        public static string Delphi2CSRules(string istring)
        {
            //New object creation
            int tindex = istring.IndexOf(".Create(");
            if (tindex != -1)
            {
                string[] tarr = istring.Split(' ');

                tindex = tarr[0].IndexOf(".Create(");
                int i = 0;
                while (tindex == -1)
                {
                    i++;
                    tindex = tarr[i].IndexOf(".Create(");
                }
                tarr[i] = "new " + tarr[i].Replace(".Create", "");
                istring = "";

                for (i = 0; i < tarr.GetLength(0); i++)
                {
                    istring = istring + " " + tarr[i];
                }
            }
            return istring;
        }

        public static string[] Delphi_CSRegexTypes = {"TList", "boolean", "Boolean", "integer", "Integer", 
                                                    "word", "Word", "dword", "Dword", "shortstring", "Shortstring", 
                                                    "Real", "real", "{", "}", "begin", "end;", "end.", "Double"},

            Delphi_CSRegexTypesSubstitutes = { "List", "bool", "bool", "int", "int", 
                                            "uint", "uint", "uint", "uint", "string", "string", 
                                            "double", "double", "/*", "*/", "{", "}", "", "double" },

            Delphi_CSNonRegexTypes = { "#", "'", "and ", "or ", 
                                  "xor ", "not ", "=", "low(Integer)", "EXIT(", ":==", "<>", "shr", "shl", 
                                  "\n", "if", "then", "FreeAndNil", "TList<", "self.", "(self)", ".Delete(", "end;"},

            Delphi_CSNonRegexSubstitutes = { "(char)", "\\", "&& ", "|| ", 
                                               "^ ", "! ", "==", "0*", "result = (", "=", "!=", ">>", "<<", 
                                               "", "if(", ")", "//FreeAndNil", "List<", "", "(this)", ".RemoveAt(", "" };

        public static string ReplaceElementsInStringRegex(string istring, ref string[] iElementList, ref string[] iSubstitutesList)
        {
            string pattern = "";

            for (int i = 0; i < iElementList.GetLength(0); i++)
            {
                pattern = "\\b" + iElementList[i] + "\\b";
                istring = Regex.Replace(istring, pattern, iSubstitutesList[i]);
            }
            return istring;
        }
        public static string ReplaceElementsInStringSimple(string istring, ref string[] iElementList, ref string[] iSubstitutesList)
        {
            for (int i = 0; i < iElementList.GetLength(0); i++)
            {
                istring = istring.Replace(iElementList[i], iSubstitutesList[i]);
            }
            return istring;
        }

        public static string Beautify_Delphi2CS(string istring)
        {
            istring = ReplaceElementsInStringRegex(istring, ref Delphi_CSRegexTypes, ref Delphi_CSRegexTypesSubstitutes);
            istring = ReplaceElementsInStringSimple(istring, ref Delphi_CSNonRegexTypes, ref Delphi_CSNonRegexSubstitutes);

            //string pattern = "";

            //istring = istring.Replace("#", "(char)"); //ASCII usage

            //istring = istring.Replace("{", "/*");//Comments
            //istring = istring.Replace("}", "*/"); 

            //istring = istring.Replace("begin", "{");//Scope
            //istring = istring.Replace("end;", "}");
            //istring = istring.Replace("end.", "");
            //istring = istring.Replace("end", "}");
            
            //istring = istring.Replace("'", "\"");//String quotes
            
            //istring = istring.Replace(" and ", " && ");//Logical
            //istring = istring.Replace(" or ", " || ");
            //istring = istring.Replace("xor", "^");
            //istring = istring.Replace("not", "!");
            //istring = istring.Replace("=", "==");
            
            //istring = istring.Replace("low(Integer)", " 0 * (");//Lower bound
            
            //pattern = @"\bresult\b";
            //string tstr = Regex.Replace(istring, pattern, "return");//Method return
            //if (istring != tstr)
            //    istring = tstr.Replace(":==", "").Replace("  "," ");

            //istring = istring.Replace("EXIT", "return");//Method return

            //istring = istring.Replace(":==", "=");//Assignment

            //istring = istring.Replace("<>", "!=");//Logical
            
            //istring = istring.Replace("shr", ">>");//Byte shift
            //istring = istring.Replace("shl", "<<");

            //istring = istring.Replace("\n", "");//Carriage return

            //istring = istring.Replace("if", "if(");//Conditional
            //istring = istring.Replace("then", ")");//Conditional

            //istring = istring.Replace("FreeAndNil", "//FreeAndNil");//Memory deallocation

            //istring = istring.Replace("boolean ", "bool");//Types
            //istring = istring.Replace("Boolean ", "bool");
            //istring = istring.Replace("integer ", "int");
            //istring = istring.Replace("Integer ", "int");
            //istring = istring.Replace("word ", "uint");
            //istring = istring.Replace("Word ", "uint");
            //istring = istring.Replace("dword ", "uint");
            //istring = istring.Replace("Dword ", "uint");
            //istring = istring.Replace("shortstring ", "string");
            //istring = istring.Replace("Shortstring ", "string");
            //istring = istring.Replace("Real ", "double");
            //istring = istring.Replace("real ", "double");

            return istring;
        }
    }
}
