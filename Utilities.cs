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
            else
            {
                //Casting
                string tstring = istring.Trim();
                tindex = tstring.IndexOf(".VInteger");
                if (tindex != -1)
                {
                    tstring = tstring.Remove(tindex, 9);
                    int tprevwordstart = tindex;
                    char tprevletter = tstring[tprevwordstart];
                    while (tprevletter != '(' & tprevletter!= ' ' & tprevwordstart > 0)
                    {
                        tprevwordstart--;
                        tprevletter = tstring[tprevwordstart];
                    }
                    tstring = tstring.Insert(tprevwordstart+1, "(int)");
                    istring = tstring;
                }
                else
                {
                    tindex = tstring.IndexOf(".VExtended");
                    if (tindex != -1)
                    {
                        tstring = tstring.Remove(tindex, 10);
                        int tprevwordstart = tindex;
                        char tprevletter = tstring[tprevwordstart];
                        while (tprevletter != '(' & tprevletter != ' ' & tprevwordstart > 0)
                        {
                            tprevwordstart--;
                            tprevletter = tstring[tprevwordstart];
                        }
                        tstring = tstring.Insert(tprevwordstart + 1, "(double)");
                        istring = tstring;
                    }
                    else
                    {
                        tindex = tstring.IndexOf(".VPWideChar");
                        if (tindex != -1)
                        {
                            tstring = tstring.Remove(tindex, 11);
                            int tprevwordstart = tindex;
                            char tprevletter = tstring[tprevwordstart];
                            while (tprevletter != '(' & tprevletter != ' ' & tprevwordstart > 0)
                            {
                                tprevwordstart--;
                                tprevletter = tstring[tprevwordstart];
                            }
                            tstring = tstring.Insert(tprevwordstart + 1, "(string)");
                            istring = tstring;
                        }
                    }
                }
            }
            return istring;
        }

        public static string[] Delphi_CSRegexTypes = {"TList", "boolean", "Boolean", "integer", "Integer", "Longint",
                                                    "word", "Word", "dword", "Dword", "shortstring", "Shortstring", 
                                                    "Real", "real", "begin", "end;", "end.", "Double", "extended", "Extended", 
                                                    "string", "var", "do", "End;", "Copy", "copy", "int64", "Int64", "variant", "ansistring", "widestring", "unicodestring", "TObject",
                                                     "try", "finally", "end", "PWideChar"},

            Delphi_CSRegexTypesSubstitutes = { "List", "bool", "bool", "int", "int", "int",
                                            "uint", "uint", "uint", "uint", "string", "string", 
                                            "double", "double", "{", "}", "", "double", "double", "double", 
                                            "delphistring", "ref", ")", "}", "WrapperUtilities.Copy", "WrapperUtilities.Copy", "long", "long", "System.Object", "delphistring", "delphistring", "delphistring", "object",
                                             "try{", "}finally{", "}", "delphistring"},

            Delphi_CSNonRegexTypes = { "#", "'", " and ", " or ", 
                                  "xor ", "not ", "=", "low(Integer)", "EXIT(", "EXIT;", ":==", "<>", "shr", "shl", 
                                  "\n", "if", "then", "SysUtils.FreeAndNil", "FreeAndNil", "TList<", "self.", "(self)", ".Delete(", "end;", "Assigned(", "CompareStr", "CompareText", "Result", "function", "for ", 
                                  "high(", "IntToStr", "FloatToStr", ".VType", "vtInteger", "vtInt64", "vtUnicodeString", "vtExtended", "^", "integer", "Integer", "BREAK", "SetLength(", "Copy(", "copy(", 
                                  "length(", "Length(", "Pos(", "SysLocale", "StrToFloat", "StrToInt", "<boolean", "boolean>", " FormatSettings", ".ClassName", ".UnitName", "Unassigned", 
                                  "VarIsEmpty", "FileExists", "FileCreate", "FileClose", "nil", "SameText", " variant", 
                                  "StrAlloc", "FillChar", "SHGetSpecialFolderPath", "StrDispose", "MAX_PATH", "CSIDL_", " TArray",
                                     "FindFirst("},

            Delphi_CSNonRegexSubstitutes = { "(char)", "\"", " && ", " || ", 
                                               "^ ", "! ", "==", "0*", "result = (", "return;", "=", "!=", ">>", "<<", 
                                               "", "if(", ")", "//SysUtils.FreeAndNil", "//FreeAndNil", "List<", "", "(this)", ".RemoveAt(", "}", "WrapperUtilities.Assigned(", "delphistring.CompareDelphiString", "delphistring.CompareDelphiString", "result", "", "for (", 
                                               "ArrayUtilities.high(", "StringUtils.IntToStr", "StringUtils.FloatToStr", ".GetType().Name", "\"int\"", "\"long\"", "\"delphistring\"", "\"double\"", "", "int", "int", "break", "WrapperUtilities.SetLen( ref ", "WrapperUtilities.Copy(", "WrapperUtilities.Copy(", 
                                               "WrapperUtilities.length(", "WrapperUtilities.Length(", "WrapperUtilities.Pos(", "SysUtils.Instance.SysLocale", "WrapperUtilities.StrToFloat", "WrapperUtilities.StrToInt", "<bool", "bool>", " SysUtils.Instance.FormatSettings", ".GetType().Name", ".GetType().Namespace", "null", 
                                               "WrapperUtilities.VarIsEmpty", "WrapperUtilities.FileExists", "WrapperUtilities.FileCreate", "WrapperUtilities.FileClose", "null", "WrapperUtilities.SameText", " System.Object",
                                           "WrapperUtilities.StrAlloc", "WrapperUtilities.FillChar", "WrapperUtilities.SHGetSpecialFolderPath", "WrapperUtilities.StrDispose", "0", "(int)WrapperUtilities.CSIDL.CSIDL_", " DelphiStandardWrapper.TArray",
                                           "WrapperUtilities.FindFirst("};

        public static string ReplaceElementsInStringRegex(string istring, ref string[] iElementList, ref string[] iSubstitutesList)
        {
            string pattern = "";
            string treturn = "";
            string[] twords = istring.Trim().Split(' ');

            for (int j = 0; j < twords.Length; j++)
            {
                string tstring = twords[j].Trim();

                for (int i = 0; i < iElementList.GetLength(0); i++)
                {
                    if (tstring == iElementList[i])
                        tstring = iSubstitutesList[i];
                    //pattern = "\\b" + iElementList[i] + "\\b";
                    //istring = Regex.Replace(istring, pattern, iSubstitutesList[i]);
                }
                treturn = treturn + " " + tstring;
            }
            return treturn;
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
            if (istring == null)
                return "";
            else if (istring == "")
                return "";

            //Change comments from { to //
            istring = istring.Replace("{", "/*").Replace("}", "*/");

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
