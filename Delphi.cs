using System;
using System.Text;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Windows.Forms;

namespace Translator
{
    [Serializable()]
    public class DelphiVarStrings
    {
        public string value;
        public bool isStatic;
        
        public DelphiVarStrings()
        { }
        public DelphiVarStrings(string ivalue, bool iisStatic)
        {
            value = ivalue;
            isStatic = iisStatic;
        }
    }

    [Serializable()]
    public class DelphiClassStrings
    {
        public string name;
        public string type;
        public string baseclass;

        public List<DelphiMethodStrings> methods;
        public List<List<DelphiMethodStrings>> actions = new List<List<DelphiMethodStrings>>();
        public List<DelphiVarStrings> properties;
        public List<DelphiVarStrings> variables;
        public List<DelphiVarStrings> consts;

        public DelphiClassStrings()
        {
            methods = new List<DelphiMethodStrings>();
            properties = new List<DelphiVarStrings>();
            variables = new List<DelphiVarStrings>();
            consts = new List<DelphiVarStrings>();
        }

        public void AddMethodDefinition(string iheader, bool iAbstract, bool iVirtual, bool iOverloaded)
        {
            DelphiMethodStrings tmethod = new DelphiMethodStrings(iheader, null);
            tmethod.isAbstract = iAbstract;
            tmethod.isVirtual = iVirtual;
            tmethod.isOverloaded = iOverloaded;
        }

        public void AddAction(string iheader, List<string> ibody, int ivar_start, int iconst_start, int ibegin_start)
        {
            DelphiMethodStrings tmethod = new DelphiMethodStrings("", null);

            for (int i = 0; i < methods.Count; i++)
                if (methods[i].header == iheader)
                {
                    tmethod.header = ibody[0];
                    tmethod.body = ibody.GetRange(1,ibody.Count-1);
                    tmethod.const_start = iconst_start;
                    tmethod.var_start = ivar_start;
                    tmethod.begin_start = ibegin_start;
                    actions[i].Add(tmethod);
                }
        }

        public void AddMethodBody(string iheader, List<string> ibody, int ivar_start, int iconst_start, int ibegin_start, int inextpos)
        {
            for (int i = 0; i < methods.Count; i++)
                if (methods[i].header == iheader)
                {
                    methods[i].body = ibody;
                    methods[i].const_start = iconst_start;
                    methods[i].var_start = ivar_start;
                    methods[i].begin_start = ibegin_start;
                    methods[i].nextpos = inextpos;
                }
        }
    }

    [Serializable()]
    public class DelphiMethodStrings
    {
        public string header ="";
        public bool isAbstract = false, isVirtual = false, isOverloaded = false, isStatic = false;
        public int nextpos = -1;

        public List<string> body;
        public int const_start, var_start, begin_start;
        public DelphiMethodStrings()
        { }
        public DelphiMethodStrings(string iheader, List<string> ibody)
        {
            header = iheader;
            body = new List<string>();

            if (ibody != null)
                body = ibody;
            
            const_start = -1;
            var_start = -1;
            nextpos = -1;
        }
    }


    public class Delphi
    {
    	//Output
        [System.Xml.Serialization.XmlAttributeAttribute("Name")]
        public string name;
        [System.Xml.Serialization.XmlAttributeAttribute("Directory")]
        public string directory;
        [System.Xml.Serialization.XmlAttributeAttribute("OutPath")]
        public string outPath;

        public Script script;

        [System.Xml.Serialization.XmlIgnoreAttribute]
        public s form;

    	//Enums, consts, types
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public Class classLocals, classGlobals;
        
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public List<Constant> localConsts, globalConsts;
        
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public List<Enum> localEnums, globalEnums;
        
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public List<TypeAlias> localTypes, globalTypes;

        [System.Xml.Serialization.XmlIgnoreAttribute]
        public List<string> interfaceUses, implementationUses;
        
    	//Log 
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public LogDelegate logsingle;
    	
        //Bookmarks
    	//Sections
        [System.Xml.Serialization.XmlIgnoreAttribute]
        private int startHeader = -1, endHeader = -1, startInterface = -1, endInterface = -1, startVar = -1, endVar = -1, startImplementation = -1, endImplementation = -1;    	

    	//Subsections
        [System.Xml.Serialization.XmlIgnoreAttribute]
        private List<int> startUses, endUses, startClassInterface, endClassInterface, startClassImplementation, endClassImplementation, startConsts, endConsts, startEnums, endEnums, startTypes, endTypes;
    	
    	
    	//Raw strings
    	//Header, class names, SubSection names, Uses interface, Uses implementation, (Global and Local): consts, enums, types and vars 
        [System.Xml.Serialization.XmlIgnoreAttribute]
        private List<string> classNames, SubSection_Names, Uses_Interface, Uses_Implementation, ConstsGlobal, EnumsGlobal, TypesGlobal, VarsGlobal, ConstsLocal, EnumsLocal, TypesLocal, VarsLocal;

    	//Raw text for the classes
        [System.Xml.Serialization.XmlIgnoreAttribute]
        private List<DelphiClassStrings> classDefinitions;
        
        [System.Xml.Serialization.XmlIgnoreAttribute]
        private List<List<string>> classImplementations;

    	
    	//Keywords
        //Divide script sections
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public static string[] sectionKeys = { "var", "implementation", "interface" };

    	//Divide section subsections
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public static string[] subsectionKeys = { "type", "const", "uses" };

        //Divide subsection kinds
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public static string[] interfaceKindKeys = { "class", "record", "procedure", "function", "interface" };
        
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public static string[] implementKindKeys = { "uses", "strict private", "public const", "public", "private const", "private", "protected", "destructor", "constructor", "class var", "class function", "class procedure", "class property", "procedure", "function", "property", "class const", "const" };
        
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public static string[] const_implementKindKeys = { "uses", "strict private", "public const", "public", "private const", "private", "protected", "destructor", "constructor", "class var", "class function", "class procedure", "class property", "procedure", "function", "property", "class const", "const", "var", "begin" };

        //Divide method commands
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public static string[] methodKeys = { "var", "begin", "label", "end", "try", "catch", "finally" };
    	
        //Divide class defintion
        [System.Xml.Serialization.XmlIgnoreAttribute]
        public static string[] classKeys = { "strict private", "public const", "public", "private const", "private", "protected", "destructor", "constructor", "class var", "class function", "class procedure", "class property", "procedure", "function", "property", "class const", "const" };

        [System.Xml.Serialization.XmlIgnoreAttribute]
        List<int> Section_Bookmarks, EnumLocalStarts, EnumLocalEnds, EnumGlobalEnds;

        [System.Xml.Serialization.XmlIgnoreAttribute]
        List<string> Section_Names;
        
        public Delphi()
        {
        }

    	//ireadheader is a flag if header is to be read. iheaderstart is normally "{ --" in Zinsser Delphi units, and iheaderend is "-- }" 
        public void Read(ref List<string> istrings, bool ireadheader, string iheaderstart, string iheaderend, LogDelegate ilog, ref s tform)
        {
        	//Initialise
            form = tform;
            script = new Script();
            logsingle = ilog;

            name = "";
            
            classNames = new List<string>();
            classDefinitions = new List<DelphiClassStrings>();
            classImplementations = new List<List<string>>();

            SubSection_Names = new List<string>();
            Uses_Interface = new List<string>();
            Uses_Implementation = new List<string>();

            ConstsGlobal = new List<string>();
            EnumsGlobal = new List<string>();
            TypesGlobal = new List<string>();
            VarsGlobal = new List<string>();            
            ConstsLocal = new List<string>();
            EnumsLocal = new List<string>();
            TypesLocal = new List<string>();
            VarsLocal = new List<string>();
            
            SubSection_Names = new List<string>();
            Section_Bookmarks = new List<int>();
            Section_Names = new List<string>();
 	
            startClassInterface = new List<int>();
            endClassInterface = new List<int>(); 
            startClassImplementation = new List<int>(); 
            endClassImplementation = new List<int>();            
            startUses = new List<int>();
            endUses = new List<int>();            
            startConsts = new List<int>();
            endConsts = new List<int>();             
            EnumLocalStarts = new List<int>();
            EnumLocalEnds = new List<int>();           
            startEnums = new List<int>();
            EnumGlobalEnds = new List<int>();           
            startTypes = new List<int>();
            endTypes = new List<int>();

            startHeader =  endHeader =  startInterface =  endInterface =  startImplementation =  endImplementation = -1;
            
            //Bookmark sections
            IndexStructure(ref istrings, ireadheader, iheaderstart, iheaderend);

            //Break up the text into Lists of strings for different parts
            if (ireadheader)
    			ParseHeader(ref istrings, ireadheader, iheaderstart, iheaderend);

            //Read unit name
            name = ((istrings[FindStringInList("unit", ref istrings, 0, true)].Replace(' ', ';')).Split(';'))[1];
            
            //Convert Delphi symbols to C# symbols
			//Beautify(ref istrings);

            ParseInterface(ref istrings, ref Uses_Interface, ref classNames, ref classDefinitions, ref ConstsGlobal, ref EnumsGlobal, ref TypesGlobal);
            ParseVar(ref istrings, ref VarsGlobal);
            ParseImplementation(ref istrings, ref Uses_Implementation, ref classNames, ref classDefinitions, ref ConstsLocal, ref EnumsLocal, ref TypesLocal);

            //Generate the text pieces into class objects
            GenerateGlobalClass("Globals_"+ name, ref classGlobals, ref ConstsGlobal, ref EnumsGlobal, ref TypesGlobal, ref VarsGlobal);
            script.classes.Add(classGlobals);
            VarsGlobal.Clear();

            GenerateGlobalClass("Locals_"+ name, ref classLocals, ref ConstsLocal, ref EnumsLocal, ref TypesLocal, ref VarsGlobal);
            script.classes.Add(classLocals);

            GenerateClasses(ref istrings, ref script.classes, ref classNames, ref classDefinitions, ref classImplementations);
            GenerateIncludes(ref interfaceUses, ref Uses_Interface);
            GenerateIncludes(ref implementationUses, ref Uses_Implementation);
            
            //Add both uses into one
            script.includes.AddRange(interfaceUses);
            script.includes.AddRange(implementationUses);
        }
        
        private void ParseHeader(ref List<string> istrings, bool ireadheader, string iheaderstart, string iheaderend)
        {
			 //Read Header
            if (ireadheader)
            	if (startHeader != -1)
	            	if (endHeader != -1)
                        for (int i = startHeader; i < endHeader; i++)
                        {
                            script.header.Add(istrings[i]);
                            istrings.RemoveAt(i);
                            istrings.Insert(i, "");
                        }
	            	else
		            	throw new Exception("Header end not found");
        }
    	
        private void Beautify(ref List<string> istrings)
        {
        	for (int i = 0; i < istrings.Count; i++)
                istrings[i] = Utilities.Beautify_Delphi2CS(istrings[i]);
        }
        
        //oclassdefinitions is a list of class variables, properties, function and procedure definitions 
        private void ParseInterface(ref List<string> istrings, ref List<string> ouses, ref List<string> oclassnames, ref List<DelphiClassStrings> oclassdefinitions, ref List<string> oconsts, ref List<string> oenums, ref List<string> otypes)
        {
            int tcurr_string_count = FindNextInterfaceSubSection(ref istrings, startInterface), tnext_subsection_pos = -1;

            //Interface sub sections
            while (tcurr_string_count != -1)
            {
            	tnext_subsection_pos = FindNextInterfaceSubSection(ref istrings, tcurr_string_count + 1);

                //if (tnext_subsection_pos == -1)
                //    tnext_subsection_pos = endInterface;

            	switch (RecognizeKey(istrings[tcurr_string_count], ref subsectionKeys))
            	{
                    case "const":   int tendRange = tnext_subsection_pos;
                                    
                                    if (tendRange == -1)
                                        tendRange = endInterface;
                                    
                                    //oconsts.AddRange(GetStringSubList(ref istrings, tcurr_string_count + 1, tendRange)); break;

                                    string tconststr = "", ttempstr = "";
                                    for (int j = tcurr_string_count + 1; j < tendRange && j < istrings.Count; j++)
                                    {
                                        tconststr = istrings[j];

                                        ttempstr = istrings[j].Trim();

                                        if (ttempstr != "")
                                        {
                                            if (ttempstr[0] == '{')
                                            {
                                                //Ignore comments containing class name
                                                while (ttempstr.IndexOf('}') == -1)
                                                {
                                                    j++;
                                                    ttempstr = istrings[j].Trim();
                                                }
                                            }
                                            else if (ttempstr != "" && ttempstr[0] != '/')
                                            {
                                                bool tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');

                                                while (!tsemicolonfound && (ttempstr.IndexOf("//") == -1))
                                                {
                                                    j++;
                                                    ttempstr = istrings[j].Trim();
                                                    tconststr = tconststr + istrings[j];

                                                    if (ttempstr != "")
                                                        tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');
                                                    else
                                                        tsemicolonfound = false;
                                                }

                                                oconsts.Add(tconststr.Replace("=", ":="));
                                            }
                                        }
                                    }
                                    break;



                    case "type":    tendRange = tnext_subsection_pos;

                                    if (tendRange == -1)
                                        tendRange = endInterface; 
                                    
                                    ParseInterfaceTypes(ref istrings, ref ouses, ref oclassnames, ref oclassdefinitions, ref oconsts, ref oenums, ref otypes, tcurr_string_count, tendRange); break;

                    case "uses":    tendRange = tnext_subsection_pos;

                                    if (tendRange == -1)
                                        tendRange = endInterface;

                                    ouses.AddRange(GetStringSubList(ref  istrings, tcurr_string_count + 1, tendRange)); break;

        			//Log unrecognized sub section
        			default: 		List<string> tlogmessages = new List<string>();
        							tlogmessages.Add("Interface sub section not recognized " + tcurr_string_count);
        							tlogmessages.AddRange(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos));
        							log(tlogmessages);
        							break;
            	}
                tcurr_string_count = tnext_subsection_pos;
            }        
        }

        //Check if a line in class definition is a variable, if it has only two words, divided by ' ' or ':' and if its first character is 'f' or 'F'
        private bool CheckVariable(string istring)
        {
            string[] tarray = istring.Trim().Split(' ');

            if ((tarray.GetLength(0) >= 2) && ((tarray[0][0] == 'f') || (tarray[0][0] == 'F')) && (istring.Split(':').GetLength(0) == 2))
                return true;
            return false;
        }

        private bool CheckRecordVariable(string istring)
        {

            string[] tarray = istring.Trim().Split(@"//".ToCharArray());
            istring = tarray[0];
            tarray = istring.Trim().Split(' ');

            if ((tarray.GetLength(0) >= 2) && (istring.Split(':').GetLength(0) == 2))
                return true;
            return false;
        }

        private void ParseInterfaceTypes(ref List<string> istrings, ref List<string> ouses, ref List<string> oclassnames, ref List<DelphiClassStrings> oclassdefinitions, ref List<string> oconsts, ref List<string> oenums, ref List<string> otypes, int istartpos, int inext_subsection_pos)
        {
            int tnext_subsection_pos = inext_subsection_pos, tcurr_string_count = istartpos;
            string tclassname = "";

            if (tnext_subsection_pos == -1)
                tnext_subsection_pos = endInterface;
            if (tnext_subsection_pos == -1)
                tnext_subsection_pos = istrings.Count;

            //Interface sub sectionsm
            while ((tcurr_string_count < tnext_subsection_pos) && (tcurr_string_count < endInterface) && (tcurr_string_count != -1))
            {
                //Ignore comments and empty lines
                if ((istrings[tcurr_string_count] != "") && !(RecognizeComment(istrings[tcurr_string_count])) && !(RecognizeEmptyLine(istrings[tcurr_string_count])))
                {
                    DelphiClassStrings tclassdef = new DelphiClassStrings();
                    switch (RecognizeKey(istrings[tcurr_string_count], ref interfaceKindKeys))
                    {
                        case "class":   //Check if it is a forward declaration or alias
                                        string tstring = istrings[tcurr_string_count];
                                        if (tstring.IndexOf(";") != -1)
                                        {
                                            //Alias, just make an empty class with this baseclass
                                            if ((tstring.IndexOf("class(") != -1) || (tstring.IndexOf("class of") != -1))
                                            {
                                                string[] ttemparr = (Regex.Replace(tstring, @"\s+", "")).Split('='); 
                                                tclassname = ttemparr[0];                                                
                                                tclassdef.name = tclassname;
                                                tclassdef.type = "c";
                                                tclassdef.baseclass = ttemparr[1].Replace("class(", "").Replace(");", "");
                                                oclassnames.Add(tclassname); 
                                                oclassdefinitions.Add(tclassdef);
                                                tcurr_string_count++;
                                                tnext_subsection_pos = tcurr_string_count + 1;// Its only one line
                                            }
                                            //Forward declaration, ignore
                                            else
                                            {
                                                tcurr_string_count = tnext_subsection_pos + 1;
                                                //tcurr_string_count = FindNextInterfaceSubType(ref istrings, tcurr_string_count + 1);
                                            }
                                        }
                                        else
                                        { 
                                            tnext_subsection_pos = FindNextSymbol(ref istrings, "end;", tcurr_string_count);
                                            tclassname = ((Regex.Replace(istrings[tcurr_string_count], @"\s+", "")).Split('='))[0];

                                            if (tnext_subsection_pos == -1)
                                                throw new Exception("Incomplete class definition");

                                            tclassdef = ParseInterfaceClass(ref istrings, tclassname, tcurr_string_count, tnext_subsection_pos);
                                            tclassdef.type = "c";
                                            tclassdef.name = tclassname;

                                            oclassnames.Add(tclassname);
                                            oclassdefinitions.Add(tclassdef);
                                            tcurr_string_count = tnext_subsection_pos + 1;
                                            //tcurr_string_count = FindNextInterfaceSubType(ref istrings, tcurr_string_count + 1);
                                        }

                                        break;
                        case "interface":   //Check if it is a forward declaration or alias
                                        tstring = istrings[tcurr_string_count];
                                        if (tstring.IndexOf(";") != -1)
                                        {
                                            //Alias, just make an empty class with this baseclass
                                            if (tstring.IndexOf("interface(") != -1)
                                            {
                                                string[] ttemparr = (Regex.Replace(tstring, @"\s+", "")).Split('=');
                                                tclassname = ttemparr[0];
                                                tclassdef.name = tclassname;
                                                tclassdef.baseclass = ttemparr[1].Replace("interface(", "").Replace(");", "");
                                                tclassdef.type = "i";
                                                oclassnames.Add(tclassname);
                                                oclassdefinitions.Add(tclassdef);
                                                tcurr_string_count++;
                                                tnext_subsection_pos = tcurr_string_count + 1;// Its only one line
                                            }
                                            //Forward declaration, ignore
                                            else
                                            {
                                                tcurr_string_count = tnext_subsection_pos + 1;
                                                //tcurr_string_count = FindNextInterfaceSubType(ref istrings, tcurr_string_count + 1);
                                            }
                                        }
                                        else
                                        {
                                            tnext_subsection_pos = FindNextSymbol(ref istrings, "end;", tcurr_string_count);
                                            tclassname = ((Regex.Replace(istrings[tcurr_string_count], @"\s+", "")).Split('='))[0];

                                            if (tnext_subsection_pos == -1)
                                                throw new Exception("Incomplete interface definition");

                                            tclassdef = ParseInterfaceClass(ref istrings, tclassname, tcurr_string_count, tnext_subsection_pos);
                                            tclassdef.name = tclassname;
                                            tclassdef.type = "i";
                                            oclassnames.Add(tclassname);
                                            oclassdefinitions.Add(tclassdef);
                                            tcurr_string_count = tnext_subsection_pos + 1;
                                            //tcurr_string_count = FindNextInterfaceSubType(ref istrings, tcurr_string_count + 1);
                                        }

                                        break;

                        case "record":  tnext_subsection_pos = FindNextSymbol(ref istrings, "end;", tcurr_string_count);
                                        tclassname = ((Regex.Replace(istrings[tcurr_string_count], @"\s+", "")).Split('='))[0];

                                        if (tnext_subsection_pos == -1)
                                            throw new Exception("Incomplete class definition");
                                        
                                        tclassdef = ParseRecord(ref istrings, tclassname, tcurr_string_count, tnext_subsection_pos);
                                        tclassdef.name = tclassname;
                                        tclassdef.type = "r";
                                        oclassnames.Add(tclassname);
                                        oclassdefinitions.Add(tclassdef);
                                        tcurr_string_count = tnext_subsection_pos + 1;
                                        //tcurr_string_count = FindNextInterfaceSubType(ref istrings, tcurr_string_count + 1);
                                        break;

                        //Check if Enum or Type Alias, else Log unrecognized sub section
                        default:        //Enum start
                                        if (istrings[tcurr_string_count].IndexOf('(') != -1)
                                        {
                                            tnext_subsection_pos = FindNextSymbol(ref istrings, ");", tcurr_string_count);

                                            if (tnext_subsection_pos == -1)
                                            {
                                                tcurr_string_count++;
                                                throw new Exception("Incomplete Enum definition");
                                            }
                                            else
                                            {
                                                oenums.Add(string.Concat((GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos+1).ToArray())));
                                                tcurr_string_count++;// = tnext_subsection_pos + 1;
                                            }
                                            //tcurr_string_count = FindNextInterfaceSubType(ref istrings, tcurr_string_count + 1);
                                        }
                                        //Type Alias start
                                        else if ((istrings[tcurr_string_count].IndexOf('=') != -1) && (istrings[tcurr_string_count].IndexOf(';') != -1))
                                        {
                                            //tnext_subsection_pos = FindNextSymbol(ref istrings, ";", tcurr_string_count);

                                            //if (tnext_subsection_pos == -1)
                                            //    throw new Exception("Incomplete Type Alias definition");

                                            if (istrings[tcurr_string_count].IndexOf("set of") != -1)
                                                otypes.Add(istrings[tcurr_string_count].Replace("set of", "HashSet<").Replace(";", ">;"));
                                            else if (istrings[tcurr_string_count].IndexOf("set of") != -1)
                                                otypes.Add(istrings[tcurr_string_count].Replace("array of ", "").Replace(";", "[];"));
                                            else
                                                otypes.Add(istrings[tcurr_string_count]);

                                            tcurr_string_count++;                                              
                                        }
                                        //Unrecognized line logged
                                        else
                                        {
                                            List<string> tlogmessages = new List<string>();
                                            tlogmessages.Add("Interface sub section not recognized " + tcurr_string_count);
                                            tlogmessages.Add(istrings[tcurr_string_count]);
                                            log(tlogmessages);
                                            tcurr_string_count++;
                                        }
                                        break;
                    }
                }
                else
                {
                    tcurr_string_count++;
                }
            }
        }

        private int FindEndOfFunctionTitle(ref List<string>  iFiltered_strings, int icurr_string_count)
        {
            bool end_found = false;
            int i = icurr_string_count;
            int bracket_closed = -1;
            while (!end_found && i < iFiltered_strings.Count)
            {
                bracket_closed = iFiltered_strings[i].IndexOf(')');

                //If closing bracket is found
                if (bracket_closed != -1)
                {
                    string tsubstring_after_bracket = iFiltered_strings[i].Substring(bracket_closed);

                    //Check if there is a ; in this substring. If yes, then the function ends here, otherwise ; is on next line
                    int semicolon = tsubstring_after_bracket.IndexOf(';');

                    if (semicolon != -1)
                        return i;
                    else
                        return i + 1;
                }
                i++;
            }
            return -1;
        }

        private DelphiMethodStrings ParseInterfaceMethod(string imethodtype, ref List<string> iFiltered_strings, int icurr_string_count, out int oend_pos)
        {
            bool toverload = false, tvirtual = false, tabstract = false, tstatic = false;
            int tnext_pos = FindEndOfFunctionTitle(ref iFiltered_strings, icurr_string_count);//FindNextKey(ref iFiltered_strings, ref classKeys, icurr_string_count);

            //If not function parameter brackets, e.g Create; instead of Create();, then add empty brackets for parsing
            if (tnext_pos == -1)
            {
                iFiltered_strings[icurr_string_count] = iFiltered_strings[icurr_string_count].Replace(";", "();");
                tnext_pos = icurr_string_count;
            }

            oend_pos = tnext_pos;
            string tfuncstring = "";

            //Add all the indented lines in function title
            for (int i = icurr_string_count; i < tnext_pos + 1; i++)
            {
                tfuncstring = tfuncstring + iFiltered_strings[i].Trim();
            }

            //Check and strip out overload, virtual, abstract
            if (tfuncstring.IndexOf("static;") != -1)
            {
                tstatic = true;
                tfuncstring.Replace("static;", "");
            } 
            if (tfuncstring.IndexOf("overload;") != -1)
            {
                toverload = true;
                tfuncstring.Replace("overload;", "");
            }
            if (tfuncstring.IndexOf("virtual;") != -1)
            {
                tvirtual = true;
                tfuncstring.Replace("virtual;", "");
            }
            if (tfuncstring.IndexOf("abstract;") != -1)
            {
                tabstract = true;
                tfuncstring.Replace("abstract;", "");
            }

            //string[] tclassnamearray = tfuncstring.Split('.');

            //Break down function elements
            string treturntype, tparameters, tmethodname;
            string[] treturntypearray = tfuncstring.Split(')');

            //If there are no parameters
            if (treturntypearray.GetLength(0) == 1)
            {
                treturntypearray = treturntypearray[0].Split(':');
                treturntype = treturntypearray[1].Split(';')[0];
                string[] tnamearray = treturntypearray[0].Split(' ');
                tmethodname = tnamearray[tnamearray.GetLength(0)-1];
                tparameters = "";
            }
            //If there are parameters
            else
            {
                treturntype = treturntypearray[1].Split(';')[0];
                treturntypearray = treturntypearray[0].Split('(');

                if (treturntype != "")
                    treturntype = treturntype.Split(':')[1];

                string[] tnamearray = treturntypearray[0].Split(' ');
                tmethodname = tnamearray[tnamearray.GetLength(0)-1];
                tparameters = treturntypearray[1];
            }

            string[] tparameter_array = tparameters.Split(';');
            tparameters = "";
            //Trim the parameters and stitch them back
            for (int i = 0; i < tparameter_array.GetLength(0); i++)
            {
                string[] tparameter = tparameter_array[i].Split(' ');
                string tformatted_param = "";
                for (int j = 0; j < tparameter.GetLength(0); j++)
                {
                    if ((tparameter[j] != "") && (tparameter[j] != " "))
                        tformatted_param = tformatted_param + "_" +tparameter[j];
                }

                //Separate parameters with '|' and remove trailing and beginning spaces
                tparameters = tparameters + "|" + tformatted_param.Trim();
            }

            string tfunctionstring = treturntype + "/" + imethodtype + "/" + tmethodname + "/" + tparameters;
            DelphiMethodStrings tfunction = new DelphiMethodStrings(tfunctionstring, null);

            tfunction.isAbstract = tabstract;
            tfunction.isVirtual = tvirtual;
            tfunction.isOverloaded = toverload;

            return tfunction;
        }

        private DelphiClassStrings ParseInterfaceClass(ref List<string> istrings, string iclassname, int icurr_string_count, int inext_subsection_pos)
        {
            DelphiClassStrings tout = new DelphiClassStrings();
            List<string> tstrings = new List<string>();
            List<DelphiVarStrings> tvars = new List<DelphiVarStrings>(), tproperties = new List<DelphiVarStrings>(), tconsts = new List<DelphiVarStrings>();
            List<DelphiMethodStrings> tmethods = new List<DelphiMethodStrings>();
            List<List<DelphiMethodStrings>> tactions = new List<List<DelphiMethodStrings>>();

            int tcurr_string_count = icurr_string_count;

            //Variable filter. Variables are removed so that functions can be read without confusion
            int i = icurr_string_count;
            int new_next_subsection_pos;

            while (i < inext_subsection_pos)
            {
                if (CheckVariable(istrings[i]))
                    tvars.Add(new DelphiVarStrings(istrings[i], false));
                else
                    tstrings.Add(istrings[i]);

                i++;
            }

            if (istrings[i].Trim() == "end;")
            {
                tstrings.Add(istrings[i]);
                i++;
            }

            new_next_subsection_pos = icurr_string_count + tstrings.Count;
            tcurr_string_count = 0;

            while ((tcurr_string_count != -1) && (tcurr_string_count < new_next_subsection_pos))
            {
                int tnext_subsection_pos = FindNextKey(ref tstrings, ref implementKindKeys, tcurr_string_count);

                if (tnext_subsection_pos == -1)
                    tnext_subsection_pos = endInterface;

                string tconststr = "", ttempstr = "";

                string tcommentcheckstr = tstrings[tcurr_string_count].Trim();

                if (tcommentcheckstr != "")
                {
                    if (tcommentcheckstr[0] == '{')
                    {
                        //Ignore comments containing class name
                        tcurr_string_count++;
                    }
                    else
                    {
                        switch (RecognizeKey(tstrings[tcurr_string_count], ref classKeys))
                        {
                            case "public":  //ignore
                                break;

                            case "protected":  //ignore
                                break;

                            case "private": //ignore
                                break;

                            case "strict private": //ignore
                                break;

                            case "class const":
                                tnext_subsection_pos = FindNextKey(ref tstrings, ref const_implementKindKeys, tcurr_string_count);
                                
                                if (tnext_subsection_pos == -1)
                                    tnext_subsection_pos = endInterface;
                                
                                for (int j = tcurr_string_count + 1; j < tnext_subsection_pos - 1 && j < tstrings.Count; j++)
                                {
                                    tconststr = tstrings[j];

                                    ttempstr = tstrings[j].Trim();

                                    if (ttempstr != "")
                                    {
                                        if (ttempstr[0] == '{')
                                        {
                                            //Ignore comments containing class name
                                            while (ttempstr.IndexOf('}') == -1)
                                            {
                                                j++;
                                                ttempstr = tstrings[j].Trim();
                                            }
                                        }
                                        else if (ttempstr != "" && ttempstr[0] != '/')
                                        {
                                            bool tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');

                                            while (!tsemicolonfound && (ttempstr.IndexOf("//") == -1))
                                            {
                                                j++;
                                                ttempstr = tstrings[j].Trim();
                                                tconststr = tconststr + tstrings[j];

                                                if (ttempstr != "")
                                                    tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');
                                                else
                                                    tsemicolonfound = false;
                                            }

                                            tconsts.Add(new DelphiVarStrings(tconststr.Replace("=", ":="), true));
                                        }
                                    }
                                }
                                break;

                            case "public const":
                                tnext_subsection_pos = FindNextKey(ref tstrings, ref const_implementKindKeys, tcurr_string_count);

                                if (tnext_subsection_pos == -1)
                                    tnext_subsection_pos = endInterface;

                                for (int j = tcurr_string_count + 1; j < tnext_subsection_pos - 1 && j < tstrings.Count; j++)
                                {
                                    tconststr = tstrings[j];

                                    ttempstr = tstrings[j].Trim();

                                    if (ttempstr != "")
                                    {
                                        if (ttempstr[0] == '{')
                                        {
                                            //Ignore comments containing class name
                                            while (ttempstr.IndexOf('}') == -1)
                                            {
                                                j++;
                                                ttempstr = tstrings[j].Trim();
                                            }
                                        }
                                        else if (ttempstr != "" && ttempstr[0] != '/')
                                        {
                                            bool tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');

                                            while (!tsemicolonfound && (ttempstr.IndexOf("//") == -1))
                                            {
                                                j++;
                                                ttempstr = tstrings[j].Trim();
                                                tconststr = tconststr + tstrings[j];

                                                if (ttempstr != "")
                                                    tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');
                                                else
                                                    tsemicolonfound = false;
                                            }

                                            tconsts.Add(new DelphiVarStrings(tconststr.Replace("=", ":="), true));
                                        }
                                    }
                                }
                                break;

                            case "private const":
                                tnext_subsection_pos = FindNextKey(ref tstrings, ref const_implementKindKeys, tcurr_string_count);

                                if (tnext_subsection_pos == -1)
                                    tnext_subsection_pos = endInterface;

                                for (int j = tcurr_string_count + 1; j < tnext_subsection_pos - 1 && j < tstrings.Count; j++)
                                {
                                    tconststr = tstrings[j];

                                    ttempstr = tstrings[j].Trim();

                                    if (ttempstr != "")
                                    {
                                        if (ttempstr[0] == '{')
                                        {
                                            //Ignore comments containing class name
                                            while (ttempstr.IndexOf('}') == -1)
                                            {
                                                j++;
                                                ttempstr = tstrings[j].Trim();
                                            }
                                        }
                                        else if (ttempstr != "" && ttempstr[0] != '/')
                                        {
                                            bool tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');

                                            while (!tsemicolonfound && (ttempstr.IndexOf("//") == -1))
                                            {
                                                j++;
                                                ttempstr = tstrings[j].Trim();
                                                tconststr = tconststr + tstrings[j];

                                                if (ttempstr != "")
                                                    tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');
                                                else
                                                    tsemicolonfound = false;
                                            }

                                            tconsts.Add(new DelphiVarStrings(tconststr.Replace("=", ":="), true));
                                        }
                                    }
                                }
                                break;

                            case "const":
                                tnext_subsection_pos = FindNextKey(ref tstrings, ref const_implementKindKeys, tcurr_string_count);

                                if (tnext_subsection_pos == -1)
                                    tnext_subsection_pos = endInterface;

                                for (int j = tcurr_string_count + 1; j < tnext_subsection_pos - 1 && j < tstrings.Count; j++)
                                {
                                    tconststr = tstrings[j];

                                    ttempstr = tstrings[j].Trim();

                                    if (ttempstr != "")
                                    {
                                        if (ttempstr[0] == '{')
                                        {
                                            //Ignore comments containing class name
                                            while (ttempstr.IndexOf('}') == -1)
                                            {
                                                j++;
                                                ttempstr = tstrings[j].Trim();
                                            }
                                        }
                                        else if (ttempstr != "" && ttempstr[0] != '/')
                                        {
                                            bool tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');

                                            while (!tsemicolonfound && (ttempstr.IndexOf("//") == -1))
                                            {
                                                j++;
                                                ttempstr = tstrings[j].Trim();
                                                tconststr = tconststr + tstrings[j];

                                                if (ttempstr != "")
                                                    tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');
                                                else
                                                    tsemicolonfound = false;
                                            }

                                            tconsts.Add(new DelphiVarStrings(tconststr.Replace("=", ":="), true));
                                        }
                                    }
                                }
                                break;

                            case "class var": tvars.Add(new DelphiVarStrings(tstrings[tcurr_string_count], true));
                                break;

                            case "constructor":
                                tmethods.Add(ParseInterfaceMethod("constructor", ref tstrings, tcurr_string_count, out tcurr_string_count));
                                tactions.Add(new List<DelphiMethodStrings>());
                                break;

                            case "class function":
                                tmethods.Add(ParseInterfaceMethod("classfunction", ref tstrings, tcurr_string_count, out tcurr_string_count));
                                tactions.Add(new List<DelphiMethodStrings>());
                                break;

                            case "class procedure":
                                tmethods.Add(ParseInterfaceMethod("classprocedure", ref tstrings, tcurr_string_count, out tcurr_string_count));
                                tactions.Add(new List<DelphiMethodStrings>());
                                break;

                            case "procedure":
                                tmethods.Add(ParseInterfaceMethod("procedure", ref tstrings, tcurr_string_count, out tcurr_string_count));
                                tactions.Add(new List<DelphiMethodStrings>());
                                break;

                            case "function":
                                tmethods.Add(ParseInterfaceMethod("function", ref tstrings, tcurr_string_count, out tcurr_string_count));
                                tactions.Add(new List<DelphiMethodStrings>());
                                break;

                            case "class property":  if (tstrings[tcurr_string_count].Trim()[0] != '/')
                                    tproperties.Add(new DelphiVarStrings(tstrings[tcurr_string_count], true));
                                break;

                            case "property":    if (tstrings[tcurr_string_count].Trim()[0] != '/') 
                                    tproperties.Add(new DelphiVarStrings(tstrings[tcurr_string_count], false));
                                break;

                            default: break;
                        }
                        tcurr_string_count = FindNextKey(ref tstrings, ref implementKindKeys, tcurr_string_count);
                    }
                }
                else
                {
                    tcurr_string_count++;
                }
            }

            tout.name = iclassname;
            tout.methods = tmethods;
            tout.actions = tactions;
            tout.variables = tvars;
            tout.properties = tproperties;
            tout.consts = tconsts;

            return tout;
        }

        private DelphiClassStrings ParseRecord(ref List<string> istrings, string iclassname, int icurr_string_count, int inext_subsection_pos)
        {
            DelphiClassStrings tout = new DelphiClassStrings();
            List<string> tstrings = new List<string>();
            List<DelphiVarStrings> tvars = new List<DelphiVarStrings>(), tproperties = new List<DelphiVarStrings>(), tconsts = new List<DelphiVarStrings>();
            List<DelphiMethodStrings> tmethods = new List<DelphiMethodStrings>();

            int tcurr_string_count = icurr_string_count;

            //Variable filter. Variables are removed so that functions can be read without confusion
            int i = icurr_string_count;

            while (i < inext_subsection_pos)
            {
                if (istrings[i].IndexOf("set of") != -1)
                    tvars.Add(new DelphiVarStrings(istrings[i].Replace("set of", "HashSet<").Replace(";", ">;"), false));
                else if (istrings[i].IndexOf("array of") != -1)
                    tvars.Add(new DelphiVarStrings(istrings[i].Replace("array of", "").Replace(";", "[];"), false));
                else if (CheckRecordVariable(istrings[i]))
                    tvars.Add(new DelphiVarStrings(istrings[i], false));
                else
                    tstrings.Add(istrings[i]);

                i++;
            }
            
            tout.name = iclassname;
            tout.methods = tmethods;
            tout.variables = tvars;
            tout.properties = tproperties;
            tout.consts = tconsts;

            return tout;
        }

        private void ParseVar(ref List<string> istrings, ref List<string> ovars)
        {
        	if (startVar != -1)
        		for (int i = startVar + 1; i < startImplementation; i++)
        			ovars.Add(istrings[i]);
        }

        //oclassimplementations is a list of class functions and procedures
        private void ParseImplementation(ref List<string> istrings, ref List<string> ouses, ref List<string> oclassnames, ref List<DelphiClassStrings> oclassimplementations, ref List<string> oconsts, ref List<string> oenum, ref List<string> oalias)
        {
            int tcurr_string_count = startImplementation, tnext_subsection_pos = -1;

            //mplementation sub sections
            while (tcurr_string_count != -1 && tcurr_string_count < endImplementation)
            {
                tnext_subsection_pos = FindNextKey(ref istrings, ref implementKindKeys, tcurr_string_count);

                if (tnext_subsection_pos == -1)
                    tnext_subsection_pos = endImplementation;

                string tcommentcheckstr = istrings[tcurr_string_count].Trim();

                if (tcommentcheckstr != "")
                {
                    if (tcommentcheckstr[0] == '{')
                    {
                        //Ignore comments containing class name
                        tcurr_string_count++;
                    }
                    else
                    {
                        switch (RecognizeKey(istrings[tcurr_string_count], ref implementKindKeys))
                        {
                            case "class function":  //Break down function elements
                                string tmethodtype = "classfunction";
                                ParseImplementationMethod(tmethodtype, tcurr_string_count, ref tnext_subsection_pos, ref istrings, ref oclassnames, ref oclassimplementations);
                                break;

                            case "class procedure": //Break down function elements
                                tmethodtype = "classprocedure";
                                ParseImplementationMethod(tmethodtype, tcurr_string_count, ref tnext_subsection_pos, ref istrings, ref oclassnames, ref oclassimplementations);
                                break;

                            case "function":    //Break down function elements
                                tmethodtype = "function";
                                ParseImplementationMethod(tmethodtype, tcurr_string_count, ref tnext_subsection_pos, ref istrings, ref oclassnames, ref oclassimplementations);
                                break;

                            case "procedure":   //Break down function elements
                                tmethodtype = "procedure";
                                ParseImplementationMethod(tmethodtype, tcurr_string_count, ref tnext_subsection_pos, ref istrings, ref oclassnames, ref oclassimplementations);
                                break;

                            case "constructor": //Break down function elements
                                tmethodtype = "constructor";
                                ParseImplementationMethod(tmethodtype, tcurr_string_count, ref tnext_subsection_pos, ref istrings, ref oclassnames, ref oclassimplementations);
                                break;

                            case "const": tnext_subsection_pos = FindNextKey(ref istrings, ref const_implementKindKeys, tcurr_string_count);
                                
                                if (tnext_subsection_pos == -1)
                                    tnext_subsection_pos = endImplementation;
                                
                                for (int j = tcurr_string_count + 1; j < tnext_subsection_pos - 1 && j < istrings.Count; j++)
                                {
                                    string tconststr = istrings[j];

                                    string ttempstr = istrings[j].Trim();

                                    if (ttempstr != "")
                                    {
                                        if (ttempstr[0] == '{')
                                        {
                                            //Ignore comments containing class name
                                            while (ttempstr.IndexOf('}') == -1)
                                            {
                                                j++;
                                                ttempstr = istrings[j].Trim();
                                            }
                                        }
                                        else
                                        {
                                            if (ttempstr != "" && ttempstr[0] != '/')
                                            {
                                                bool tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');
                                                while ( !tsemicolonfound  && (ttempstr.IndexOf("//") == -1))
                                                {
                                                    j++;
                                                    ttempstr = istrings[j].Trim();
                                                    tconststr = tconststr + istrings[j];

                                                    if (ttempstr != "")
                                                        tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');
                                                    else
                                                        tsemicolonfound = false;
                                                }
                                                oconsts.Add(tconststr.Replace("=", ":="));
                                            }
                                        }
                                    }
                                }
                                break;


                            //oconsts.AddRange(GetStringSubList(ref istrings, tcurr_string_count + 1, tnext_subsection_pos)); 
                            //break;

                            case "uses": ouses.AddRange(GetStringSubList(ref istrings, tcurr_string_count + 1, tnext_subsection_pos));
                                break;

                            //Log unrecognized sub section
                            default: List<string> tlogmessages = new List<string>();
                                tlogmessages.Add("Interface sub section not recognized " + tcurr_string_count);
                                tlogmessages.AddRange(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos));
                                log(tlogmessages);
                                break;
                        }
                        tcurr_string_count = tnext_subsection_pos;
                    }
                }
                else
                {
                    tcurr_string_count++;
                }
            }     
        }
        
        private void ParseImplementationMethod(string imethodtype, int icurr_string_count, ref int inext_subsection_pos, ref List<string> istrings, ref List<string> oclassnames, ref List<DelphiClassStrings> oclassimplementations)
        {
            string tclassname;

            //Check for parameters

            int tnext_pos = FindNextSymbol(ref istrings, "begin", icurr_string_count, true);
            int tvar_pos = FindNextSymbol(ref istrings, "var", icurr_string_count, true);
            int tconst_pos = FindNextSymbol(ref istrings, "const", icurr_string_count, true);

            int tclosebracket_pos = FindNextSymbol(ref istrings, ")", icurr_string_count-1);
            int topenbracket_pos = FindNextSymbol(ref istrings, "(", icurr_string_count - 1);
            int tbegin_pos = tnext_pos;
            int tvarfunctionpos = -1, tprocedurepos = -1;
            List<int> taction_begins = new List<int>(), taction_ends = new List<int>();
            List<List<string>> tactions = new List<List<string>>(); 

            //If there is a var section
            if (tvar_pos != -1)
            {
                int i = 0;
                //Check if const is a parameter
                if (topenbracket_pos != -1)
                {
                    while ((tvar_pos <= tclosebracket_pos) && (tvar_pos >= topenbracket_pos))
                    {
                        //Look for more consts
                        tvar_pos = FindNextSymbol(ref istrings, "var", icurr_string_count + i);
                        i++;
                    }
                }

                ////Check if this var comes before "begin". If yes, then this is a sub-section declaration. Otherwise ignore.
                //if ((tvar_pos != -1) && (tvar_pos < tnext_pos))
                //    tnext_pos = tvar_pos;
                //else
                //    tvar_pos = -1;
            }

            //If there is a var section
            if (tvar_pos != -1)
            {
                ActionFilter:
                //Check for Delphi Actions and separate them - they will mess up normal parsing
                tvarfunctionpos = FindNextSymbol(ref istrings, "function", tvar_pos);
                tprocedurepos = FindNextSymbol(ref istrings, "procedure", tvar_pos);

                //Check if Delphi Action exists
                if (((tvarfunctionpos != -1) && (tvarfunctionpos < tbegin_pos) && (tvarfunctionpos > tclosebracket_pos)) ||
                    ((tprocedurepos != -1) && (tprocedurepos < tbegin_pos) && (tvarfunctionpos > tclosebracket_pos)))
                {
                    int tfuncindex, ii;

                    if (tvarfunctionpos != -1)
                    {
                        if (tprocedurepos != -1)
                        {
                            if (tvarfunctionpos < tprocedurepos)
                                tfuncindex = tvarfunctionpos;
                            else
                                tfuncindex = tprocedurepos;
                        }
                        else
                            tfuncindex = tvarfunctionpos;
                    }
                    else
                        tfuncindex = tprocedurepos;

                    ii = tfuncindex+1;

                    int tbegincounter = 0;
                    string tvar_str = istrings[ii].Trim();
                    //Match tmatch = Regex.Match(tvar_str, "\\bbegin\\b"), tmatch2;

                    //Find begin
                    while (tvar_str != "begin")//(!tmatch.Success)
                    {
                        ii++;
                        tvar_str = istrings[ii].Trim();
                        //tmatch = Regex.Match(tvar_str, "\\bbegin\\b");
                    }

                    //Find final end;
                    int tbegin = ii, tend = -1;
                    taction_begins.Add(tbegin);

                    tbegincounter++;
                    while (tbegincounter > 0)
                    {
                        ii++;
                        tvar_str = istrings[ii].Trim();
                        //tmatch = Regex.Match(tvar_str, "\\bbegin\\b");
                        //tmatch2 = Regex.Match(tvar_str, "\\bend;\\b");

                        if (tvar_str == "begin")//(tmatch.Success)
                            tbegincounter++;
                        else if (tvar_str == "end;")//(tmatch2.Success)
                            tbegincounter--;
                    }

                    tend = ii;
                    taction_ends.Add(tend);

                    tnext_pos = FindNextSymbol(ref istrings, "begin", tend, true);
                    tbegin_pos = tnext_pos;

                    //Pack everything into an Action
                    List<string> taction = new List<string>();
                    //tvarfunctionpos++;
                    tend++;
                    taction = istrings.GetRange(tfuncindex, tend - tfuncindex);
                    istrings.RemoveRange(tfuncindex, tend - tfuncindex);
                    tactions.Add(taction);

                    List<string> tblanks = new List<string>();
                    for (int i = 0; i < tend - tfuncindex; i++)
                        tblanks.Add("");

                    istrings.InsertRange(tfuncindex, tblanks);
                    goto ActionFilter;
                }

                //If the var comes before the begin, then this belongs to our current function. 
                if (tvar_pos < tnext_pos)
                    tnext_pos = tvar_pos;
                else
                    tvar_pos = -1;
            }

            //If there is a const sections
            if ((tconst_pos != -1) && (tconst_pos < tbegin_pos))
            {
                int i = 0;
                //Check if const is a parameter
                if (topenbracket_pos != -1)
                {
                    while ((tconst_pos <= tclosebracket_pos) && (tconst_pos >= topenbracket_pos))
                    {
                        //Look for more consts
                        tconst_pos = FindNextSymbol(ref istrings, "const", icurr_string_count + i);
                        i++;
                    }
                }
                //Check if this const comes before "begin". If yes, then this is a sub-section declaration. Otherwise ignore.
                if ((tconst_pos != -1) && (tconst_pos < tnext_pos))
                    tnext_pos = tconst_pos;
                else
                    tconst_pos = -1;
            }
            else
                tconst_pos = -1;

            string tfuncstring = "";

            //Add all the indented lines in function title
            for (int i = icurr_string_count; i < tnext_pos; i++)
            {
                tfuncstring = tfuncstring + istrings[i].Trim();
            }

            string[] tclassnamearray = tfuncstring.Split('.');
            string[] treturnarray = tclassnamearray;
            tclassnamearray = tclassnamearray[0].Split(' ');
            tclassname = tclassnamearray[tclassnamearray.GetLength(0)-1];

            inext_subsection_pos = FindNextKey(ref istrings, ref implementKindKeys, tnext_pos);

            for (int i = 0; i < oclassimplementations.Count; i++)
            {
                //Find the class
                if (oclassimplementations[i].name == tclassname)
                {
                    List<string> tlist = ParseImplementationMethodBody(tclassname, ref istrings, ref treturnarray, tfuncstring, imethodtype, tnext_pos, inext_subsection_pos);
                    

                    string theader = tlist[0];
                    tlist.RemoveAt(0);
                    oclassimplementations[i].AddMethodBody(theader, tlist, tvar_pos, tconst_pos, tbegin_pos, tnext_pos);

                    //Add the actions
                    for (int j = 0; j < tactions.Count; j++)
                    {
                        List<string> tactionstrings = new List<string>();
                        tactionstrings.Add("");
                        tactionstrings.AddRange(tactions[j]);

                        imethodtype = RecognizeKey(tactions[j][0], ref implementKindKeys);

                        icurr_string_count = 1;
                        tnext_pos = FindNextSymbol(ref tactionstrings, "begin", icurr_string_count, true);
                        tvar_pos = FindNextSymbol(ref tactionstrings, "var", icurr_string_count, true);
                        tconst_pos = FindNextSymbol(ref tactionstrings, "const", icurr_string_count, true);

                        tclosebracket_pos = FindNextSymbol(ref tactionstrings, ")", icurr_string_count - 1);
                        topenbracket_pos = FindNextSymbol(ref tactionstrings, "(", icurr_string_count - 1);
                        tbegin_pos = tnext_pos;
                        tvarfunctionpos = -1;
                        tprocedurepos = -1;

                        taction_begins = new List<int>();
                        taction_ends = new List<int>();
                        tactions = new List<List<string>>();

                        //If there is a var section
                        if (tvar_pos != -1)
                        {
                            //If the var comes before the begin, then this belongs to our current function. 
                            if (tvar_pos < tnext_pos)
                                tnext_pos = tvar_pos;
                            else
                                tvar_pos = -1;
                        }

                        //If there is a const sections
                        if ((tconst_pos != -1) && (tconst_pos < tbegin_pos))
                        {
                            int ii = 0;
                            //Check if const is a parameter
                            if (topenbracket_pos != -1)
                            {
                                while ((tconst_pos <= tclosebracket_pos) && (tconst_pos >= topenbracket_pos))
                                {
                                    //Look for more consts
                                    tconst_pos = FindNextSymbol(ref tactionstrings, "const", icurr_string_count + ii);
                                    ii++;
                                }
                            }
                            //Check if this const comes before "begin". If yes, then this is a sub-section declaration. Otherwise ignore.
                            if ((tconst_pos != -1) && (tconst_pos < tnext_pos))
                                tnext_pos = tconst_pos;
                            else
                                tconst_pos = -1;
                        }
                        else
                            tconst_pos = -1;

                        tfuncstring = "";

                        //Add all the indented lines in function title
                        for (int ii = icurr_string_count; ii < tnext_pos; ii++)
                            tfuncstring = tfuncstring + tactionstrings[ii].Trim();

                        tclassnamearray = tfuncstring.Split('.');
                        treturnarray = tclassnamearray;

                        inext_subsection_pos = FindNextKey(ref istrings, ref implementKindKeys, tnext_pos);

                        tlist = ParseImplementationMethodBody(tclassname, tactionstrings, ref treturnarray, tfuncstring, imethodtype, tnext_pos, tactionstrings.Count - 1);
                        oclassimplementations[i].AddAction(theader, tlist, tvar_pos, tconst_pos, tbegin_pos);
                    }
                    break;
                }
            }
        }

        private List<string> ParseImplementationMethodBody(string iclassname, List<string> istrings, ref string[] iclassnamearray, string ifuncstring, string itype, int ipos, int inextpos)
        {
            string imethodtype = RecognizeKey(ifuncstring, ref classKeys);
            
            if (imethodtype == "strict private")
                imethodtype = "private";

            imethodtype = imethodtype.Replace(" ", "");

            //Break down function elements
            string treturntype, tparameters, tmethodname;
            string[] treturntypearray = ifuncstring.Split(')');

            //If there are no parameters
            if (treturntypearray.GetLength(0) == 1)
            {
                treturntypearray = treturntypearray[0].Split(':');

                //There is no return type
                if (treturntypearray.GetLength(0) == 1)
                {
                    treturntype = "";
                }
                else
                {
                    treturntype = treturntypearray[1].Split(';')[0];
                }
                string[] tnamearray = treturntypearray[0].Split(' ');
                tmethodname = tnamearray[tnamearray.GetLength(0) - 1];
                tparameters = "";
            }
            //If there are parameters
            else
            {
                treturntype = treturntypearray[1].Split(';')[0];
                treturntypearray = treturntypearray[0].Split('(');

                if (treturntype != "")
                    treturntype = treturntype.Split(':')[1];

                string[] tnamearray = treturntypearray[0].Split(' ');
                tmethodname = tnamearray[tnamearray.GetLength(0) - 1];
                tparameters = treturntypearray[1];
            }

            string[] tparameter_array = tparameters.Split(';');
            tparameters = "";
            //Trim the parameters and stitch them back
            for (int i = 0; i < tparameter_array.GetLength(0); i++)
            {
                string[] tparameter = tparameter_array[i].Split(' ');
                string tformatted_param = "";
                for (int j = 0; j < tparameter.GetLength(0); j++)
                {
                    if ((tparameter[j] != "") && (tparameter[j] != " "))
                        tformatted_param = tformatted_param + "_" + tparameter[j];
                }

                //Separate parameters with '|' and remove trailing and beginning spaces
                tparameters = tparameters + "|" + tformatted_param.Trim();
            }

            string[] tmethodnamearr = tmethodname.Split('.');

            if (tmethodnamearr.Length > 1)
                tmethodname = tmethodname.Split('.')[1];
            else if (tmethodnamearr.Length < 1)
            {
                //error
            }

            tmethodname = tmethodname.Replace(";", "");

            string tfunctionstring = treturntype + "/" + imethodtype + "/" + tmethodname + "/" + tparameters;

            List<string> tout = new List<string>();
            tout.Add(tfunctionstring);            

            tout.AddRange(GetStringSubList(ref istrings, ipos, inextpos));

            return tout;
        }

        private List<string> ParseImplementationMethodBody(string iclassname, ref List<string> istrings, ref string[] iclassnamearray, string ifuncstring, string itype, int ipos, int inextpos)
        {
            if (inextpos == -1)
                inextpos = istrings.Count;

            List<string> tstrarray = GetStringSubList(ref istrings, ipos, inextpos);
            return ParseImplementationMethodBody(iclassname, tstrarray, ref iclassnamearray, ifuncstring, itype, 0, inextpos-ipos);
        }

    	private void IndexStructure(ref List<string> istrings, bool ireadheader, string iheaderstart, string iheaderend)
        {
            if (ireadheader)
            {
                startHeader = FindStringInList(iheaderstart, ref istrings, 0, true);

                if (startHeader == -1)
                    startHeader = FindStringInList("{--", ref istrings, 0, true);

                if (startHeader != -1)
                {
                    endHeader = FindStringInList(iheaderend, ref istrings, startHeader, true);
                    if (endHeader == -1)
                    {
                        endHeader = FindStringInList("}", ref istrings, startHeader, true);
                        
                        if (endHeader == -1)
                            throw new Exception("Header end not found");
                    }

                    Section_Names.Add("Header");
                    Section_Bookmarks.Add(startHeader);
                }

                startInterface = FindStringInList("interface", ref istrings, endHeader, true) + 1;
            }
            else
            {
                startInterface = FindStringInList("interface", ref istrings, 0, true) + 1;
            }

            if (startInterface == -1)
                throw new Exception("Interface not found");

        	Section_Names.Add("Interface");
    		Section_Bookmarks.Add(startInterface);

    		//If there is no other section, process
        	endInterface = FindNextSection(ref istrings, startInterface);
    		if (endInterface == -1)
        		return;

            startImplementation = FindStringInList("implementation", ref istrings, endInterface, true) + 1;
    		if (startImplementation != -1)
    		{
    			//Look for a Var section in between interface and implementation
    			if ((endInterface+1) != startImplementation)
    			{
    				startVar = endInterface;
    				endVar = startImplementation;
    				
					Section_Names.Add("Var");
					Section_Bookmarks.Add(startVar);
    			}

    			Section_Names.Add("Implementation");
	    		Section_Bookmarks.Add(startImplementation);

	    		//If there is no other section after "implementation", then process
                endImplementation = FindNextSymbol(ref istrings, "end.", startImplementation, true);//FindNextSection(ref istrings, startImplementation);
	    		if (endImplementation == -1)
	    		{
	        		return;
	    		}
    		}
    		else
    		{
                startVar = FindStringInList("var", ref istrings, endInterface, true) + 1;
	            if (startVar != -1)
	            {
					Section_Names.Add("Var");
					Section_Bookmarks.Add(startVar);
	            }
    		}
        }
        
        static private List<Class> StringsToClass(string[] istrings)
        {
            return new List<Class>();
        }      

        private int GoToNextKeyword(string ifind, ref List<string> iarray, int iindex, bool imatchCase)
		{
            return 0;
		}

        private int FindNextFunctionImplementation(ref List<string> iarray, int istartpoint)
        {
            for (int i = istartpoint; i < iarray.Count;)
            {
                return i;
            }
            return -1;
        }

        static private int FindNextSymbol(ref List<string> istrings, string isymbol, int istartpos, bool iexactmatch)
        {
            string[] tEnd = new string[1];
            tEnd[0] = isymbol;

            for (int i = istartpos; i < istrings.Count; i++)
            {
                if (Check_If_StringList_Elements_Exist_In_String(istrings[i], ref tEnd, iexactmatch) != -1)
                    return i;
            }
            return -1;
        }

        static private int FindNextSymbol(ref List<string> istrings, string isymbol, int istartpos)
        {
            return FindNextSymbol(ref istrings, isymbol, istartpos, false);
        }

        static private int FindNextKey(ref List<string> istrings, ref string[] ikeys, int istartpos)
        {
            if (istartpos < 0)
                return -1;

            for (int i = istartpos + 1; i < istrings.Count; i++)
            {
                if (Check_If_StringList_Elements_Exist_In_String(istrings[i], ref ikeys) != -1)
                    return i;
            }
            return -1;
        }
        
        private int FindNextSection(ref List<string> istrings, int istartpos)
        {
            for (int i = istartpos; i < istrings.Count; i++)
            {
                if (Match_StringList_Elements_To_String(istrings[i], ref sectionKeys) != -1)
                    return i;
            }
            return -1;
        }

        private int FindNextInterfaceSubSection(ref List<string> istrings, int istartpos)
        {
        	for (int i = istartpos; i < endInterface; i++)
        	{
                if (Match_StringList_Elements_To_String(istrings[i], ref subsectionKeys) != -1)
        			return i;
        	}
        	return -1;
        }

        private int FindNextInterfaceSubType(ref List<string> istrings, int istartpos)
        {
            for (int i = istartpos; i < endInterface; i++)
            {
                if (Check_If_StringList_Elements_Exist_In_String(istrings[i], ref interfaceKindKeys) != -1)
                    return i;
            }
            return -1;
        }

        static private string RecognizeKey(string istring, ref string[] ikeys)
		{

            string pattern = "";

            //for (int i = 0; i < iElementList.GetLength(0); i++)
            //{
            //    pattern = "\\b" + iElementList[i] + "\\b";
            //    istring = Regex.Replace(istring, pattern, iSubstitutesList[i]);
            //}

           	for (int i=0; i < ikeys.GetLength(0); i++)
           	{
                pattern = "\\b" + ikeys[i] + "\\b";
                Match tmatch = Regex.Match(istring, pattern);

           		if (tmatch.Success)//(istring.IndexOf(ikeys[i]) != -1)
           			return ikeys[i].Trim();
           	}
           	return "";
		}

        static private bool RecognizeComment(string istring)
        {
            //Remove all white spaces
            istring = Regex.Replace(istring, @"\s+", "");

            //If first character is '/', then it is a comment
            if (istring[0] == '/')
                return true;

            return false;
        }

        static private bool RecognizeEmptyLine(string istring)
        {
            //Remove all white spaces
            istring = Regex.Replace(istring, @"\s+", "");

            if (istring.Length == 0)
                return true;

            return false;
        }

        static private List<string> RecognizeClassImplementation(string iline)
        {
            return new List<string>();
        }

        //Differentiate and return segments for Variables and Properties
        static private List<string> RecognizeClassDefinition(string iline)
        {
            List<string> tout = new List<string>();
            string[] tstr_arr;

            int isproperty = iline.IndexOf("property");


            //Handle Properties
            if (isproperty != -1)
            {
                string tstr = iline.Replace("property", "").Replace("class", "").Replace(";", "").Trim();
                tstr_arr = tstr.Split(':');
                
                //Add name
                tout.Add(tstr_arr[0]);

                //Check if read
                int tread = tstr_arr[1].IndexOf("read");
                int twrite = tstr_arr[1].IndexOf("write");

                if ((tread == -1) && (twrite == -1))
                {
                    tout.Add(tstr_arr[1].Trim());
                    tout.Add("null");
                    tout.Add("null");
                }
                else if (tread == -1)
                {
                    tstr_arr[1] = tstr_arr[1].Replace("write", ";");
                    tstr_arr = tstr_arr[1].Split(';');
                    
                    //Add type and read
                    //tout.Add(tstr_arr[0].Trim());
                    //tout.Add(tstr_arr[1].Trim());
                    
                    tout.Add(tstr_arr[0].Trim());
                    tout.Add("null");
                    tout.Add(tstr_arr[1].Trim());
                }
                else if (twrite == -1)
                {
                    tstr_arr[1] = tstr_arr[1].Replace("read", ";");
                    tstr_arr = tstr_arr[1].Split(';');

                    //Add type and write
                    tout.Add(tstr_arr[0].Trim());
                    tout.Add(tstr_arr[1].Trim());
                    tout.Add("null");//tstr_arr[1].Trim());
                }
                else if (tread > twrite)
                {
                    tstr_arr[1] = tstr_arr[1].Replace("write", ";");
                    tstr_arr[1] = tstr_arr[1].Replace("read", ";");
                    tstr_arr = tstr_arr[1].Split(';');

                    //Add type, read and write
                    tout.Add(tstr_arr[0].Trim());
                    tout.Add(tstr_arr[2].Trim());
                    tout.Add(tstr_arr[1].Trim());
                }
                else
                {
                    tstr_arr[1] = tstr_arr[1].Replace("write", ";");
                    tstr_arr[1] = tstr_arr[1].Replace("read", ";");
                    tstr_arr = tstr_arr[1].Split(';');

                    //Add type, read and write
                    tout.Add(tstr_arr[0].Trim());
                    tout.Add(tstr_arr[1].Trim());
                    tout.Add(tstr_arr[2].Trim());
                }
            }
            //Handle variables and constants
            else
            {
                tstr_arr = iline.Split(':');
                string[] tstr_arr2 = tstr_arr[0].Trim().Split(' ');
                tout.Add(tstr_arr2[tstr_arr2.GetLength(0) - 1]);
                tout.Add(tstr_arr[1].Split(';')[0]);
            }
            return tout;
        }

        static private int Match_StringList_Elements_To_String(string istring, ref string[] ielements)
        {
            for (int i = 0; i < ielements.GetLength(0); i++)
            {
                if (istring == ielements[i])
                    return i;
            }
            return -1;
        }

        //Check if any of the elements in a array ielements is found inside a string istring.    
        static private int Check_If_StringList_Elements_Exist_In_String(string istring, ref string[] ielements)
        {
            return Check_If_StringList_Elements_Exist_In_String(istring, ref ielements, false);
        }

        static private int Check_If_StringList_Elements_Exist_In_String(string istring, ref string[] ielements, bool iexactmatch)
        {
            //Split string to check in, and element to check against, according to spaces. 
            //If the first element word is found, check for the rest in order. Otherwise return false.

            //Split string into words
            string[] tstring_words = istring.Split(' ');

            //Loop through the elements array and check each one
            for (int i = 0; i < ielements.GetLength(0); i++)
            {
                //Split the current element into words
                string[] telement_words = ielements[i].Split(' ');

                int first_word_found = 0;
                int prev_word_found = 0;
                int curr_word_found = 0;

                //Loop through the words in the element. Find the first word. If not found then element does not exist in string
                for (int j = 0; j < telement_words.GetLength(0); j++)
                {
                    //Loop through the contents of the string to look for the word. 
                    for(int k=0; k < tstring_words.GetLength(0); k++)
                    {
                        if (iexactmatch)
                        {
                            foreach (String str in tstring_words[k].Split(" \n\r".ToCharArray()))
                            {
                                if (str.Equals(telement_words[j]))
                                {
                                    curr_word_found = k;
                                    goto check_order_of_words;
                                }
                            }
                        }
                        else
                        {
                            if (tstring_words[k].Contains(telement_words[j]))
                            {
                                curr_word_found = k;
                                goto check_order_of_words;
                            }
                        }
                    }
                    //The elementword was not found in the string array, the element is not present in the string
                    goto element_not_found;

                check_order_of_words:

                    //Words should follow in sequence
                    if(curr_word_found >= prev_word_found)
                    {
                        prev_word_found = curr_word_found;
                        if(curr_word_found == 0)
                            first_word_found = curr_word_found;
                    }
                    else
                        goto element_not_found;
                }

                //If reached this point, that means the element has been found in the string in the correct order of words.
                //Calculate the index till the first occurence in the string and return it.
                int index = 0;
                for (int j = 0; j < first_word_found; j++)
                    index = index + telement_words[j].Length;

                return index;

            element_not_found:
                prev_word_found = 0;//Dummy statement needed for label
                //Go to next element
            }
            return -1;
        } 

        static private int CheckStringListElementsInString(string istring, ref string[] ielements)
		{
            string[] tarray = istring.Split(' ');
            for (int i = 0; i < ielements.GetLength(0); i++)
           	{
                string s = ielements[i];
                if(tarray.FirstOrDefault(e => e == s) != null)
           			return i;
           	}
           	return -1;
		}
                                                           
	    static public int FindStringInList(string ifind, ref List<string> iarray, int iindex, bool imatchCase)
		{
            int i = iindex;

            if (i < 0)
                i = 1;

	    	for (; i < iarray.Count; i++)
	    	{
	    		string tstring;

                if (!imatchCase)
	    			tstring = iarray[i].ToLower();
	    		else
	    			tstring = iarray[i];
	    		
	    		if (tstring.IndexOf(ifind) != -1)
	    			return i;
	    	}
	    	return -1;
		}
	    
	    static private List<string> GetStringSubList(ref List<string> istrings, int istart, int iend)
	    {
	    	List<string> toutput = new List<string>();
	    	
	    	for (int i=istart; i < iend; i++)
	    		toutput.Add(istrings[i]);
	    	
	    	return toutput;
	    }

        private Function GenerateMethod(ref List<string> iMethodStrings, ref string[] iheader_arr, string iheader, string iname, string ireturntype, string itype, int const_start, int begin_start, int var_start, bool tisVirtual, bool tisAbstract, ref Class iclass, ref List<string> inames, int icounter, int icounter2)
        {
            bool tIsStatic = false;
            Variable tvar;

            if ((itype == "classfunction") || (itype == "classprocedure") || (itype == "constructor") || (itype == "destructor"))
                tIsStatic = true;

            List<Variable> tparams = new List<Variable>();

            //Getting the parameters
            iheader_arr = iheader_arr[3].Split('|');

            //Remove first element that is always "" empty
            List<string> tlist = new List<string>(iheader_arr);
            tlist.RemoveAt(0);
            iheader_arr = tlist.ToArray();

            for (int ii = 0; ii < iheader_arr.GetLength(0); ii++)
            {
                string[] tvar_arr = iheader_arr[ii].Trim().Split('_');

                //Remove first element that is always "" empty
                tlist = new List<string>(tvar_arr);
                tlist.RemoveAt(0);
                tvar_arr = tlist.ToArray();

                int tlength = tvar_arr.GetLength(0);

                if (tlength <= 0)
                {
                    //No parameters
                }
                else if (tlength == 1)
                {
                    if (tvar_arr[0] == "")
                    {
                        //No parameters
                    }
                    else
                    {
                        throw new Exception("Parsing function parameter : " + iheader_arr[ii] + " resulted in error while Generating class " + inames[icounter2] + " from Text.");
                    }
                }
                else if (tlength == 2)
                {
                    //Variables in method parameter. They are not static, so false in Variable Constructor
                    tvar = new Variable(tvar_arr[0], tvar_arr[tlength - 1], "", "", false);
                    tparams.Add(tvar);
                }
                else //if (tlength == 3)
                {
                    //Variables in method parameter. They are not static, so false in Variable Constructor
                    //tvar = new Variable(tvar_arr[0] + " " + tvar_arr[1],  tvar_arr[tlength - 1], false);
                    for (int jj = 1; jj < (tlength - 1); jj++)
                    {
                        tvar = new Variable(tvar_arr[jj], tvar_arr[tlength - 1], "", "", false);
                        tparams.Add(tvar);
                    }
                }
                //else
                //{
                //    throw new Exception("Parsing function parameter : " + theader_arr[ii] + " resulted in error while Generating class " + inames[icounter2] + " from Text.");
                //}
            }
            
            int from_pos = -1, till_pos = -1, body_start = begin_start;

            //Get const and var lists
            List<Constant> tconsts = new List<Constant>();
            if (const_start != -1)
            {
                from_pos = const_start;
                body_start = const_start;

                if (var_start > const_start)
                {
                    till_pos = var_start;
                }
                else
                    till_pos = begin_start;

                for (int ii = from_pos + 1; ii < till_pos; ii++)
                {
                    string tvar_str = iMethodStrings[ii];   
                    string ttempstr = iMethodStrings[ii].Trim();

                    if (ttempstr != "")
                    {
                        if (ttempstr[0] == '{')
                        {
                            //Ignore comments containing class name
                            while (ttempstr.IndexOf('}') == -1)
                            {
                                ii++;
                                ttempstr = iMethodStrings[ii].Trim();
                            }
                        }
                        else if (ttempstr != "" && ttempstr[0] != '/')
                        {
                            bool tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');
                            while ( !tsemicolonfound && (ttempstr.IndexOf("//") == -1))
                            {
                                ii++;
                                ttempstr = iMethodStrings[ii].Trim();
                                tvar_str = tvar_str + iMethodStrings[ii];

                                if (ttempstr != "")
                                    tsemicolonfound = (ttempstr[ttempstr.Length - 1] == ';');
                                else
                                    tsemicolonfound = false;
                            }

                            tvar_str = tvar_str.Replace("=", ":=");

                            string[] tvar_arr = tvar_str.Split('=');
                            
                            tvar_arr[0] = tvar_arr[0].Trim();
                            tvar_arr[1] = tvar_arr[1].Replace(";", "");
                            tvar_arr[1] = tvar_arr[1].Trim();

                            tconsts.Add(new Constant(tvar_arr[0], tvar_arr[1]));
                        }
                    }
                }
            }

            List<Variable> tvars = new List<Variable>();
            if (var_start != -1)
            {
                from_pos = var_start;

                if (const_start > var_start)
                {
                    till_pos = const_start;
                    body_start = var_start;
                }
                else
                    till_pos = begin_start;

                if (const_start == -1)
                    body_start = var_start;

                for (int ii = from_pos + 1; ii < till_pos && ii < iMethodStrings.Count; ii++)
                {
                    string tvar_str = iMethodStrings[ii];
                    string ttempstr = iMethodStrings[ii].Trim();

                    if (ttempstr != "")
                    {
                        if (ttempstr[0] == '{')
                        {
                            //Ignore comments containing class name
                            while (ttempstr.IndexOf('}') == -1)
                            {
                                ii++;
                                ttempstr = iMethodStrings[ii].Trim();
                            }
                        }
                        else if (ttempstr != "" && ttempstr[0] != '/')
                        {
                            string tvarcomment = "";
                            string[] tvarcommentarr = tvar_str.Split("//".ToCharArray());

                            if (tvarcommentarr.Length > 1)
                            {
                                tvar_str = tvarcommentarr[0];
                                tvarcomment = tvarcommentarr[1];
                            }

                            while (tvar_str.IndexOf(':') == -1)
                            {
                                ii++;
                                tvar_str = tvar_str + iMethodStrings[ii];
                                tvarcommentarr = tvar_str.Split("//".ToCharArray());

                                if (tvarcommentarr.Length > 1)
                                {
                                    tvar_str = tvarcommentarr[0];
                                    tvarcomment = tvarcomment + tvarcommentarr[1];
                                }
                            }

                            if (tvar_str.Trim() != "")
                                tvars.AddRange(StringToVariable(tvar_str, tvarcomment));
                            //string[] tvar_arr = tvar_str.Split(':');

                            //tvar_arr[0].Trim();
                            //tvar_arr[1] = tvar_arr[1].Replace(";", "");
                            //tvar_arr[1].Trim();

                            //tvars.Add(new Variable(tvar_arr[0], tvar_arr[1], false));
                        }
                    }
                }
            }

            if ((body_start != -1) && (iMethodStrings.Count > 0))
            {
                if (begin_start - body_start >= iMethodStrings.Count)
                    iMethodStrings = new List<string>();
                else
                    iMethodStrings.RemoveRange(0, begin_start - body_start);
            }

            //name, parameters, type, IsVirtual, IsAbstract, IsStatic, variables, commands
            Function tfunc = new Function(iname, tparams, ireturntype, tisVirtual, tisAbstract, tIsStatic, tconsts, tvars, iMethodStrings);
            return tfunc;
        }

        private void GenerateFunction(ref List<string> istrings, ref DelphiClassStrings iCurrentDefinition, ref Class iclass, ref List<string> inames, int icounter, int icounter2)
        {
            string theader = iCurrentDefinition.methods[icounter].header;
            string[] theader_arr = theader.Trim().Split('/');
            string tname = theader_arr[2];
            string treturntype = theader_arr[0];
            string ttype = theader_arr[1];

            List<string> tstrings = iCurrentDefinition.methods[icounter].body;
            bool tisVirtual = iCurrentDefinition.methods[icounter].isVirtual;
            bool tisAbstract = iCurrentDefinition.methods[icounter].isAbstract;

            int tnextpos = iCurrentDefinition.methods[icounter].nextpos;
            int const_start = iCurrentDefinition.methods[icounter].const_start, begin_start = iCurrentDefinition.methods[icounter].begin_start, var_start = iCurrentDefinition.methods[icounter].var_start;

            if (tnextpos != -1)
            {
                if (const_start != -1)
                    const_start = const_start - tnextpos;
                if (begin_start != -1)
                    begin_start = begin_start - tnextpos;
                if (var_start != -1)
                    var_start = var_start - tnextpos;
            }


            //Add Function
            iclass.functions.Add(GenerateMethod(ref tstrings,ref theader_arr, theader, tname, treturntype, ttype, const_start, begin_start, var_start, tisVirtual, tisAbstract, ref iclass, ref inames, icounter, icounter2));

            //Add Actions
            List<Function> tactions = new List<Function>();
            for (int i = 0; i < iCurrentDefinition.actions[icounter].Count; i++)
            {
                //ToDo
                theader = iCurrentDefinition.actions[icounter][i].header;
                theader_arr = theader.Trim().Split('/');
                tname = theader_arr[2];
                treturntype = theader_arr[0];
                ttype = theader_arr[1];

                tstrings = iCurrentDefinition.actions[icounter][i].body;
                tisVirtual = iCurrentDefinition.actions[icounter][i].isVirtual;
                tisAbstract = iCurrentDefinition.actions[icounter][i].isAbstract;

                tnextpos = iCurrentDefinition.actions[icounter][i].nextpos;
                const_start = iCurrentDefinition.actions[icounter][i].const_start;
                begin_start = iCurrentDefinition.actions[icounter][i].begin_start;
                var_start = iCurrentDefinition.actions[icounter][i].var_start;

                if (tnextpos != -1)
                {
                    if (const_start != -1)
                        const_start = const_start - tnextpos;
                    if (begin_start != -1)
                        begin_start = begin_start - tnextpos;
                    if (var_start != -1)
                        var_start = var_start - tnextpos;
                }

                tactions.Add(GenerateMethod(ref tstrings, ref theader_arr, theader, tname, treturntype, ttype, const_start, begin_start, var_start, tisVirtual, tisAbstract, ref iclass, ref inames, icounter, icounter2));
            }
            iclass.actions.Add(tactions);
        }

        private void GenerateClasses(ref List<string> istrings, ref List<Class> oclasses, ref List<string> inames, ref List<DelphiClassStrings> idefinitions, ref List<List<string>> iimplementations)
        {
            //For each class discovered
            for (int i = 0; i < inames.Count; i++)
            {
                Class tclass = new Class();
                DelphiClassStrings tdefinition = idefinitions[i];
                //List<string> timplementation = iimplementations[i];
                List<string> tdefinition_parts = new List<string>();
                tclass.name = inames[i];
                tclass.type = tdefinition.type;
                tclass.baseclass = tdefinition.baseclass;
                Variable tvar;
                Constant tconst;

                //Add Constants, Variables, Properties and method names from definitions
                for (int j = 0; j < tdefinition.consts.Count; j++)
                {
                    //tdefinition_parts = RecognizeClassDefinition(tdefinition.consts[j].value);
                    tdefinition.consts[j].value = tdefinition.consts[j].value.Split(';')[0];
                    string[] tconstarr = tdefinition.consts[j].value.Split("//".ToCharArray());
                    string tconstcomment = "";

                    if (tconstarr.Length > 1)
                        tconstcomment = tconstarr[1];

                    tconst = StringToConstant(tconstarr[0], tconstcomment, 0);//new Constant(tdefinition_parts[0], tdefinition_parts[1], tdefinition_parts[2], tdefinition.variables[j].isStatic);
                    tclass.constants.Add(tconst);
                }

                for (int j = 0; j < tdefinition.variables.Count; j++)
                {
                    //tdefinition_parts = RecognizeClassDefinition(tdefinition.variables[j].value);
                    if (tdefinition.variables[j].value.Trim() != "")
                        tclass.variables.AddRange(StringToVariable(tdefinition.variables[j].value));

                    //string[] tvar_arr = tdefinition.variables[j].value.Replace(";","").Trim().Split(':');
                    //string tvartype = tvar_arr[1].Trim();
                    //string[] tvars = tvar_arr[0].Trim().Split(',');

                    //int tlength = tvars.GetLength(0);

                    //for (int jj = 0; jj < tlength; jj++)
                    //{
                    //    tvar = new Variable(tvars[jj], tvartype, tdefinition.variables[j].isStatic);
                    //    tclass.variables.Add(tvar);
                    //}

                    //tvar = new Variable(tdefinition_parts[0], tdefinition_parts[1], tdefinition.variables[j].isStatic);
                    //tclass.variables.Add(tvar);
                }

                for (int j = 0; j < tdefinition.properties.Count; j++)
                {
                    tdefinition_parts = RecognizeClassDefinition(tdefinition.properties[j].value);
                    Property tprop = new Property(tdefinition_parts[0], tdefinition_parts[1], tdefinition_parts[2], tdefinition_parts[3], tdefinition.properties[j].isStatic);
                    tclass.properties.Add(tprop);
                }

                for (int j = 0; j < tdefinition.methods.Count; j++)
                {
                    GenerateFunction(ref istrings, ref tdefinition, ref tclass, ref inames, j, i);
                }
                oclasses.Add(tclass);
            }
        }

        private List<string> GenerateCommands(ref List<string> iDelphiCommands, int istartpos)
        {
            return new List<string>();
        }

        private Constant StringToEnumConstant(string iconst, int iindex, ref string iwholestring)
        {
            string[] tarr = iconst.Split('=');

            //An enum with value = index
            if (tarr.Length == 1)
            {
                return new Constant(iconst, iindex + "");
            }
            //A constant with a value
            else if (tarr.Length == 2)
            {
                return new Constant(tarr[0], tarr[1]);
            }
            //?? Bad input
            else
            {
                //Be sad :-(
                string[] tparams = new string[1];
                tparams[0] = "Bad Enum value ! " + iconst + "," + iindex + " in " + iwholestring + " file: " + outPath;
                //form.Invoke(logsingle, tparams);
                return new Constant();
            }
        }

        private List<Constant> StringToConstant(string istring)
        {
            List<Constant> tlist = new List<Constant>();

            string ttype = "", comment = "";

            comment = GetComment(istring);
            istring = RemoveComment(istring);

            //Replace , with ~@ for easier parsing
            bool tcomma = false;
            for (int i = 0; i < istring.Length; i++)
            {
                char tchar = istring[i];

                if (tchar == "'".ToCharArray()[0])
                {
                    if (tcomma)
                        tcomma = false;
                    else
                        tcomma = true;
                }
                else
                {
                    if (!tcomma && tchar == ',')
                    {
                        istring.Remove(i, 1);
                        istring.Insert(i, "~@");
                    }
                }
            }
            if (istring != "")
            {
                //Check if this a array declaration
                string[] tstr = istring.Split('=');
                string tname = tstr[0].Trim();
                string tvalue = tstr[1].Trim();

                //Check for Arrays
                int tIsArray = istring.IndexOf("array");
                int tOpBracket = istring.IndexOf("(");

                if ((tIsArray != -1) && (istring.IndexOf("[") != -1))
                {
                    tvalue = tvalue.Replace('(', '{');
                    tvalue = tvalue.Replace(')', '}');

                    tstr = tname.Split(':');
                    tname = tstr[0];
                    tstr = tstr[1].Split(' ');
                    ttype = tstr[tstr.GetLength(0) - 1] + "[]";
                    tname = tname.Replace("var", "");
                    tname = tname.Replace(" ", "");
                    tlist.Add(new Constant(tname, ttype, tvalue, comment));
                }
                //Check for DATA TYPES that are Arrays
                else if (tOpBracket != -1 && ((istring.IndexOf("low(") == -1) && (istring.IndexOf("high(") == -1)))
                {
                    tstr = tname.Split(':');
                    tname = tstr[0];
                    tstr = tstr[1].Split(' ');
                    ttype = tstr[tstr.GetLength(0) - 1];
                    tname = tname.Replace("var", "");
                    tname = tname.Replace(" ", "");
                    tlist.Add(new Constant(tname, ttype, tvalue, comment));
                }
                else
                {
                    istring = istring.Split(';')[0];

                    string[] tnames = istring.Split("~@".ToCharArray());

                    for (int i = 0; i < tnames.Length; i++)
                    {
                        tlist.Add(StringToConstant(tnames[i], comment, 0));
                    }
                }
            }
            return tlist;
        }

        private Constant StringToConstant(string iconst, string icomment, int idummy)
        {
            Constant tout;
            string tname = "", tvalue = "", ttype = "";
            string ttest = iconst.Trim();

            if (ttest != "" && ttest != "end" && ttest != "end.")
            {
                string[] tstr = iconst.Split('=');
                tname = tstr[0].Trim();
                tvalue = tstr[1].Trim();
                ttype = "";

                //Check what type this is
                //String
                if (tvalue.IndexOf("'") != -1)
                {
                    ttype = "string";
                    tvalue.Replace("'", "");
                }

                //Hex
                else if (tvalue.IndexOf("$") != -1)
                {
                    ttype = "int";
                    tvalue = tvalue.Replace("$", "");
                    tvalue = "" + Convert.ToInt32(tvalue, 16);
                }

                //Double
                else if (tvalue.IndexOf(".") != -1)
                {
                    ttype = "double";
                }

                else
                {
                    ttype = "int";
                }
            }

            tout = new Constant(tname, ttype, tvalue, icomment);
            return tout;
        }

        private string GetComment(string istring)
        {
            string tcomment = "";
            string[] tarr = istring.Split('{');
            
            if (tarr.Length > 1)
                tcomment = tarr[1];

            istring = tarr[0];

            tarr = istring.Split("//".ToCharArray());

            if (tarr.Length > 1)
                return tcomment + tarr[1];
            else
                return "";
        }

        private string RemoveComment(string istring)
        {
            string[] tarr = istring.Split("//".ToCharArray());
            return tarr[0].Split('{')[0];
        }

        private Variable StringToVariable(string istring, string itype, string icomment)
        {
            Variable tout;
            string tname = "", ttype = "", tvalue = "", comment = icomment;

            if (itype != "")
            {
                if (itype.IndexOf("set of") != -1)
                    itype = itype.Replace("set of", "HashSet<").Replace(";", ">;");
                else if (itype.IndexOf("array of") != -1)
                    itype = itype.Replace("array of", "").Replace(";", "[];");

                if (istring.IndexOf('=') != -1)
                {
                    string[] tstrarr = istring.Split('=');
                    tvalue = tstrarr[1];
                    istring = tstrarr[0];
                }

                tname = istring;
                ttype = itype;
            }

            tout = new Variable(tname, ttype, tvalue, comment, false);
            return tout;
        }

        private List<Variable> StringToVariable(string istring)
        {
            return StringToVariable(istring, "");
        }

        private List<Variable> StringToVariable(string istring, string icomment)
        {
            List<Variable> tlist = new List<Variable>();

            if (istring.Trim() != "implementation")
            {
                string ttype = "", comment = "";

                comment = icomment + GetComment(istring);
                istring = RemoveComment(istring);

                if (istring.Trim() != "")
                {
                    string[] tarr = istring.Split(':');
                    string[] tnames = tarr[0].Split(',');
                    ttype = tarr[1];

                    for (int i = 0; i < tnames.Length; i++)
                    {
                        tlist.Add(StringToVariable(tnames[i], ttype, comment));
                        comment = "";
                    }
                }
            }
            return tlist;
        }

        private Enum StringToEnum(string istring)
        {
            Enum tout;
            List<Constant> tconsts = new List<Constant>();

            //Split into arguments
            string[] tenum_arguments = istring.Split('(');
            //Get name
            string tname = tenum_arguments[0].Split('=')[0].Trim();
            
            //Remove trailing ')' and ';'
            tenum_arguments[1] = tenum_arguments[1].Replace(')', ' ').Replace(';', ' ');

            string[] tconstants = tenum_arguments[1].Split(',');
            tconstants[tconstants.GetLength(0) - 1].Trim();

            //Loop and add all constants
            for (int i = 0; i < tconstants.GetLength(0); i++)
            {
                Constant tconst = StringToEnumConstant(tconstants[i], i, ref istring);
                tconsts.Add(tconst);
            }

            tout = new Enum(tname, tconsts);
            return tout;
        }

        private TypeAlias StringToType(string istring)
        {
            TypeAlias tout;
            string tname, ttype;

            string[] tarr = istring.Split('=');
            tname = tarr[0];
            ttype = tarr[1];

            tout = new TypeAlias(tname, ttype);
            return tout;
        }

        private void GenerateGlobalClass(string iname, ref Class oclassGlobals, ref List<string> iconstsGlobal, ref List<string> ienumsGlobal, ref List<string> itypesGlobal, ref List<string> ivarsGlobal)
        {
            List<Constant> tconstants = new List<Constant>();
            List<Variable> tvars = new List<Variable>();
            List<Enum> tenums = new List<Enum>();
            List<TypeAlias> ttypes = new List<TypeAlias>();

            for (int i = 0; i < iconstsGlobal.Count; i++)
            {
                if (iconstsGlobal[i].Trim() != "")
                {
                    string[] tarr = iconstsGlobal[i].Split(@"//".ToCharArray());
                    string tconst = tarr[0].Trim();

                    int tcomment = tconst.IndexOf('{');
                    if (tcomment != -1)
                    {
                        do
                        {
                            i++;
                            tcomment = tconst.IndexOf('}');
                        } while ((tcomment == -1) && (i < iconstsGlobal.Count));
                    }
                    if (i < iconstsGlobal.Count)
                    {
                        if (tconst != "")
                        {
                            //Check to see if we have the whole statement

                            int tcolon;
                        SemiColonCheck:

                            tcolon = tconst.IndexOf(';');
                            if (tcolon == -1)
                            {
                                i++;
                                if (i >= iconstsGlobal.Count)
                                    goto Out;

                                tconst = tconst + iconstsGlobal[i];
                                tarr = tconst.Split(@"//".ToCharArray());
                                tconst = tarr[0].Trim();
                                goto SemiColonCheck;
                            }
                        Out:
                            tconstants.AddRange(StringToConstant(tconst));
                        }
                    }
                }
            }

            for (int i = 0; i < ienumsGlobal.Count; i++)
            {
                tenums.Add(StringToEnum(ienumsGlobal[i]));
            }

            for (int i = 0; i < itypesGlobal.Count; i++)
            {
                ttypes.Add(StringToType(itypesGlobal[i]));
            }

            for (int i = 0; i < ivarsGlobal.Count; i++)
            {
                if (ivarsGlobal[i].Trim() != "")
                    tvars.AddRange(StringToVariable(ivarsGlobal[i]));
            }

            oclassGlobals = new Class();
            oclassGlobals.name = iname;
            oclassGlobals.variables = tvars;
            oclassGlobals.enums = tenums;
            oclassGlobals.constants = tconstants;
            oclassGlobals.types = ttypes;
        }

        private void GenerateIncludes(ref List<string> Objects_UsesInterface, ref List<string> Uses_Interface)
        {
            Objects_UsesInterface = new List<string>();

            for (int i = 0; i < Uses_Interface.Count; i++ )
                if ((Uses_Interface[i].IndexOf('{') == -1) && (Uses_Interface[i].IndexOf('}') == -1) && (Uses_Interface[i].Trim() != ""))
                    Objects_UsesInterface.Add(Uses_Interface[i].Replace(";","").Replace(",","").Trim());
        }

        public void log(List<string> imessages)
        {
            //for (int i = 0; i < imessages.Count; i++)
                //logsingle(imessages[i]);//Console.WriteLine(imessages[i]);
        }
    }    
}
