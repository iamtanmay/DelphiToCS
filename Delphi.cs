using System;
using System.Text;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Translator
{
    public class DelphiClassStrings
    {
        public string name;
        public List<DelphiMethodStrings> methods;
        public List<string> properties;
        public List<string> variables;

        public DelphiClassStrings()
        {
            methods = new List<DelphiMethodStrings>();
            properties = new List<string>();
            variables = new List<string>();
        }

        public void AddMethodDefinition(string iheader, bool iAbstract, bool iVirtual, bool iOverloaded)
        {
            DelphiMethodStrings tmethod = new DelphiMethodStrings(iheader, null);
            tmethod.isAbstract = iAbstract;
            tmethod.isVirtual = iVirtual;
            tmethod.isOverloaded = iOverloaded;
        }

        public void AddMethodBody(string iheader, List<string> ibody)
        {
            for (int i = 0; i < methods.Count; i++)
                if (methods[i].header == iheader)
                    methods[i].body = ibody;
        }
    }

    public class DelphiMethodStrings
    {
        public string header ="";
        public bool isAbstract = false, isVirtual = false, isOverloaded = false;

        public List<string> body;

        public DelphiMethodStrings(string iheader, List<string> ibody)
        {
            header = iheader;
            body = new List<string>();
            body = ibody;
        }
    }

    public class Delphi
    {
    	//Output
        public string name;        
        public Script script;
        
    	//Enums, consts, types
    	public Class classLocals, classGlobals;
        public List<Constant> localConsts, globalConsts;
    	public List<Enum> localEnums, globalEnums;
    	public List<Type> localTypes, globalTypes;
    	
    	public List<string> interfaceUses, implementationUses;
        
    	//Log 
        public LogDelegate logsingle;
    	
        //Bookmarks
    	//Sections
        private int startHeader = -1, endHeader = -1, startInterface = -1, endInterface = -1, startVar = -1, endVar = -1, startImplementation = -1, endImplementation = -1;    	
    	//Subsections
    	private List<int> startUses, endUses, startClassInterface, endClassInterface, startClassImplementation, endClassImplementation, startConsts, endConsts, startEnums, endEnums, startTypes, endTypes;
    	
    	
    	//Raw strings
    	//Header, class names, SubSection names, Uses interface, Uses implementation, (Global and Local): consts, enums, types and vars 
    	private List<string> classNames, SubSection_Names, Uses_Interface, Uses_Implementation, ConstsGlobal, EnumsGlobal, TypesGlobal, VarsGlobal, ConstsLocal, EnumsLocal, TypesLocal, VarsLocal;
    	//Raw text for the classes
    	private List<DelphiClassStrings> classDefinitions;
        private List<List<string>> classImplementations;

    	
    	//Keywords
        //Divide script sections
        public static string[] sectionKeys = {"var","implementation","interface" };
    	//Divide section subsections
    	public static string[] subsectionKeys = {"type","const","uses"};
        //Divide subsection kinds
        public static string[] interfaceKindKeys = {"class","record","procedure","function" };
        public static string[] implementKindKeys = { "const", "constructor", "class var", "class function", "class procedure", "procedure", "function", "uses" };
        //Divide method commands
        public static string[] methodKeys = {"var","begin","label","end","try","catch","finally" };
    	
        //Divide class defintion
        public static string[] classKeys = {"public","private","const","constructor","class var","class function","class procedure","procedure","function","property"};

        List<int> Section_Bookmarks, EnumLocalStarts, EnumLocalEnds, EnumGlobalEnds;
        List<string> Section_Names;

    	//ireadheader is a flag if header is to be read. iheaderstart is normally "{ --" in Zinsser Delphi units, and iheaderend is "-- }" 
        public void Read(ref List<string> istrings, bool ireadheader, string iheaderstart, string iheaderend, LogDelegate ilog)
        {
        	//Initialise
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

            startHeader =  endHeader =  startInterface =  endInterface =  startImplementation =  endImplementation =  startInterface =  endInterface = -1;
            
            //Read unit name
            name = ((istrings[FindStringInList("unit", ref istrings, 0, true)].Replace(' ', ';')).Split(';'))[1];

            //Bookmark sections
            IndexStructure(ref istrings, ireadheader, iheaderstart, iheaderend);

            //Break up the text into Lists of strings for different parts
            if (ireadheader)
    			ParseHeader(ref istrings, ireadheader, iheaderstart, iheaderend);

			//Convert Delphi symbols to C# symbols
			//Beautify(ref istrings);

            ParseInterface(ref istrings, ref Uses_Interface, ref classNames, ref classDefinitions, ref ConstsGlobal, ref EnumsGlobal, ref TypesGlobal);
            ParseVar(ref istrings, ref VarsGlobal);
            ParseImplementation(ref istrings, ref Uses_Implementation, ref classNames, ref classDefinitions, ref ConstsLocal, ref EnumsLocal, ref TypesLocal);

            //Generate the text pieces into class objects
            GenerateGlobalClass(ref classGlobals, ref ConstsGlobal, ref EnumsGlobal, ref TypesGlobal, ref VarsGlobal);
            script.classes.Add(classGlobals);
            VarsGlobal.Clear();

            GenerateGlobalClass(ref classLocals, ref ConstsLocal, ref EnumsLocal, ref TypesLocal, ref VarsGlobal);
            script.classes.Add(classLocals);

            GenerateClasses(ref script.classes, ref classNames, ref classDefinitions, ref classImplementations);
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
	            			script.header.Add(istrings[i]);
	            	else
		            	throw new Exception("Header end not found");
        }
    	
        private void Beautify(ref List<string> istrings)
        {
        	for (int i = 0; i < istrings.Count; i++)
                istrings[i] = Utilities.Beautify(istrings[i]);
        }
        
        //oclassdefinitions is a list of class variables, properties, function and procedure definitions 
        private void ParseInterface(ref List<string> istrings, ref List<string> ouses, ref List<string> oclassnames, ref List<DelphiClassStrings> oclassdefinitions, ref List<string> oconsts, ref List<string> oenums, ref List<string> otypes)
        {
            int tcurr_string_count = FindNextInterfaceSubSection(ref istrings, startInterface), tnext_subsection_pos = -1;

            //Interface sub sections
            while (tcurr_string_count != -1)
            {
            	tnext_subsection_pos = FindNextInterfaceSubSection(ref istrings, tcurr_string_count + 1);

                if (tnext_subsection_pos == -1)
                    tnext_subsection_pos = endInterface;

            	switch (RecognizeKey(istrings[tcurr_string_count], ref subsectionKeys))
            	{
                    case "const":   oconsts.AddRange(GetStringSubList(ref istrings, tcurr_string_count + 1, tnext_subsection_pos)); break;

        			case "type": 	ParseInterfaceTypes(ref istrings, ref ouses, ref oclassnames, ref oclassdefinitions, ref oconsts, ref oenums, ref otypes, tcurr_string_count); break;

                    case "uses":    ouses.AddRange(GetStringSubList(ref istrings, tcurr_string_count + 1, tnext_subsection_pos)); break;

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

            if ((tarray.GetLength(0) == 2) && ((tarray[0][0] == 'f') || (tarray[0][0] == 'F')) && (istring.Split(':').GetLength(0) == 2))
                return true;
            return false;
        }

        private void ParseInterfaceTypes(ref List<string> istrings, ref List<string> ouses, ref List<string> oclassnames, ref List<DelphiClassStrings> oclassdefinitions, ref List<string> oconsts, ref List<string> oenums, ref List<string> otypes, int istartpos)
        {
            int tnext_subsection_pos = -1, tcurr_string_count = istartpos;
            string tclassname = "";

            //Interface sub sectionsm
            while (tcurr_string_count < endInterface)
            {
                //Ignore comments and empty lines
                if (!(RecognizeComment(istrings[tcurr_string_count])) && !(RecognizeEmptyLine(istrings[tcurr_string_count])))
                {
                    switch (RecognizeKey(istrings[tcurr_string_count], ref interfaceKindKeys))
                    {
                        case "class":   tnext_subsection_pos = FindNextSymbol(ref istrings, "end;", tcurr_string_count);
                                        tclassname = ((Regex.Replace(istrings[tcurr_string_count], @"\s+", "")).Split('='))[0];
                                        oclassnames.Add(tclassname);

                                        if (tnext_subsection_pos == -1)
                                            throw new Exception("Incomplete class definition");

                                        DelphiClassStrings tclassdef = new DelphiClassStrings();
                                        tclassdef.name = tclassname;
                                        tclassdef = ParseInterfaceClass(ref istrings, tclassname, tcurr_string_count, tnext_subsection_pos);

                                        oclassdefinitions.Add(tclassdef);
                                        tcurr_string_count = tnext_subsection_pos + 1;
                                        break;

                        case "record":  tnext_subsection_pos = FindNextSymbol(ref istrings, "end;", tcurr_string_count);
                                        tclassname = ((Regex.Replace(istrings[tcurr_string_count], @"\s+", "")).Split('='))[0];
                                        oclassnames.Add(tclassname);

                                        if (tnext_subsection_pos == -1)
                                            throw new Exception("Incomplete class definition");

                                        tclassdef = new DelphiClassStrings();
                                        tclassdef.name = tclassname;
                                        tclassdef = ParseInterfaceClass(ref istrings, tclassname, tcurr_string_count, tnext_subsection_pos);

                                        oclassdefinitions.Add(tclassdef);
                                        tcurr_string_count = tnext_subsection_pos + 1;
                                        break;

                        //Check if Enum or Type Alias, else Log unrecognized sub section
                        default:        //Enum start
                                        if (istrings[tcurr_string_count].IndexOf('(') != -1)
                                        {
                                            tnext_subsection_pos = FindNextSymbol(ref istrings, ");", tcurr_string_count);

                                            if (tnext_subsection_pos == -1)
                                                throw new Exception("Incomplete Enum definition");

                                            oenums.Add(string.Concat((GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos).ToArray())));
                                            tcurr_string_count = tnext_subsection_pos + 1;
                                        }
                                        //Type Alias start
                                        else if ((istrings[tcurr_string_count].IndexOf('=') != -1) && (istrings[tcurr_string_count].IndexOf(';') != -1))
                                        {
                                            //tnext_subsection_pos = FindNextSymbol(ref istrings, ";", tcurr_string_count);

                                            //if (tnext_subsection_pos == -1)
                                            //    throw new Exception("Incomplete Type Alias definition");

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
                                        }
                                        break;
                    }
                    tcurr_string_count = FindNextInterfaceSubType(ref istrings, tcurr_string_count + 1);
                }
                else
                {
                    tcurr_string_count++;
                }
            }
        }

        private DelphiMethodStrings ParseInterfaceMethod(string imethodtype, ref List<string> iFiltered_strings, int icurr_string_count)
       {
            bool toverload = false, tvirtual = false, tabstract = false;
            int tnext_pos = FindNextKey(ref iFiltered_strings, ref classKeys, icurr_string_count + 1);
            string tfuncstring = "";

            //Add all the indented lines in function title
            for (int i = icurr_string_count; i < tnext_pos; i++)
            {
                tfuncstring = tfuncstring + iFiltered_strings[i].Trim();
            }

            //Check and strip out overload, virtual, abstract
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
                tmethodname = treturntypearray[0];
                tparameters = "";
            }
            //If there are parameters
            else
            {
                treturntype = treturntypearray[1].Split(';')[0];
                treturntypearray = treturntypearray[0].Split('(');
                treturntype = treturntype.Split(':')[1];
                tmethodname = treturntypearray[0];
                tparameters = treturntypearray[1];
            }

            string tfunctionstring = treturntype + "_" + imethodtype + "_" + tmethodname + "_" + tparameters;
            DelphiMethodStrings tfunction = new DelphiMethodStrings(tfunctionstring, null);

            tfunction.isAbstract = tabstract;
            tfunction.isVirtual = tvirtual;
            tfunction.isOverloaded = toverload;

            return tfunction;
        }

        private DelphiClassStrings ParseInterfaceClass(ref List<string> istrings, string iclassname, int icurr_string_count, int inext_subsection_pos)
        {
            DelphiClassStrings tout = new DelphiClassStrings();
            List<string> tstrings = new List<string>(), tvars = new List<string>(), tproperties = new List<string>();
            List<DelphiMethodStrings> tmethods = new List<DelphiMethodStrings>();

            int tcurr_string_count = icurr_string_count;

            //Variable filter. Variables are removed so that functions can be read without confusion
            int i = icurr_string_count;
            int new_next_subsection_pos;

            while (i < inext_subsection_pos)
            {
                if (CheckVariable(istrings[i]))
                    tvars.Add(istrings[i]);
                else
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

                switch (RecognizeKey(tstrings[tcurr_string_count], ref classKeys))
                {
                    //{ "public", "private", "const", "constructor", "class function", "class procedure", "procedure", "function", "property"};
                    case "public":  //ignore
                        break;

                    case "private": //ignore
                        break;

                    case "const": break;

                    case "class var": tvars.Add(istrings[tcurr_string_count]);
                        break;

                    case "constructor":
                        tmethods.Add(ParseInterfaceMethod("constructor", ref tstrings, tcurr_string_count));
                        break;

                    case "class function":
                        tmethods.Add(ParseInterfaceMethod("class_function", ref tstrings, tcurr_string_count));
                        break;

                    case "class procedure":
                        tmethods.Add(ParseInterfaceMethod("class_procedure", ref tstrings, tcurr_string_count));
                        break;

                    case "procedure":
                        tmethods.Add(ParseInterfaceMethod("procedure", ref tstrings, tcurr_string_count));
                        break;

                    case "function":
                        tmethods.Add(ParseInterfaceMethod("function", ref tstrings, tcurr_string_count));
                        break;

                    case "property": tproperties.Add(istrings[tcurr_string_count]);
                        break;

                    default: break;
                }
                tcurr_string_count = tnext_subsection_pos;
            }

            tout.name = iclassname;
            tout.methods = tmethods;
            tout.variables = tvars;
            tout.properties = tproperties;

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
            while (tcurr_string_count != -1)
            {
                tnext_subsection_pos = FindNextKey(ref istrings, ref implementKindKeys, tcurr_string_count);

                if (tnext_subsection_pos == -1)
                    tnext_subsection_pos = endImplementation;

                switch (RecognizeKey(istrings[tcurr_string_count], ref implementKindKeys))
            	{
                    case "class function":  //Break down function elements
                                        string tmethodtype = "class_function";
                                        ParseImplementationMethod(tmethodtype, tcurr_string_count, tnext_subsection_pos, ref istrings, ref oclassnames, ref oclassimplementations);                                        
                                        break;

                    case "class procedure": //Break down function elements
                                        tmethodtype = "class_procedure";
                                        ParseImplementationMethod(tmethodtype, tcurr_string_count, tnext_subsection_pos, ref istrings, ref oclassnames, ref oclassimplementations);                                        
                                        break;

                    case "function":    //Break down function elements
                                        tmethodtype = "function";
                                        ParseImplementationMethod(tmethodtype, tcurr_string_count, tnext_subsection_pos, ref istrings, ref oclassnames, ref oclassimplementations);                                        
                                        break;

                    case "procedure":   //Break down function elements
                                        tmethodtype = "procedure";
                                        ParseImplementationMethod(tmethodtype, tcurr_string_count, tnext_subsection_pos, ref istrings, ref oclassnames, ref oclassimplementations);                                        
                                        break;

                    case "constructor": //Break down function elements
                                        tmethodtype = "constructor";                                        
                                        ParseImplementationMethod(tmethodtype, tcurr_string_count, tnext_subsection_pos, ref istrings, ref oclassnames, ref oclassimplementations);                                        
                                        break;

                    case "const":       oconsts.AddRange(GetStringSubList(ref istrings, tcurr_string_count + 1, tnext_subsection_pos)); 
                                        break;

                    case "uses":        ouses.AddRange(GetStringSubList(ref istrings, tcurr_string_count + 1, tnext_subsection_pos)); 
                                        break;
                    
                    //Log unrecognized sub section
                    default:            List<string> tlogmessages = new List<string>();
                                        tlogmessages.Add("Interface sub section not recognized " + tcurr_string_count);
                                        tlogmessages.AddRange(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos));
                                        log(tlogmessages);
                                        break;
                }
                tcurr_string_count = tnext_subsection_pos + 1;
            }     
        }
        
        private void ParseImplementationMethod(string imethodtype, int icurr_string_count, int inext_subsection_pos, ref List<string> istrings, ref List<string> oclassnames, ref List<DelphiClassStrings> oclassimplementations)
        {
            string tclassname;

            int tnext_pos = FindNextSymbol(ref istrings, "begin", icurr_string_count);
            string tfuncstring = "";

            //Add all the indented lines in function title
            for (int i = icurr_string_count; i < tnext_pos; i++)
            {
                tfuncstring = tfuncstring + istrings[i].Trim();
            }

            string[] tclassnamearray = tfuncstring.Split('.');
            tclassname = tclassnamearray[0].Split(' ')[1];

            for (int i = 0; i < oclassimplementations.Count; i++)
            {
                //Find the class
                if (oclassimplementations[i].name == tclassname)
                {
                    List<string> tlist = ParseImplementationMethodBody(ref istrings, ref tclassnamearray, tclassname, imethodtype, icurr_string_count, inext_subsection_pos);
                    string theader = tlist[0];
                    tlist.RemoveAt(0);
                    oclassimplementations[i].AddMethodBody(theader, tlist);
                    break;
                }
            }
        }

        private List<string> ParseImplementationMethodBody(ref List<string> istrings, ref string[] iclassnamearray, string iclassname, string itype, int ipos, int inextpos)
        {
            //Break down function elements
            string treturntype, tparameters, tmethodname;
            string[] treturntypearray = iclassnamearray[1].Split(')');

            iclassname = iclassnamearray[0].Split(' ')[1];

            //If there are no parameters
            if (treturntypearray.GetLength(0) == 1)
            {
                treturntypearray = treturntypearray[0].Split(':');
                treturntype = treturntypearray[1].Split(';')[0];
                tmethodname = treturntypearray[0];
                tparameters = "";
            }
            //If there are parameters
            else
            {
                treturntype = treturntypearray[1].Split(';')[0];
                treturntypearray = treturntypearray[0].Split('(');
                treturntype = treturntype.Split(':')[1];
                tmethodname = treturntypearray[0];
                tparameters = treturntypearray[1];
            }

            string tfunctionstring = treturntype + "_" + itype + "_" + tmethodname + "_" + tparameters;
            List<string> tout = new List<string>();
            tout.Add(tfunctionstring);
            tout.AddRange(GetStringSubList(ref istrings, ipos, inextpos));

            return tout;
        }

    	private void IndexStructure(ref List<string> istrings, bool ireadheader, string iheaderstart, string iheaderend)
        {
            if (ireadheader)
            {
            	startHeader = FindStringInList(iheaderstart, ref istrings, 0, true);
            	if (startHeader != -1)
            	{
            		endHeader = FindStringInList(iheaderend, ref istrings, startHeader, true);
            		if (endHeader == -1)
		        		throw new Exception("Header end not found");

		        	Section_Names.Add("Header");
	        		Section_Bookmarks.Add(startHeader);
        		}
            }

            startInterface = FindStringInList("interface", ref istrings, 0, true) + 1;
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
	    		endImplementation = FindNextSection(ref istrings, startImplementation);
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
            for (int i = istartpoint; i < iarray.Count; i++)
            {
                return i;
            }
            return -1;
        }

        static private int FindNextSymbol(ref List<string> istrings, string isymbol, int istartpos)
        {
            string[] tEnd = new string[1];
            tEnd[0] = isymbol;

            for (int i = istartpos; i < istrings.Count; i++)
            {
                if (Check_If_StringList_Elements_Exist_In_String(istrings[i], ref tEnd) != -1)
                    return i;
            }
            return -1;
        }

        static private int FindNextKey(ref List<string> istrings, ref string[] ikeys, int istartpos)
        {
            for (int i = istartpos; i < istrings.Count; i++)
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
           	for (int i=0; i < ikeys.GetLength(0); i++)
           	{
           		if (istring.IndexOf(ikeys[i]) != -1)
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

            if (isproperty != -1)
            {
                string tstr = iline.Replace("property", "").Replace(";", "").Trim();
                tstr_arr = tstr.Split(':');
                
                //Add name
                tout.Add(tstr_arr[0]);

                //Check if read
                int tread = tstr_arr[1].IndexOf("read");
                int twrite = tstr_arr[1].IndexOf("write");

                if ((tread == -1) && (twrite == -1))
                {
                    tout.Add(tstr_arr[1].Trim());
                }
                else if (tread == -1)
                {
                    tstr_arr[1] = tstr_arr[1].Replace("read", ";");
                    tstr_arr = tstr_arr[1].Split(';');
                    
                    //Add type and read
                    tout.Add(tstr_arr[0].Trim());
                    tout.Add(tstr_arr[1].Trim());
                }
                else if (twrite == -1)
                {
                    tstr_arr[1] = tstr_arr[1].Replace("write", ";");
                    tstr_arr = tstr_arr[1].Split(';');

                    //Add type and write
                    tout.Add(tstr_arr[0].Trim());
                    tout.Add(tstr_arr[1].Trim());
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
            else
            {
                tstr_arr = iline.Split(':');
                tout.Add(tstr_arr[0].Trim());
                tout.Add(tstr_arr[1].Replace(";", ""));
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
            //Split string to check in, and element to check against, according to spaces. 
            //If the first element word is found, check for the rest in order. Otherwise return false.

            //Split string into words
            string[] tstring_words = istring.Split(' ');

            //Loop through the elements array and check each one
            for (int i = 0; i < ielements.GetLength(0); i++)
           	{
                //Split the current element into words
                string[] telement_words = ielements[i].Split(' ');

                //Find the first word. If not found return -1
                if (tstring_words.FirstOrDefault(e => e == telement_words[j]) != null)
                {
                    int tcounter = telement_words.GetLength(0);
                    for (int j = 0; j < telement_words.GetLength(0); j++)
                    {
                        if (tstring_words.FirstOrDefault(e => e == telement_words[j]) != null)
                            tcounter--;
                    }

                }

           	}
           	return -1;
		}
                                                           
	    static private int FindStringInList(string ifind, ref List<string> iarray, int iindex, bool imatchCase)
		{
	    	for (int i = iindex; i < iarray.Count; i++)
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

        private void GenerateClasses(ref List<Class> oclasses, ref List<string> inames, ref List<DelphiClassStrings> idefinitions, ref List<List<string>> iimplementations)
        {
            //For each class discovered
            for (int i = 0; i < inames.Count; i++)
            {
                Class tclass = new Class();
                DelphiClassStrings tdefinition = idefinitions[i];
                List<string> timplementation = iimplementations[i];
                List<string> tdefinition_parts = new List<string>();
                tclass.name = inames[i];
                Variable tvar;

                //Add Variables, Properties and method names from definitions
                for (int j = 0; j < tdefinition.variables.Count; j++)
                {
                    tdefinition_parts = RecognizeClassDefinition(tdefinition.variables[j]);
                    tvar = new Variable(tdefinition_parts[1], tdefinition_parts[2]);
                    tclass.variables.Add(tvar);
                }

                for (int j = 0; j < tdefinition.properties.Count; j++)
                {
                    tdefinition_parts = RecognizeClassDefinition(tdefinition.properties[j]);
                    Property tprop = new Property(tdefinition_parts[1], tdefinition_parts[2], tdefinition_parts[3], tdefinition_parts[4]);
                    tclass.properties.Add(tprop);
                }

                for (int j = 0; j < tdefinition.methods.Count; j++)
                {
                    string theader = tdefinition.methods[j].header;
                    string[] theader_arr = theader.Split('_');
                    string tname = theader_arr[2];
                    string ttype = theader_arr[0];
                    string tIsStatic = theader_arr[1];
                    string tIsAbstract = "" + tdefinition.methods[j].isAbstract;
                    string tIsOverloaded = "" + tdefinition.methods[j].isOverloaded;
                    string tIsVirtual = "" + tdefinition.methods[j].isVirtual;

                    List<Variable> tparams = new List<Variable>();

                    int ii = 6;
                    for (; ii < tdefinition_parts.Count; )
                    {
                        tvar = new Variable(tdefinition_parts[ii], tdefinition_parts[ii + 1]);
                        tparams.Add(tvar);
                        ii += 2;
                    }

                    //name, parameters, type, IsVirtual, IsAbstract, IsStatic, variables, commands
                    Function tfunc = new Function(tname, tparams, ttype, Convert.ToBoolean(tIsVirtual), Convert.ToBoolean(tIsAbstract), Convert.ToBoolean(tIsStatic), new List<Variable>(), new List<string>());
                    tclass.functions.Add(tfunc);
                }
            }
        }

        private List<string> GenerateCommands(ref List<string> iDelphiCommands, int istartpos)
        {
            return new List<string>();
        }

        private Constant StringToConstant(string iconst)
        {
            Constant tout;
            string[] tstr = iconst.Split('=');
            string tname = tstr[0].Trim();
            string tvalue = tstr[1].Trim();
            tvalue = tvalue.Split(';')[0];
            string ttype = "";

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
                tvalue.Replace("$", "");
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

            tout = new Constant(tname, ttype, tvalue);
            return tout;
        }

        private Variable StringToVariable(string istring)
        {
            Variable tout;
            string tname, ttype;

            string[] tarr = istring.Split(':');
            tname = tarr[0];
            ttype = tarr[1];

            tout = new Variable(tname, ttype);
            return tout;
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
            tenum_arguments[1].Replace(')', ' ');
            tenum_arguments[1].Replace(';', ' ');

            string[] tconstants = tenum_arguments[1].Split(',');
            tconstants[tconstants.GetLength(0) - 1].Trim();

            //Loop and add all constants
            for (int i = 0; i < tconstants.GetLength(0); i++)
            {
                Constant tconst = StringToConstant(tconstants[i]);
                tconsts.Add(tconst);
            }

            tout = new Enum(tname, tconsts);
            return tout;
        }

        private Type StringToType(string istring)
        {
            Type tout;
            string tname, ttype;

            string[] tarr = istring.Split(':');
            tname = tarr[0];
            ttype = tarr[1];

            tout = new Type(tname, ttype);
            return tout;
        }

        private void GenerateGlobalClass(ref Class oclassGlobals, ref List<string> iconstsGlobal, ref List<string> ienumsGlobal, ref List<string> itypesGlobal, ref List<string> ivarsGlobal)
        {
            List<Constant> tconstants = new List<Constant>();
            List<Variable> tvars = new List<Variable>();
            List<Enum> tenums = new List<Enum>();
            List<Type> ttypes = new List<Type>();

            for (int i = 0; i < iconstsGlobal.Count; i++)
            {
                tconstants.Add(StringToConstant(iconstsGlobal[i]));
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
                tvars.Add(StringToVariable(ivarsGlobal[i]));
            }

            oclassGlobals.variables = new List<Variable>();
            oclassGlobals.enums = new List<Enum>();
            oclassGlobals.constants = new List<Constant>();
            oclassGlobals.types = new List<Type>();

            oclassGlobals.variables = tvars;
            oclassGlobals.enums = tenums;
            oclassGlobals.constants = tconstants;
            oclassGlobals.types = ttypes;
        }

        private void GenerateIncludes(ref List<string> Objects_UsesInterface, ref List<string> Uses_Interface)
        {
        }

        public void log(List<string> imessages)
        {
            for (int i = 0; i < imessages.Count; i++)
                Console.WriteLine(imessages[i]);
        }
    }    
}
