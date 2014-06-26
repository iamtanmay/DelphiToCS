using System;
using System.Text;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Translator
{
    public struct DelphiClassDef
    {
        public List<string> methods;
        public List<string> properties;
        public List<string> variables;

        public DelphiClassDef()
        {
            methods = new List<string>();
            properties = new List<string>();
            variables = new List<string>();
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
    	delegate void delegate_log(List<string> imessages);
    	delegate_log log; 
    	
        //Bookmarks
    	//Sections
    	private int startHeader, endHeader, startInterface, endInterface, startVar, endVar, startImplementation, endImplementation;    	
    	//Subsections
    	private List<int> startUses, endUses, startClassInterface, endClassInterface, startClassImplementation, endClassImplementation, startConsts, endConsts, startEnums, endEnums, startTypes, endTypes;
    	
    	
    	//Raw strings
    	//Header, class names, SubSection names, Uses interface, Uses implementation, (Global and Local): consts, enums, types and vars 
    	private List<string> classNames, SubSection_Names, Uses_Interface, Uses_Implementation, ConstsGlobal, EnumsGlobal, TypesGlobal, VarsGlobal, ConstsLocal, EnumsLocal, TypesLocal, VarsLocal;
    	//Raw text for the classes
    	private List<DelphiClassDef> classDefinitions;
        private List<List<string>> classImplementations;

    	
    	//Keywords
        //Divide script sections
        public static string[] sectionKeys = { "var", "implementation", "interface" };
    	//Divide section subsections
    	public static string[] subsectionKeys = { "type", "const", "uses"};
        //Divide subsection kinds
        public static string[] interfaceKindKeys = { "class", "record", "procedure", "function" };
        public static string[] implementKindKeys = { "const", "constructor", "class function", "class procedure", "procedure", "function", "uses" };
        //Divide method commands
        public static string[] methodKeys = { "var", "begin", "label", "end", "try", "catch", "finally" };
    	
        //Divide class defintion
        public static string[] classKeys = { "public", "private", "const", "constructor", "class function", "class procedure", "procedure", "function", "property"};

        List<int> Section_Bookmarks, EnumLocalStarts, EnumLocalEnds, EnumGlobalEnds;
        List<string> Section_Names;

    	//ireadheader is a flag if header is to be read. iheaderstart is normally "{ --" in Zinsser Delphi units, and iheaderend is "-- }" 
        public void Read(ref List<string> istrings, bool ireadheader, string iheaderstart, string iheaderend)
        {
        	//Initialise
            script = new Script();
            
            name = "";
            
            classNames = new List<string>();
            classDefinitions = new List<DelphiClassDef>();
            classImplementations = new List<List<string>>();
            
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
			Beautify(ref istrings);

            ParseInterface(ref istrings, ref Uses_Interface, ref classNames, ref classDefinitions, ref ConstsGlobal, ref EnumsGlobal, ref TypesGlobal);
            ParseVar(ref istrings, ref VarsGlobal);
            ParseImplementation(ref istrings, ref Uses_Implementation, ref classNames, ref classImplementations, ref ConstsLocal, ref EnumsLocal, ref TypesLocal);

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
                
        private void GenerateClasses(ref List<Class> oclasses, ref List<string> inames, ref List<List<string>> idefinitions, ref List<List<string>> iimplementations)
        {
        	//For each class discovered
        	for (int i=0; i< inames.Count; i++)
        	{
        		Class tclass = new Class();
        		List<string> tdefinition = idefinitions[i];
        		List<string> timplementation = iimplementations[i];
        		tclass.name = inames[i];
                Variable tvar;

        		//Add Variables, Properties and method names from definitions
        		for(int j=0; j < tdefinition.Count; j++)
        		{
        			//Contains all the parameters for each 
        			List<string> tdefinition_parts = RecognizeClassDefinition(tdefinition[j]);
        			switch(tdefinition_parts[0])
        			{
    					case "Variable":	//name, type
                                            tvar = new Variable(tdefinition_parts[1], tdefinition_parts[2]);
    										tclass.variables.Add(tvar);
    										break;
    										
						case "Property":	//name, type, read, write
                                            Property tprop = new Property(tdefinition_parts[1], tdefinition_parts[2], tdefinition_parts[3], tdefinition_parts[4]);
    										tclass.properties.Add(tprop);
    										break;
    					
						//This is for both functions and procedures. Difference is that type is returned "" for procedures    										
                        case "Function":    string tname = tdefinition_parts[1];
                                            string ttype = tdefinition_parts[2];
                                            string tIsAbstract = tdefinition_parts[3];
                                            string tIsStatic = tdefinition_parts[4];
                                            string tIsVirtual = tdefinition_parts[5];

                                            List<Variable> tparams = new List<Variable>();

                                            int ii = 6;
    										for (; ii< tdefinition_parts.Count; )
    										{
                                                tvar = new Variable(tdefinition_parts[ii], tdefinition_parts[ii + 1]);
                                                tparams.Add(tvar);
												ii += 2;    											
    										}

    										//name, parameters, type, IsVirtual, IsAbstract, IsStatic, variables, commands
                                            Function tfunc = new Function(tname, tparams, ttype, Convert.ToBoolean(tIsVirtual), Convert.ToBoolean(tIsAbstract), Convert.ToBoolean(tIsStatic), new List<Variable>(), new List<string>());
    										tclass.functions.Add(tfunc);
    										break;
    										
						default:		    log(tdefinition);
											break;
        			}
        		}
        		
        		//Add Method implementation        		
        		for(int j=0; j < timplementation.Count; j++)
        		{
        			int k = FindNextFunctionImplementation(ref timplementation, j);
                    List<string> tfunctiontext = GetStringSubList(ref timplementation, j, k);

                    List<string> tdefinition_parts = RecognizeClassImplementation(tfunctiontext[0]);

                    if (tdefinition_parts[0] == "Function")
                    {
                        string tname = tdefinition_parts[1];
                        string ttype = tdefinition_parts[2];

                        List<Variable> tvars = new List<Variable>();
                        List<Variable> tparams = new List<Variable>();
                        List<string> tcommands = new List<string>();

                        int ii = 3;
                        for (; (tdefinition_parts[ii] != "Variables") && (ii < tdefinition_parts.Count); )
                        {
                            tvar = new Variable(tdefinition_parts[ii], tdefinition_parts[ii + 1]);
                            tparams.Add(tvar);
                            ii += 2;
                        }

                        for (; (tdefinition_parts[ii] != "Commands") && (ii < tdefinition_parts.Count); )
                        {
                            tvar = new Variable(tdefinition_parts[ii], tdefinition_parts[ii + 1]);
                            tvars.Add(tvar);
                            ii += 2;
                        }

                        tcommands = GenerateCommands(ref tdefinition_parts, ii);

                        //Search for the right function in declarations

                        //Add the variables and commands
                    }
                    else
                    {
                        log(tdefinition);
                        throw new Exception("Bad Function implementation found");
                    }
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
            return tout;
        }

        private Type StringToType(string istring)
        {
            Type tout;
            return tout;
        }

        private void GenerateGlobalClass(ref Class oclassGlobals, ref List<string> iconstsGlobal, ref List<string> ienumsGlobal, ref List<string> itypesGlobal, ref List<string> ivarsGlobal)
        {
            List<Constant> tconstants = new List<Constant>();
            List<Variable> tvars = new List<Variable>();
            List<Enum> tenums = new List<Enum>();
            List<Type> ttypes = new List<Type>();
            
            for (int i=0; i<iconstsGlobal.Count;i++)
            {
                tconstants.Add(StringToConstant(iconstsGlobal[i]));
            }

            for (int i=0; i<ienumsGlobal.Count;i++)
            {
                tenums.Add(StringToEnum(ienumsGlobal[i]));
            }

            for (int i=0; i<itypesGlobal.Count;i++)
            {
                ttypes.Add(StringToType(itypesGlobal[i]));
            }

            for (int i=0; i<ivarsGlobal.Count;i++)
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
        	{
                Utilities.Beautify(istrings[i]);
        	}
        }
        
        //oclassdefinitions is a list of class variables, properties, function and procedure definitions 
        private void ParseInterface(ref List<string> istrings, ref List<string> ouses, ref List<string> oclassnames, ref List<DelphiClassDef> oclassdefinitions, ref List<string> oconsts, ref List<string> oenums, ref List<string> otypes)
        {                        
        	int tcurr_string_count = startInterface, tnext_subsection_pos = -1;

            //Interface sub sections
            while (tcurr_string_count != -1)
            {
            	tnext_subsection_pos = FindNextInterfaceSubSection(ref istrings, tcurr_string_count);

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
                tcurr_string_count = tnext_subsection_pos + 1;
            }        
        }

        //Check if a line in class definition is a variable, if it has only two words, divided by ' ' or ':' and if its first character is 'f' or 'F'
        private bool CheckVariable(string istring)
        {
            string[] tarray = istring.Split(' ');

            if ((tarray.GetLength(0) == 2) && ((tarray[0][0] == 'f') || (tarray[0][0] == 'F')) && (istring.Split(':').GetLength(0) == 2))
                return true;
            return false;
        }

        private DelphiClassDef ParseClassMethods(ref List<string> istrings, string iclassname, int icurr_string_count, int inext_subsection_pos)
        {
            DelphiClassDef tout = new DelphiClassDef();
            List<string> tstrings = new List<string>(), tmethods = new List<string>(), tvars = new List<string>(), tproperties = new List<string>();

            tstrings = istrings; // GetStringSubList(ref istrings, icurr_string_count, inext_subsection_pos);

            //Variable filter. Variables are removed so that functions can be read without confusion
            int i = icurr_string_count;

            while (i < inext_subsection_pos)
            {
                if (CheckVariable(tstrings[i]))
                {
                    tvars.Add(tstrings[i]);
                    tstrings.RemoveAt(i);
                }
                else
                    i++;
            }

            for (i = icurr_string_count; i < inext_subsection_pos; i++)
            {
                int tnext_subsection_pos = FindNextKey(ref tstrings, ref implementKindKeys, icurr_string_count);

                if (tnext_subsection_pos == -1)
                    tnext_subsection_pos = endImplementation;

                switch (RecognizeKey(tstrings[icurr_string_count], ref classKeys))
                { 
                    //{ "public", "private", "const", "constructor", "class function", "class procedure", "procedure", "function", "property"};
                    case "public":  //ignore
                                    break;
                    
                    case "private": //ignore
                                    break;

                    case "const":   break;

                    case "constructor": //Break down function elements
                                    string tclassname;
                                    string tmethodtype = "constructor";

                                    bool toverload = false, tvirtual = false, tabstract = false;
                                    int tnext_pos = FindNextKey(ref tstrings, ref classKeys, icurr_string_count);
                                    string tfuncstring = "";

                                    //Add all the indented lines in function title
                                    for (i = icurr_string_count; i < tnext_pos; i++)
                                    {
                                        tfuncstring = tfuncstring + istrings[i].Trim();
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

                                    string[] tclassnamearray = tfuncstring.Split('.');
                                    tclassname = tclassnamearray[0].Split(' ')[1];

                                    //Break down function elements
                                    string treturntype, tparameters, tmethodname;
                                    string[] treturntypearray = tclassnamearray[1].Split(')');

                                    iclassname = tclassnamearray[0].Split(' ')[1];
                        
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

                                    string tfunctionstring = "func_" + iclassname + "_" + tmethodname + "_" + itype + "_" + tparameters + "_" + treturntype;
                                    tmethods.Add(tfunctionstring);
                                    tmethods.Add(toverload + "_" + tvirtual + "_" + tabstract);

                                    break;

                    case "class function": //Break down function elements
                                    tclassname = "";
                                    tmethodtype = "class_function";

                                    toverload = false;
                                    tvirtual = false;
                                    tabstract = false;
                                    tnext_pos = FindNextKey(ref tstrings, ref classKeys, icurr_string_count);
                                    tfuncstring = "";

                                    //Add all the indented lines in function title
                                    for (i = icurr_string_count; i < tnext_pos; i++)
                                    {
                                        tfuncstring = tfuncstring + istrings[i].Trim();
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

                                    tclassnamearray = tfuncstring.Split('.');
                                    tclassname = tclassnamearray[0].Split(' ')[1];


                                    //Break down function elements
                                    treturntypearray = tclassnamearray[1].Split(')');

                                    iclassname = tclassnamearray[0].Split(' ')[1];
                        
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

                                    tfunctionstring = "func_" + iclassname + "_" + tmethodname + "_" + itype + "_" + tparameters + "_" + treturntype;
                                    tmethods.Add(tfunctionstring);
                                    tmethods.Add(toverload + "_" + tvirtual + "_" + tabstract);

                                    break;

                    case "class procedure":  //Break down function elements
                                    tclassname = "";
                                    tmethodtype = "class_procedure";

                                    toverload = false;
                                    tvirtual = false;
                                    tabstract = false;
                                    tnext_pos = FindNextKey(ref tstrings, ref classKeys, icurr_string_count);
                                    tfuncstring = "";

                                    //Add all the indented lines in function title
                                    for (i = icurr_string_count; i < tnext_pos; i++)
                                    {
                                        tfuncstring = tfuncstring + istrings[i].Trim();
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

                                    tclassnamearray = tfuncstring.Split('.');
                                    tclassname = tclassnamearray[0].Split(' ')[1];


                                    //Break down function elements
                                    treturntypearray = tclassnamearray[1].Split(')');

                                    iclassname = tclassnamearray[0].Split(' ')[1];

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

                                    tfunctionstring = "func_" + iclassname + "_" + tmethodname + "_" + itype + "_" + tparameters + "_" + treturntype;
                                    tmethods.Add(tfunctionstring);
                                    tmethods.Add(toverload + "_" + tvirtual + "_" + tabstract);

                                    break;

                    case "procedure":  //Break down function elements
                                    tclassname = "";
                                    tmethodtype = "procedure";

                                    toverload = false;
                                    tvirtual = false;
                                    tabstract = false;
                                    tnext_pos = FindNextKey(ref tstrings, ref classKeys, icurr_string_count);
                                    tfuncstring = "";

                                    //Add all the indented lines in function title
                                    for (i = icurr_string_count; i < tnext_pos; i++)
                                    {
                                        tfuncstring = tfuncstring + istrings[i].Trim();
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

                                    tclassnamearray = tfuncstring.Split('.');
                                    tclassname = tclassnamearray[0].Split(' ')[1];


                                    //Break down function elements
                                    treturntypearray = tclassnamearray[1].Split(')');

                                    iclassname = tclassnamearray[0].Split(' ')[1];

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

                                    tfunctionstring = "func_" + iclassname + "_" + tmethodname + "_" + itype + "_" + tparameters + "_" + treturntype;
                                    tmethods.Add(tfunctionstring);
                                    tmethods.Add(toverload + "_" + tvirtual + "_" + tabstract);

                                    break;

                    case "function":  //Break down function elements
                                    tclassname = "";
                                    tmethodtype = "function";

                                    toverload = false;
                                    tvirtual = false;
                                    tabstract = false;
                                    tnext_pos = FindNextKey(ref tstrings, ref classKeys, icurr_string_count);
                                    tfuncstring = "";

                                    //Add all the indented lines in function title
                                    for (i = icurr_string_count; i < tnext_pos; i++)
                                    {
                                        tfuncstring = tfuncstring + istrings[i].Trim();
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

                                    tclassnamearray = tfuncstring.Split('.');
                                    tclassname = tclassnamearray[0].Split(' ')[1];


                                    //Break down function elements
                                    treturntypearray = tclassnamearray[1].Split(')');

                                    iclassname = tclassnamearray[0].Split(' ')[1];

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

                                    tfunctionstring = "func_" + iclassname + "_" + tmethodname + "_" + itype + "_" + tparameters + "_" + treturntype;
                                    tmethods.Add(tfunctionstring);
                                    tmethods.Add(toverload + "_" + tvirtual + "_" + tabstract);

                                    break;

                    case "property": tproperties.Add(istrings[icurr_string_count]);
                                    break;
                    
                    default: break;
                }
            }

            tout.methods = tmethods;
            tout.variables = tvars;
            tout.properties = tproperties;

            return tout;
        }

        private void ParseInterfaceTypes(ref List<string> istrings, ref List<string> ouses, ref List<string> oclassnames, ref List<DelphiClassDef> oclassdefinitions, ref List<string> oconsts, ref List<string> oenums, ref List<string> otypes, int istartpos)
        {
            int tnext_subsection_pos = -1, tcurr_string_count = istartpos;
            string tclassname = "";

            //Interface sub sectionsm
            while (tcurr_string_count < endInterface)
            {
                //Ignore comments and empty lines
                if (!(RecognizeComment(istrings[tcurr_string_count])) && !(RecognizeEmptyLine(istrings[tcurr_string_count])))
                {
                    switch (RecognizeKey(istrings[tcurr_string_count], ref subsectionKeys))
                    {
                        case "class":   tnext_subsection_pos = FindNextSymbol(ref istrings, "end;", tcurr_string_count);
                                        tclassname = ((Regex.Replace(istrings[tcurr_string_count], @"\s+", "")).Split('='))[0];
                                        oclassnames.Add(tclassname);

                                        if (tnext_subsection_pos == -1)
                                            throw new Exception("Incomplete class definition");

                                        DelphiClassDef tclassdef = new DelphiClassDef();
                                        tclassdef = ParseClassMethods(ref istrings, tclassname, tcurr_string_count, tnext_subsection_pos);

                                        oclassdefinitions.Add(tclassdef);
                                        tcurr_string_count = tnext_subsection_pos + 1;
                                        break;

                        case "record":  tnext_subsection_pos = FindNextSymbol(ref istrings, "end;", tcurr_string_count);
                                        tclassname = ((Regex.Replace(istrings[tcurr_string_count], @"\s+", "")).Split('='))[0];
                                        oclassnames.Add(tclassname);

                                        if (tnext_subsection_pos == -1)
                                            throw new Exception("Incomplete class definition");

                                        tclassdef = new DelphiClassDef();
                                        tclassdef = ParseClassMethods(ref istrings, tclassname, tcurr_string_count, tnext_subsection_pos);

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

                                            throw new Exception("Unknown definition found");
                                        }
                                        break;
                    }
                    tcurr_string_count = FindNextInterfaceSubSection(ref istrings, tcurr_string_count);
                }
                else
                {
                    tcurr_string_count++;
                }
            }
        }
        
        private void ParseVar(ref List<string> istrings, ref List<string> ovars)
        {
        	if (startVar != -1)
        		for (int i = startVar + 1; i < startImplementation; i++)
        			ovars.Add(istrings[i]);
        }

        //oclassimplementations is a list of class functions and procedures
        private void ParseImplementation(ref List<string> istrings, ref List<string> ouses, ref List<string> oclassnames, ref List<List<string>> oclassimplementations, ref List<string> oconsts, ref List<string> oenum, ref List<string> oalias )
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
                                        string tclassname, tmethodtype = "class_function";

                                        int tnext_pos = FindNextSymbol(ref istrings, "begin", tcurr_string_count);
                                        string tfuncstring = "";

                                        //Add all the indented lines in function title
                                        for (int i = tcurr_string_count; i < tnext_pos; i++)
                                        {
                                            tfuncstring = tfuncstring + istrings[i].Trim();
                                        }

                                        string[] tclassnamearray = tfuncstring.Split('.');
                                        tclassname = tclassnamearray[0].Split(' ')[1];

                                        //oclassimplementations looks like: List<List<string>>[i] = classes, List<List<string>>[i][0] = class name, List<List<string>>[i][j + 0] = method string, 
                                        //List<List<string>>[i][j + 1] = method keywords (virtual, abstract, overload), List<List<string>>[i][j+1+n] = method commands 
                                        for (int i = 0; i < oclassimplementations.Count; i++)
                                        {
                                            //Find the class
                                            if (oclassimplementations[i][0] == tclassname)
                                            {
                                                oclassimplementations[i].AddRange(ParseMethod(ref istrings, ref tclassnamearray, tclassname, tmethodtype, tcurr_string_count, tnext_subsection_pos));
                                                break;
                                            }
                                        }
                                        break;

                    case "class procedure": //Break down function elements
                                        tmethodtype = "class_procedure";

                                        tnext_pos = FindNextSymbol(ref istrings, "begin", tcurr_string_count);
                                        tfuncstring = "";

                                        //Add all the indented lines in function title
                                        for (int i = tcurr_string_count; i < tnext_pos; i++)
                                        {
                                            tfuncstring = tfuncstring + istrings[i].Trim();
                                        }

                                        tclassnamearray = tfuncstring.Split('.');
                                        tclassname = tclassnamearray[0].Split(' ')[1];

                                        //oclassimplementations looks like: List<List<string>>[i] = classes, List<List<string>>[i][0] = class name, List<List<string>>[i][j + 0] = method string, 
                                        //List<List<string>>[i][j + 1] = method keywords (virtual, abstract, overload), List<List<string>>[i][j+1+n] = method commands 
                                        for (int i = 0; i < oclassimplementations.Count; i++)
                                        {
                                            //Find the class
                                            if (oclassimplementations[i][0] == tclassname)
                                            {
                                                oclassimplementations[i].AddRange(ParseMethod(ref istrings, ref tclassnamearray, tclassname, tmethodtype, tcurr_string_count, tnext_subsection_pos));
                                                break;
                                            }
                                        }
                                        break;

                    case "function":    //Break down function elements
                                        tmethodtype = "function";
                        
                                        tnext_pos = FindNextSymbol(ref istrings, "begin", tcurr_string_count);
                                        tfuncstring = "";

                                        //Add all the indented lines in function title
                                        for (int i = tcurr_string_count; i < tnext_pos; i++)
                                        {
                                            tfuncstring = tfuncstring + istrings[i].Trim();
                                        }

                                        tclassnamearray = tfuncstring.Split('.');
                                        tclassname = tclassnamearray[0].Split(' ')[1];

                                        //oclassimplementations looks like: List<List<string>>[i] = classes, List<List<string>>[i][0] = class name, List<List<string>>[i][j + 0] = method string, 
                                        //List<List<string>>[i][j + 1] = method keywords (virtual, abstract, overload), List<List<string>>[i][j+1+n] = method commands 
                                        for (int i = 0; i < oclassimplementations.Count; i++)
                                        {
                                            //Find the class
                                            if (oclassimplementations[i][0] == tclassname)
                                            {
                                                oclassimplementations[i].AddRange(ParseMethod(ref istrings, ref tclassnamearray, tclassname, tmethodtype, tcurr_string_count, tnext_subsection_pos));
                                                break;
                                            }
                                        }
                                        break;

                    case "procedure":   //Break down function elements
                                        tmethodtype = "procedure";
                                        
                                        tnext_pos = FindNextSymbol(ref istrings, "begin", tcurr_string_count);
                                        tfuncstring = "";

                                        //Add all the indented lines in function title
                                        for (int i = tcurr_string_count; i < tnext_pos; i++)
                                        {
                                            tfuncstring = tfuncstring + istrings[i].Trim();
                                        }

                                        tclassnamearray = tfuncstring.Split('.');
                                        tclassname = tclassnamearray[0].Split(' ')[1];

                                        //oclassimplementations looks like: List<List<string>>[i] = classes, List<List<string>>[i][0] = class name, List<List<string>>[i][j + 0] = method string, 
                                        //List<List<string>>[i][j + 1] = method keywords (virtual, abstract, overload), List<List<string>>[i][j+1+n] = method commands 
                                        for (int i = 0; i < oclassimplementations.Count; i++)
                                        {
                                            //Find the class
                                            if (oclassimplementations[i][0] == tclassname)
                                            {
                                                oclassimplementations[i].AddRange(ParseMethod(ref istrings, ref tclassnamearray, tclassname, tmethodtype, tcurr_string_count, tnext_subsection_pos));
                                                break;
                                            }
                                        }
                                        break;

                    case "constructor": //Break down function elements
                                        tmethodtype = "constructor";
                                        
                                        tnext_pos = FindNextSymbol(ref istrings, "begin", tcurr_string_count);
                                        tfuncstring = "";

                                        //Add all the indented lines in function title
                                        for (int i = tcurr_string_count; i < tnext_pos; i++)
                                        {
                                            tfuncstring = tfuncstring + istrings[i].Trim();
                                        }

                                        tclassnamearray = tfuncstring.Split('.');
                                        tclassname = tclassnamearray[0].Split(' ')[1];

                                        //oclassimplementations looks like: List<List<string>>[i] = classes, List<List<string>>[i][0] = class name, List<List<string>>[i][j + 0] = method string, 
                                        //List<List<string>>[i][j + 1] = method keywords (virtual, abstract, overload), List<List<string>>[i][j+1+n] = method commands 
                                        for (int i = 0; i < oclassimplementations.Count; i++)
                                        {
                                            //Find the class
                                            if (oclassimplementations[i][0] == tclassname)
                                            {
                                                oclassimplementations[i].AddRange(ParseMethod(ref istrings, ref tclassnamearray, tclassname, tmethodtype, tcurr_string_count, tnext_subsection_pos));
                                                break;
                                            }
                                        }
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
        
        private List<string> ParseMethod(ref List<string> istrings, ref string[] iclassnamearray, string iclassname, string itype, int ipos, int inextpos)
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

            string tfunctionstring = "func_" + iclassname + "_" + tmethodname + "_" + itype + "_" + tparameters + "_" + treturntype;
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
            
            startInterface = FindStringInList("interface", ref istrings, 0, true);
        	if (startInterface == -1)
	    		throw new Exception("Interface not found");

        	Section_Names.Add("Interface");
    		Section_Bookmarks.Add(startInterface);

    		//If there is no other section, process
        	endInterface = FindNextSection(ref istrings, startInterface);
    		if (endInterface == -1)
        		return;
    		
            startImplementation = FindStringInList("implementation", ref istrings, endInterface, true);
    		if (startImplementation != -1)
    		{
    			//Look for a Var section in between interface and implementation
    			if (endInterface != startImplementation)
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
	            startVar = FindStringInList("var", ref istrings, endInterface, true);
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

        static private int GoToNextKeyword(string ifind, ref List<string> iarray, int iindex, bool imatchCase)
		{
            return 0;
		}

        static private int FindNextFunctionImplementation(ref List<string> iarray, int istartpoint)
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
                if (CheckStringListElementsInString(istrings[i], ref tEnd) != -1)
                    return i;
            }
            return -1;
        }

        static private int FindNextKey(ref List<string> istrings, ref string[] ikeys, int istartpos)
        {
            for (int i = istartpos; i < istrings.Count; i++)
            {
                if (CheckStringListElementsInString(istrings[i], ref ikeys) != -1)
                    return i;
            }
            return -1;
        }
        
        static private int FindNextSection(ref List<string> istrings, int istartpos)
        {
            for (int i = istartpos; i < istrings.Count; i++)
            {
                if (CheckStringListElementsInString(istrings[i], ref sectionKeys) != -1)
                    return i;
            }
            return -1;
        }

        static private int FindNextInterfaceSubSection(ref List<string> istrings, int istartpos)
        {
        	for (int i = istartpos; i < istrings.Count; i++)
        	{
        		if (CheckStringListElementsInString(istrings[i], ref subsectionKeys) != -1)
        			return i;
        	}
        	return -1;
        }

        static private string RecognizeKey(string istring, ref string[] ikeys)
		{
           	for (int i=0; i < ikeys.GetLength(0); i++)
           	{
           		if (istring.IndexOf(ikeys[i]) != -1)
           			return ikeys[i];
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

        static private List<string> RecognizeClassDefinition(string iline)
        {
            return new List<string>();
        }
        
        static private int CheckStringListElementsInString(string istring, ref string[] ielements)
		{
        	for (int i=0; i < ielements.GetLength(0); i++)
           	{
           		if (istring.IndexOf(ielements[i]) != -1)
           			return i;
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
	}    
}
