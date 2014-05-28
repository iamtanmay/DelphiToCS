using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DelphiToCSTranslator
{
    public class DelphiParser
    {
    	//Logical data
        public string Unit_Name;
        public Script script;
        public List<Constant> Objects_ConstLocal, Object_ConstGlobal;
    	public List<Enum> Objects_ConstLocal, Object_ConstGlobal;
    	public List<Alias> Objects_ConstLocal, Object_ConstGlobal;
    	//Classes to store enums, consts, alias for global and local use
    	public Class Objects_ConstClassLocal, Objects_ConstClassGlobal;
    	public List<Include> Objects_UsesInterface, Objects_UsesImplementation, Objects_UsesCombined;
    	public List<Class> Objects_Classes;

        
        //Indexing
    	//Bookmarks for sections
    	private int HeaderStart, HeaderEnd, InterfaceStart, InterfaceEnd, VarStart, VarEnd, ImplementationStart, ImplementationEnd, InterfaceUsesStart, InterfaceUsesEnd;    	
    	//Bookmarks for subsections
    	private List<int> UsesStarts, UsesEnds, ClassInterfaceStarts, ClassInterfaceEnds, ClassImplementStarts, ClassImplementEnds, ConstStarts, ConstEnds, EnumGlobalStarts, EnumEnds, AliasStarts, AliasEnds, SubSection_Bookmarks;
    	
    	
    	//String data
    	//Text for header, list of class names, list of SubSection names as they occur in file, Uses in interface, implementation, text for consts, Enums, Alias and Vars (Global and Local)
    	private List<string> Header, Class_Names, SubSection_Names, Uses_Interface, Uses_Implementation, ConstsGlobal, EnumsGlobal, AliasGlobal, VarsGlobal, ConstsLocal, EnumsLocal, AliasLocal, VarsLocal;
    	//Raw text for the classes
    	private List<List<string>> Class_Definitions, Class_Implementations;

    	
    	//Keywords
        //Broadly dividing the script into sections
    	public string[] Section_Keywords = { "var", "implementation", "interface"};
    	//Dividing each section into subsections
    	public string[] SubSection_Keywords = { "type", "const", "uses", "class", "record", "procedure", "function", "class function", "class procedure"};
    	//Divide methods into commands
    	public string[] Method_Keywords = { "var", "begin", "label", "end", "try", "catch", "finally"};
    	
    	
    	//ireadheader is a flag if header is to be read. iheaderstart is normally "{ --" in Zinsser Delphi units, and iheaderend is "-- }" 
        public void ExtractStructure(ref List<string> istrings, bool ireadheader, string iheaderstart, string iheaderend)
        {
        	//Initialise
            script = new Script();
            
            Unit_Name = "";
            Header = new List<string>();
            
            Class_Names = new List<string>();
            Class_Definitions = new List<List<string>>();
            Class_Implementations = new List<List<string>>();
            
            ConstsGlobal = new List<string>();
            EnumsGlobal = new List<string>();
            AliasGlobal = new List<string>();
            VarsGlobal = new List<string>();
            
            ConstsLocal = new List<string>();
            EnumsLocal = new List<string>();
            AliasLocal = new List<string>();
            VarsLocal = new List<string>();
            
            SubSection_Names = new List<string>();
            Section_Bookmarks = new List<int>();
             	
            ClassInterfaceStarts = new List<int>();
            ClassInterfaceEnds = new List<int>(); 
            ClassImplementStarts = new List<int>(); 
            ClassImplementEnds = new List<int>(); 
            
            UsesStarts = new List<int>();
            UsesEnds = new List<int>();
            
            ConstStarts = new List<int>();
            ConstEnds = new List<int>(); 
            
            EnumLocalStarts = new List<int>();
            EnumLocalEnds = new List<int>(); 
            
            EnumGlobalStarts = new List<int>();
            EnumGlobalEnds = new List<int>(); 
            
            AliasStarts = new List<int>();
            AliasEnds = new List<int>();

            HeaderStart =  HeaderEnd =  InterfaceStart =  InterfaceEnd =  ImplementationStart =  ImplementationEnd =  InterfaceUsesStart =  InterfaceUsesEnd = -1;
            
            //Read unit name
            Unit_Name = ((istrings[FindStringInList("unit", ref istrings, 0)].Replace(' ', ';')).Split(';'))[1];

            //Bookmark sections
           Indexing:
            IndexStructure();

            //Break up the text into Lists of strings for different parts
           Processing:
            if (ireadheader)
    			ParseHeader(ref istrings, ref Header, ireadheader, iheaderstart, iheaderend);

			//Convert comments
			ParseComments(ref istrings);
            ParseInterface(ref istrings, ref Uses_Interface, ref Class_Names, ref Class_Definitions, ref ConstsGlobal, ref EnumsGlobal, ref AliasGlobal);
            ParseVar(ref istrings, ref VarsGlobal);
            ParseImplementation(ref istrings, ref Uses_Implementation, ref Class_Names, ref Class_Implementations, ref ConstsLocal, ref EnumsLocal, ref AliasLocal);
            
            //Generate the text pieces into class objects
           Generation:
            GenerateClasses(ref Objects_Classes, ref Class_Names, ref Class_Definitions, ref Class_Implementations);
            GenerateConst(ref Objects_ConstClassGlobal, ref ConstsGlobal, ref EnumsGlobal, ref AliasGlobal, ref VarsGlobal);
            GenerateConst(ref Objects_ConstClassLocal, ref ConstsLocal, ref EnumsLocal, ref AliasLocal, new List<string>());
            GenerateIncludes(ref Objects_UsesInterface, ref Uses_Interface);
            GenerateIncludes(ref Objects_UsesImplementation, ref Uses_Implementation);
            
            //Add both uses into one
            Objects_UsesCombined.AddRange(Objects_UsesInterface);
            Objects_UsesCombined.AddRange(Objects_UsesImplementation);
            
            GenerateScript(ref Script, ref Usref Class Objects_Classes, ref Objects_ConstClassGlobal, ref Objects_ConstClassLocal);
        }
                
        GenerateClasses(ref List<Class> oclasses, ref List<string> inames, ref List<List<string>> idefinitions, ref List<List<string>> iimplementations)
        {
        	for (int i=0; i< inames.Count(); i++)
        	{
        		Class tclass;
        		List<string> tdefinition = idefinitions[i];
        		List<string> timplementation = iimplementations[i];
        		tclass.name = inames[i];

        		//Add Variables, Properties and method names from definitions
        		for(int j=0; j < tdefinition.Count(); j++)
        		{
        			//Contains all the parameters for each 
        			List<string> tdefintion_parts = RecognizeClassDefinition(tdefinition[i]);
        			switch(tdefintion_parts[0])
        			{
    					case "Variable":	//name, type
        									Variable tvar = new Variable(tdefinition_parts[1], tdefinition_parts[2]);
    										tclass.variables.Add(tvar);
    										break;
    										
						case "Property":	//name, type, read, write
    										Property tprop = new Property(tdefinition_parts[1], tdefinition_parts[2], tdefinition_parts[3], tdefinition_parts[4]);
    										tclass.properties.Add(tprop);
    										break;
    					
						//This is for both functions and procedures. Difference is that type is returned "" for procedures    										
						case "Function":	List<Variable> tvars = new List<Variable>();
    										for (int ii=5; ii< tdefintion_parts.Count; )
    										{
    											Variable tvar = new Variable(tdefinition_parts[ii], tdefinition_parts[ii+1]);
    											tvars.Add(tvar);
												ii += 2;    											
    										}
    										//name, type, IsVirtual, IsAbstract, IsStatic, Parameters
    										Function tfunc = new Function(tdefinition_parts[1], tdefinition_parts[2], ToBool(tdefinition_parts[3]), ToBool(tdefinition_parts[4]), ToBool(tdefinition_parts[5]), tvars);
    										tclass.functions.Add(tfunc);
    										break;
    										
						case default:		Log(tdefinition[j]);
											break;
        			}
        		}
        		
        		//Add Method implementation data from implementations        		
        		for(int j=0; j < timplementation.Count(); j++)
        		{
        			j = FindNextFunctionImplementation(ref timplementation, j);
        			List<string> tfunctiontext = GetFunctionText(ref timplementation, j);
        			Function tfunction = FunctionFromText(ref tfunctiontext);
        			tclass.functions.
        			
        			
        			switch (RecognizeInterfaceSubSection(istrings[tcurr_string_count]))
	            	{
	        			case "Class": 	oclassdefinitions.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
	        			case "Const": 	oconst.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
	        			case "Enum": 	oenums.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
	        			case "Alias": 	oalias.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
						default: 		break;
	            	}
        			
        			//Contains all the parameters for each 
        			List<string> timplementation_parts = RecognizeClassDefinition(timplementation[i]);
        			
        			//Check for Var
        			//Check for Begin
        			//
        			switch(tdefintion_parts[0])
        			{
    					case "Variable":	//name, type
        									Variable tvar = new Variable(tdefinition_parts[1], tdefinition_parts[2]);
    										tclass.variables.Add(tvar);
    										break;
    										
						case "Property":	//name, type, read, write
    										Property tprop = new Property(tdefinition_parts[1], tdefinition_parts[2], tdefinition_parts[3], tdefinition_parts[4]);
    										tclass.properties.Add(tprop);
    										break;
    					
						//This is for both functions and procedures. Difference is that type is returned "" for procedures    										
						case "Function":	//Get list of variables
    										List<Variable> tvars = new List<Variable>();
    										for (int ii=5; ii< tdefintion_parts.Count; )
    										{
    											Variable tvar = new Variable(tdefinition_parts[ii], tdefinition_parts[ii+1]);
    											tvars.Add(tvar);
												ii += 2;    											
    										}
    										//name, type, IsVirtual, IsAbstract, IsStatic, Parameters
    										Function tfunc = new Function(tdefinition_parts[1], tdefinition_parts[2], ToBool(tdefinition_parts[3]), ToBool(tdefinition_parts[4]), ToBool(tdefinition_parts[5]), tvars);
    										tclass.functions.Add(tfunc);
    										break;
    										
						case default:		Log(tdefinition[j]);
											break;
        			}
        		}
        	}
        }
        
        GenerateConst(ref Objects_ConstClassGlobal, ref ConstsGlobal, ref EnumsGlobal, ref AliasGlobal, ref VarsGlobal)
        {
        }
        
        GenerateIncludes(ref Objects_UsesInterface, ref Uses_Interface)
        {        
        }
        
        private void ParseHeader(ref List<string> istrings, ref List<string> oheader, bool ireadheader, string iheaderstart, string iheaderend)
        {
			 //Read Header
            if (ireadheader)
            	if (HeaderStart != -1)
	            	if (HeaderEnd != -1)
	            		for (int i = HeaderStart; i < HeaderEnd; i++)
	            			oheader.Add(istrings[i]);
	            	else
		            {
		            	//Throw exception -> header end not found
		            }       			
        }
    	
        private void ParseComments(ref List<string> istrings)
        {
        	for (int i = 0; i < istrings.Count(); i++)
        	{
        		istrings[i].Replace("{", "/*");
        		istrings[i].Replace("}", "*/");
        	}
        }
        
        //oclassdefinitions is a list of class variables, properties, function and procedure definitions 
        private void ParseInterface(ref List<string> istrings, ref List<string> ouses, ref List<string> oclassnames, ref List<List<string>> oclassdefinitions, ref List<string> oconst, ref List<string> oenums, ref List<string> oalias )
        {                        
        	int tcurr_string_count = InterfaceStart;
        	int tnext_subsection_pos = -1;
        	
            //uses
            tcurr_string_count = FindStringInList("uses", ref istrings, tcurr_string_count);
            if (tcurr_string_count != -1)
            {
	            tnext_subsection_pos = FindNextSubSection(ref istrings, tcurr_string_count);
	            
	            if (tnext_subsection_pos == -1)
	            	tnext_subsection_pos = InterfaceEnd;
	            
	            for (tcurr_string_count; tcurr_string_count < tnext_subsection_pos; tcurr_string_count++)
	            {
	            	ouses.Add(istrings[tcurr_string_count]);
	            }
            }
            
            //The rest
            while (tcurr_string_count < InterfaceEnd)
            {
            	tcurr_string_count = FindNextSubSection(ref istrings, tcurr_string_count);
            	tnext_subsection_pos = FindNextSubSection(ref istrings, tcurr_string_count);
            	switch (RecognizeInterfaceSubSection(istrings[tcurr_string_count]))
            	{
        			case "Class": 	oclassdefinitions.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
        			case "Const": 	oconst.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
        			case "Enum": 	oenums.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
        			case "Alias": 	oalias.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
					default: 		break;
            	}
            }
        }
        
        private void ParseVar(ref List<string> istrings, ref List<string> ovars)
        {
        	if (VarStart != -1)
        		for (int i = VarStart + 1; i < ImplementationStart; i++)
        			ovars.Add(istrings[i]);
        }

        //oclassimplementations is a list of class functions and procedures
        private void ParseImplementation(ref List<string> istrings, ref List<string> ouses, ref List<string> oclassnames, ref List<List<string>> oclassimplementations, ref List<List<string>> oconst, ref List<List<string>> oenum, ref List<List<string>> oalias )
        {
        	int tcurr_string_count = ImplementationStart;
        	int tnext_subsection_pos = -1;
        	
            //uses
            tcurr_string_count = FindStringInList("uses", ref istrings, tcurr_string_count);
            if (tcurr_string_count != -1)
            {
	            tnext_subsection_pos = FindNextSubSection(ref istrings, tcurr_string_count);
	            
	            if (tnext_subsection_pos == -1)
	            	tnext_subsection_pos = ImplementationEnd;
	            
	            for (tcurr_string_count; tcurr_string_count < tnext_subsection_pos; tcurr_string_count++)
	            {
	            	ouses.Add(istrings[tcurr_string_count]);
	            }
            }
            
            //The rest
            while (tcurr_string_count < InterfaceEnd)
            {
            	tcurr_string_count = FindNextSubSection(ref istrings, tcurr_string_count);
            	tnext_subsection_pos = FindNextSubSection(ref istrings, tcurr_string_count);
            	switch (RecognizeInterfaceSubSection(istrings[tcurr_string_count]))
            	{
        			case "Function": 	oclassdefinitions.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
        			case "Procedure": 	oclassdefinitions.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
        			case "Const": 		oconst.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
        			case "Enum": 		oenum.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
        			case "Alias": 		oalias.Add(GetStringSubList(ref istrings, tcurr_string_count, tnext_subsection_pos)); break;
					default: 			break;
            	}
            }
        }
  
    	private void IndexStructure()
        {
            if (ireadheader)
            {
            	HeaderStart = FindStringInList(iheaderstart, ref istrings, 0, true);
            	if (HeaderStart != -1)
            	{
            		HeaderEnd = FindStringInList(iheaderend, ref istrings, HeaderStart, true);
            		if (HeaderEnd == -1)
		        		throw new Exception("Header end not found");

		        	Section_Names.Add("Header");
	        		Section_Bookmarks.Add(HeaderStart);
        		}
            }
            
            InterfaceStart = FindStringInList("interface", ref istrings, 0, true);
        	if (InterfaceStart == -1)
	    		throw new Exception("Interface not found");

        	Section_Names.Add("Interface");
    		Section_Bookmarks.Add(InterfaceStart);

    		//If there is no other section, process
        	InterfaceEnd = FindNextSection(InterfaceStart);
    		if (InterfaceEnd == -1)
        		goto Processing;
    		
            ImplementationStart = FindStringInList("implementation", ref istrings, InterfaceEnd, true);
    		if (ImplementationStart != -1)
    		{
    			//Look for a Var section in between interface and implementation
    			if (InterfaceEnd != ImplementationStart)
    			{
    				VarStart = InterfaceEnd;
    				VarEnd = ImplementationStart;
    				
					Section_Names.Add("Var");
					Section_Bookmarks.Add(VarStart);
    			}

    			Section_Names.Add("Implementation");
	    		Section_Bookmarks.Add(ImplementationStart);

	    		//If there is no other section after "implementation", then process
	    		ImplementationEnd = FindNextSection(ImplementationStart);
	    		if (ImplementationEnd == -1)
	    		{
	        		goto Processing;
	    		}
    		}
    		else{
	            VarStart = FindStringInList("var", ref istrings, 0, true);
	            if (VarStart != -1)
	            {
					Section_Names.Add("Var");
					Section_Bookmarks.Add(VarStart);
	            }
    		}
        }
        
        static public List<Class> StringsToClass(string[] istrings)
        {
        }      

        static public int GoToNextKeyword( string ifind, ref List<string> iarray, int iindex, bool imatchCase)
		{
			
		}

	    static public int FindStringInList( string ifind, ref List<string> iarray, int iindex, bool imatchCase)
		{
	    	for (int i = tindex; i < tarray.GetLength(0); i++)
	    	{
	    		string tstring;
	    		
	    		if (!imatchcase)
	    			tstring = istrings[i].ToLower();
	    		else
	    			tstring = istrings[i];
	    		
	    		if (tstring.IndexOf(ifind))
	    			return i;
	    	}
	    	return -1;
		}
  c    }    
 }
