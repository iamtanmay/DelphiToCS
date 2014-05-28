using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DelphiToCSTranslator
{
	//In Delphi "uses"
	public class Include
	{
		public string name;
	}
	
	}
    public class Constant
    {
        public string name;
        public string value;
    }

    public class Enums
    {
        public string name;
        public List<string> enums;
        public List<string> values;
    }

    public class Alias
    {
        public string name;
        public string type;
    }

    public class Entity
    {
        public string inheritance;
        public string name;
    }

    public class Variable : Entity
    {
        public string type;
    }

    public class Property : Variable
    {
        public string read;
        public string write;
    }

    public class Procedure : Entity
    {
        public bool isVirtual;
        public bool isAbstract;
        public bool isStatic;
        public List<Variable>  parameters;
        public List<Variable> classVariables;
        public List<string> commands; 
    }

    public class Function : Procedure
    {
        public string type;
    }

    public class Record
    {
        public List<Variable> variables;
    }

    public class Interface: Record
    {
    	public string name;
        public string baseclass;
        public List<Property> properties;
        public List<Procedure> procedures;
        public List<Function> functions;
    }

    public class Class : Interface
    {
        public string baseclass;
    }

    public class Script
    {
    	public List<Include> includes;
        public List<Class> classes;
        public List<Interface> interfaces;
    }
}
