using System.Collections.Generic;

namespace Translator
{
    public class Variable
    {
        public string name, type;
        public Variable()
        {
        }
        public Variable(string name, string type)
        {
        }
    }

    public class Type: Variable
    {
    }

    public class Constant: Variable
    {
        public string value;
        public Constant()
        {
        }
        public Constant(string value)
        {
        }
        public Constant(string name, string type, string value)
        {
        }
    }

    public class Property : Variable
    {
        public string read, write;
        public Property(string name, string type, string read, string write)
        {
        }
    }

    public class Enum
    {
        public string name;
        public List<Constant> enums;
    }

    public class Function
    {
        public string name;
        public bool isVirtual, isAbstract, isStatic;
        public List<Variable> parameters, variables;
        public List<string> commands; 
        public string returnType;

        public Function()
        {
        }

        public Function(string name, List<Variable> parameters, string returnType, bool isVirtual, bool isAbstract, bool isStatic, List<Variable> variables, List<string> commands)
        {
        }
    }
    
    public class Class
    {
    	public string name, baseclass;
        public List<Variable> variables;
        public List<Constant> constants;
        public List<Enum> enums;
        public List<Type> types;
        public List<Property> properties;
        public List<Function> functions;
    }
    
    public class Interface:Class
    {
    }

    public class Script
    {
        public List<string> header;
    	public List<string> includes;
        public List<Class> classes;
        public List<Interface> interfaces;
    }
}
