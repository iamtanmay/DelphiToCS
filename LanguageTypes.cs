using System.Collections.Generic;

namespace Translator
{
    public class Variable
    {
        public string name, type;
        public bool isStatic;

        public Variable()
        {
        }
        public Variable(string name, string type, bool isStatic)
        {
        }
    }

    public class Type: Variable
    {
        public Type()
        {
        }
        public Type(string name, string type)
        {
        }
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
        public Property(string name, string type, string read, string write, bool isStatic)
        {
        }
    }

    public class Enum
    {
        public string name;
        public List<Constant> enums;
        public Enum()
        {
            enums = new List<Constant>();
        }

        public Enum(string name, List<Constant> enums)
        {
        }
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
            parameters = new List<Variable>();
            variables = new List<Variable>();
            commands = new List<string>();
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

        public Class()
        {
            variables = new List<Variable>();
            constants = new List<Constant>();
            enums = new List<Enum>();
            types = new List<Type>();
            properties = new List<Property>();
            functions = new List<Function>();
        }
    }
    
    public class Interface:Class
    {
        public Interface() : base()
        {
        }
    }

    public class Script
    {
        public List<string> header;
    	public List<string> includes;
        public List<Class> classes;
        public List<Interface> interfaces;

        public Script()
        {
            header = new List<string>();
            includes = new List<string>();
            classes = new List<Class>();
            interfaces = new List<Interface>();
        }
    }
}
