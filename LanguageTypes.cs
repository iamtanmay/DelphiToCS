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
        public Variable(string iname, string itype, bool iisStatic)
        {
            name = iname;
            type = itype;
            isStatic = iisStatic;
        }
    }

    public class Type: Variable
    {
        public Type()
        {
        }
        public Type(string iname, string itype)
        {
            name = iname;
            type = itype;
        }
    }

    public class Constant: Variable
    {
        public string value;
        public Constant()
        {
        }
        public Constant(string ivalue)
        {
        }
        public Constant(string iname, string itype, string ivalue)
        {
            name = iname;
            type = itype;
            value = ivalue;
        }
    }

    public class Property : Variable
    {
        public string read, write;
        public Property(string iname, string itype, string iread, string iwrite, bool iisStatic)
        {
            name = iname;
            type = itype;
            read = iread;
            write = iwrite;
            isStatic = iisStatic;
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

        public Enum(string iname, List<Constant> ienums)
        {
            name = iname;
            enums = new List<Constant>();
            enums = ienums;
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

        public Function(string iname, List<Variable> iparameters, string ireturnType, bool iisVirtual, bool iisAbstract, bool iisStatic, List<Variable> ivariables, List<string> icommands)
        {
            parameters = new List<Variable>();
            variables = new List<Variable>();
            commands = new List<string>();

            name = iname;
            parameters = iparameters;
            returnType = ireturnType;
            isVirtual = iisVirtual;
            isStatic = iisStatic;
            isAbstract = iisAbstract;
            variables = ivariables;
            commands = icommands;
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
