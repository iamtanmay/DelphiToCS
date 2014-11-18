using System;
using System.Text;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Xml.Serialization;
using System.Xml;

namespace Translator
{
    public class Variable
    {
        [System.Xml.Serialization.XmlAttributeAttribute("Name")]
        public string name;
        [System.Xml.Serialization.XmlAttributeAttribute("Type")]
        public string type;
        [System.Xml.Serialization.XmlAttributeAttribute("Value")]
        public string value;
        [System.Xml.Serialization.XmlAttributeAttribute("Comment")]
        public string comment;
        [System.Xml.Serialization.XmlAttributeAttribute("isStatic")]
        public bool isStatic;

        public Variable()
        {
        }

        public Variable(string iname, string itype, string ivalue, string icomment, bool iisStatic)
        {
            name = iname.Trim();
            type = itype.Trim();
            value = ivalue.Trim();
            comment = icomment.Trim();
            isStatic = iisStatic;
        }
    }

    public class TypeAlias : Variable
    {
        public TypeAlias()
        {
        }
        public TypeAlias(string iname, string itype)
        {
            name = iname.Trim();
            type = itype.Trim();
        }
    }

    public class Constant: Variable
    {
        public Constant()
        {
        }
        public Constant(string ivalue)
        {
        }
        public Constant(string iname, string itype, string ivalue, string icomment)
        {
            name = iname;
            type = itype;
            value = ivalue;
            comment = icomment;
        }
        public Constant(string iname, string itype, string ivalue, string icomment, bool iisStatic)
        {
            name = iname;
            type = itype;
            value = ivalue;
            comment = icomment;
            isStatic = iisStatic;
        }

        //Evaluate the possible type for the Constant based on value

        public Constant(string iname, string ivalue)
        {
            string ttype = CheckType(ivalue.Trim());
            name = iname.Trim();
            type = ttype.Trim();
            value = ivalue.Trim();
        }

        public string CheckType(string value)
        {
            bool b;
            char c;
            int i;
            float f;

            if (bool.TryParse(value, out b))
                return "bool";

            if (char.TryParse(value, out c))
                return "char";

            if (float.TryParse(value, out f))
                return "float";
            
            if (int.TryParse(value, out i))
                return "int";

            return "string";
        }
    }

    public class Property : Variable
    {
        [System.Xml.Serialization.XmlAttributeAttribute("Get")]
        public string read;
        [System.Xml.Serialization.XmlAttributeAttribute("Set")]
        public string write;
        public Property()
        { }
        public Property(string iname, string itype, string iread, string iwrite, bool iisStatic)
        {
            name = iname.Trim();
            type = itype.Trim();
            read = iread.Trim();
            write = iwrite.Trim();
            isStatic = iisStatic;
        }
    }

    public class Enum
    {
        [System.Xml.Serialization.XmlAttributeAttribute("Name")]
        public string name;
        public List<Constant> enums;
        public Enum()
        {
            enums = new List<Constant>();
        }

        public Enum(string iname, List<Constant> ienums)
        {
            name = iname.Trim();
            enums = new List<Constant>();
            enums = ienums;
        }
    }

    public class Function
    {
        [System.Xml.Serialization.XmlAttributeAttribute("Name")]
        public string name;
        [System.Xml.Serialization.XmlAttributeAttribute("Return")]
        public string returnType = "void";

        [System.Xml.Serialization.XmlAttributeAttribute("isVirtual")]
        public bool isVirtual;
        [System.Xml.Serialization.XmlAttributeAttribute("isAbstract")]
        public bool isAbstract;
        [System.Xml.Serialization.XmlAttributeAttribute("isStatic")]
        public bool isStatic;

        public List<Variable> parameters;
        public List<Variable> variables;
        public List<Constant> constants;
        public List<string> commands;

        public Function()
        {
            parameters = new List<Variable>();
            variables = new List<Variable>();
            constants = new List<Constant>();
            commands = new List<string>();
            returnType = "void";
        }

        public Function(string iname, List<Variable> iparameters, string ireturnType, bool iisVirtual, bool iisAbstract, bool iisStatic, List<Constant> iconsts, List<Variable> ivariables, List<string> icommands)
        {
            parameters = new List<Variable>();
            variables = new List<Variable>();
            commands = new List<string>();

            name = iname.Trim();
            parameters = iparameters;
            returnType = ireturnType.Trim();
            
            if (returnType == "")
                returnType = "void";

            isVirtual = iisVirtual;
            isStatic = iisStatic;
            isAbstract = iisAbstract;
            constants = iconsts;
            variables = ivariables;
            commands = icommands;
        }
    }

    public class Class
    {
        //type = record, class or interface
        [System.Xml.Serialization.XmlAttributeAttribute("Name")]
        public string name;
        [System.Xml.Serialization.XmlAttributeAttribute("Type")]
        public string type;
        [System.Xml.Serialization.XmlAttributeAttribute("Base")]
        public string baseclass;
        public List<Variable> variables;
        public List<Constant> constants;
        public List<Enum> enums;
        public List<TypeAlias> types;
        public List<Property> properties;
        public List<Function> functions;
        public List<List<Function>> actions;

        public Class()
        {
            type = "";
            variables = new List<Variable>();
            constants = new List<Constant>();
            enums = new List<Enum>();
            types = new List<TypeAlias>();
            properties = new List<Property>();
            functions = new List<Function>();
            actions = new List<List<Function>>();
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
        [System.Xml.Serialization.XmlAttributeAttribute("Header")]
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
