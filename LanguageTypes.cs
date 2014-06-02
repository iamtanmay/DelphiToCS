namespace LanguageTypes
{
    public class Variable
    {
        public string name, type;
    }

    public class Type: Variable;
    {
    }

    public class Constant: Variable
    {
        public string value;
    }

    public class Property : Variable
    {
        public string read, write;
    }

    public class Enum
    {
        public string name;
        public List<Constant> enums;
    }

    public class Function
    {
        public bool isVirtual, isAbstract, isStatic;
        public List<Variable> parameters, variables;
        public List<string> commands; 
        public string returnType;
    }
    
    public class Class
    {
    	public string name, baseclass;
        public List<Variable> variables;
        public List<Property> properties;
        public List<Function> functions;
    }
    
    public class Interface:Class
    {
    }

    public class Script
    {
    	public List<string> includes;
        public List<Class> classes;
        public List<Interface> interfaces;
    }
}
