using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Text;

namespace delphi2cs
{
    enum TokenType { Undefined, String, Text, Number, Hex, White, Symbol, SemiColon, Remark, EOF }

    class DelphiTokenizer
    {
        MemoryStream ms = new MemoryStream();
        StreamReader sr;

        int ident = 0;
        string remarks = "";
        string next = "";
        TokenType nextType = TokenType.Undefined;

        //----------------------------------------------------------------------
        public DelphiTokenizer()
        {
           sr = new StreamReader(ms);                
        }

        public int Percent()
        {
            return (int)(100 * ms.Position / ms.Length);
        }

        //----------------------------------------------------------------------
        public void LoadString(string text)
        {
            StreamWriter sw = new StreamWriter(ms);
            sw.Write(text);
            sw.Flush();
            ms.Seek(0, SeekOrigin.Begin);
            ReadTokenFromFile();
        }

        //----------------------------------------------------------------------
        public string NextToken()
        {
            string rv = next;
            ReadTokenFromFile();
            return rv;
        }

        //----------------------------------------------------------------------
        public string PeekToken()
        {
            return next;
        }
        //----------------------------------------------------------------------
        public void Expect(string text)
        {
            if (next.ToLower() != text.ToLower())
            {
                string msg = string.Format("//ERROR: expected '{0}' found '{1}'", text, next);
                System.Console.Error.WriteLine(msg);
                throw new Exception(msg);
            }
            else
                ReadTokenFromFile();
        }

        //----------------------------------------------------------------------
        public string ReadUntil(string text)
        {
            string rv = next;
            while (!rv.EndsWith(text))
                rv += (char)sr.Read();
            rv = rv.Remove(rv.Length - text.Length);
            /*
            TokenType prev = TokenType.Undefined;
            while (next != text)
            {
                if (prev == nextType)
                    rv += " ";
                rv += next;
                prev = nextType;
                ReadTokenFromFile();
            }
             */
            
            ReadTokenFromFile(); // the text expected
            return rv;
        }

        //----------------------------------------------------------------------
        public string ReadUntil(string[] texts, out string found)
        {
            string rv = next;
            found = "";
            for (int i = 0 ; i<texts.Length; i++)
                if (texts[i] != ";")
                    texts[i] = " " + texts[i];

            while (found == "")
            {
                int x = sr.Read();
                if (x == 0x27) // "'"
                {
                    rv += (char)x;
                    while ((x = sr.Read()) != 0x27)
                        rv += (char)x;
                }
                if (x < 32)
                    rv += " ";
                else
                    rv += (char)x;
                foreach (string word in texts)
                    if (rv.EndsWith(word))
                        found = word;
            }
            rv = rv.Remove(rv.Length - found.Length);
            ReadTokenFromFile(); // the text expected
            found = found.Trim();
            return rv;
        }

        //----------------------------------------------------------------------
        public int GetIdent()
        {
            int rv = ident;
            return rv;
        }

        //----------------------------------------------------------------------
        public string GetRemarks()
        {
            string rv = remarks;
            remarks = "";
            return rv;
        }

        //----------------------------------------------------------------------
        public bool HasRemarks { get { return remarks != ""; } }

        //----------------------------------------------------------------------
        private TokenType CharType(char ch)
        {
            if (nextType == TokenType.Undefined)
            {
                if ((ch >= 'a') && (ch <= 'z'))
                    return TokenType.Text;
                else if ((ch >= 'A') && (ch <= 'Z'))
                    return TokenType.Text;
                else if (ch == '_')
                    return TokenType.Text;
                else if ((ch >= '0') && (ch <= '9'))
                    return TokenType.Number;
                else if (ch == '$')
                    return TokenType.Hex;
                else if (ch == '\'')
                    return TokenType.String;
                else if (ch == '#')
                    return TokenType.Text;
                else if (ch == '{')
                    return TokenType.Remark;
                else if ((ch == ' ') || (ch == '\n') || (ch == '\r') || (ch == '\t'))
                    return TokenType.White;
                else
                    return TokenType.Symbol;
                
            }
            else if (nextType == TokenType.Number)
            {
                 if ((ch >= '0') && (ch <= '9'))
                    return TokenType.Number;
            }
            else if (nextType == TokenType.Hex)
            {
                if ((ch >= '0') && (ch <= '9'))
                    return TokenType.Hex;
                else if ((ch >= 'a') && (ch <= 'f'))
                    return TokenType.Hex;
                else if ((ch >= 'A') && (ch <= 'F'))
                    return TokenType.Hex;
                else if (ch == '$')
                    return TokenType.Hex;
            }
            else if (nextType == TokenType.String)
            {
                if ((next.Length >= 2) && next.StartsWith("\'") && next.EndsWith("\'"))
                {
                    next = next.Replace('\'','"');
                    return TokenType.Undefined;
                }
                else
                    return TokenType.String;
            }
            else if (nextType == TokenType.Text)
            {
                if ((ch >= 'a') && (ch <= 'z'))
                    return TokenType.Text;
                else if ((ch >= 'A') && (ch <= 'Z'))
                    return TokenType.Text;
                else if ((ch >= '0') && (ch <= '9'))
                    return TokenType.Text;
                else if (ch == '_')
                    return TokenType.Text;
                else if (ch == '#')
                    return TokenType.Text;
            }
            else if (nextType == TokenType.Remark)
            {
                if (next.StartsWith("//") && next.EndsWith("\n"))
                    return TokenType.Undefined;
                else if (next.StartsWith("{") && next.EndsWith("}"))
                    return TokenType.Undefined;
                else
                    return TokenType.Remark;
            }
            else if (nextType == TokenType.Symbol)
            {
                if ((next == "(") || (next == ")") || (next == "[") || (next == "]") || (next == "=") || (next == ":=") || (next == "^") || (next == ",") || (next == ";")) // must appear alone! don't group together like );
                    return TokenType.Undefined;
                if ((ch == '/') && (next == "/"))
                {
                    nextType = TokenType.Remark;
                    return TokenType.Remark;
                }
                if ((ch == '<') || (ch == '>') || (ch == '=') || (ch == ';') || (ch == ':') || (ch == ',') || (ch == '+') || (ch == '-') || (ch == '/') || (ch == '*') || (ch == '[') || (ch == ']') || (ch == '(') || (ch == ')') || (ch == '.') || (ch == '@') || (ch == '^'))
                    return TokenType.Symbol;
            }
            return TokenType.Undefined;
        }

        //----------------------------------------------------------------------
        private void CheckNextCharType()
        {
            // skip white spaces
            while (CharType((char)sr.Peek()) == TokenType.White)
            {
                char ch = (char)sr.Read();
            }
            
            nextType =  CharType((char)sr.Peek());
        }

        //----------------------------------------------------------------------
        private void ReadTokenType()
        {
            while (CharType((char)sr.Peek()) == nextType)
            {
                int b = sr.Read();
                if (b > 0x7f)
                    b = 0x2A;
                next = next + (char)b; 
            }
        }
        
        //----------------------------------------------------------------------
        private void ReadTokenFromFile()
        {

            nextType = TokenType.Remark; // prime the loop
            while (nextType == TokenType.Remark)
            {
                next = "";
                nextType = TokenType.Undefined;

                CheckNextCharType();
                ReadTokenType();
            }
            if (next == "RUSSIAN_CHARSET")
                Console.WriteLine();
            /*
            if (sr.EndOfStream)
            {
                next = "";
                nextType = TokenType.EOF;
                return;
            }
            */
        }



        internal void PushBack(string p)
        {   
         
            ms.Seek(-next.Length, SeekOrigin.Current); // push next
            ms.Flush();            
            int ch = sr.Peek();
            while ((sr.Peek() == ' ') || (sr.Peek() == '\n'))
            {
                ms.Seek(-1, SeekOrigin.Current);
                ms.Flush();
            }
            ms.Seek(-p.Length, SeekOrigin.Current); // push expression
            ms.Flush();
            ReadTokenFromFile();            
        }
    }
}
