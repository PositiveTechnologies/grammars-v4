using System.Collections.Concurrent;
using Antlr4.Runtime;

namespace PythonParseTree
{
    public enum PythonVersion
    {
        Autodetect,
        Python2 = 2,
        Python3 = 3
    }

    public abstract class PythonParserBase : Parser
    {
        public PythonVersion Version { get; set; }

        protected PythonParserBase(ITokenStream input) : base(input)
        {
        }

        protected bool CheckVersion(int version) => Version == PythonVersion.Autodetect || version == (int) Version;

        protected void SetVersion(int requiredVersion) => Version = (PythonVersion) requiredVersion;

        protected bool CheckQuotes()
        {
            var first = _ctx.GetChild(0);
            var last = _ctx.GetChild(_ctx.ChildCount - 1);

            return first.GetText() == last.GetText() && last.GetText() != "f";
        }
    }
}
