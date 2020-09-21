using System;
using System.Text.RegularExpressions;
using Antlr4.Runtime;

public abstract class PhpParserBase : Parser
{
    private Regex _intRegex = new Regex("int(eger)?", RegexOptions.IgnoreCase);
    private Regex _boolRegex = new Regex("bool(ean)?", RegexOptions.IgnoreCase);
    private Regex _floatRegex = new Regex("float|double|real", RegexOptions.IgnoreCase);

    public Php Version { get; set; }

    protected PhpParserBase(ITokenStream input) : base(input)
    {
    }

    protected bool CheckVersion(Php version) => Version == Php.Autodetect || version >= Version;

    protected void SetVersion(Php requiredVersion)
    {
        if (Version < requiredVersion)
        {
            Version = requiredVersion;
        }
    }

    protected bool MatchInt() => _intRegex.IsMatch(_input.Lt(1).Text);

    protected bool MatchBool() => _boolRegex.IsMatch(_input.Lt(1).Text);

    protected bool MatchFloat() => _floatRegex.IsMatch(_input.Lt(1).Text);

    protected bool Next(string str) => _input.Lt(1).Text.Equals(str, StringComparison.OrdinalIgnoreCase);
}