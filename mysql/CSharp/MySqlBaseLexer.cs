using Antlr4.Runtime;
using System.Text;

public abstract class MySqlBaseLexer : Lexer
{
    public string _delimiter = ";";

    private string _custom_delimiter = ";";

    public MySqlBaseLexer(ICharStream input)
        : base(input)
    {
    }

    protected bool isDelimiterPrevious()
    {
        var delimiterTokenText = "DELIMITER ";
        var startIndex = _input.Index - delimiterTokenText.Length;
        if (startIndex < 0)
        {
            return false;
        }
        var prevToken = _input.GetText(new Antlr4.Runtime.Misc.Interval(startIndex, _input.Index - 1));
        return delimiterTokenText == prevToken;
    }

    protected bool setCustomDelimiter()
    {
        int i = 1;
        StringBuilder builder = new StringBuilder();
        var currentSymbol = (char)_input.La(i);
        while (currentSymbol != '\n')
        {
            builder.Append(currentSymbol);
            i++;
            currentSymbol = (char)_input.La(i);
            if (currentSymbol == '\0')
            {
                break;
            }
        }

        _custom_delimiter = builder.ToString();
        _input.Seek(_input.Index + _custom_delimiter.Length - 1);
        return true;
    }

    protected bool isDelimiterChanged()
    {
        return _delimiter != _custom_delimiter;
    }

    protected bool isCustomDelimiter()
    {
        for (int i = 0; i < _custom_delimiter.Length; i++)
        {
            char symbol = (char)_input.La(i+1);
            if (symbol != _custom_delimiter[i])
            {
                return false;
            }
        }

        _input.Seek(_input.Index + _custom_delimiter.Length - 1);
        return true;
    }
}