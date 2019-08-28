using Antlr4.Runtime;

public abstract class KotlinBaseLexer : Lexer
{
    public void PopModeIfNecessary()
    {
        if (_modeStack.Count > 0)
            PopMode();
    }

    protected KotlinBaseLexer(ICharStream input) : base(input)
    {
    }

}