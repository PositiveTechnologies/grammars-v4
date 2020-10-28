using Antlr4.Runtime;

namespace GoParseTree
{
    public abstract class GoLexerBase : Lexer
    {
        private bool emitSemicolon;
        protected GoLexerBase(ICharStream input) : base(input)
        {
        }

        public override IToken NextToken()
        {
            if (emitSemicolon && _input.La(2) == '\n')
            {
                IToken token = _factory.Create(_tokenFactorySourcePair, GoLexer.SEMI, "", _channel, _tokenStartCharIndex, CharIndex - 1, _tokenStartLine, _tokenStartCharPositionInLine);
                Emit(token);
                emitSemicolon = false;
                return token;
            }

            emitSemicolon = false;

            return base.NextToken();
        }

        protected void HandleNewLine()
        {
            emitSemicolon = true;
        }
    }
}