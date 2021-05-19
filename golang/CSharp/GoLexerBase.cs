using System.Collections.Generic;
using Antlr4.Runtime;
using static GoParseTree.GoLexer;

namespace GoParseTree
{
    public abstract class GoLexerBase : Lexer
    {
        private readonly HashSet<int> tokensToEmmitSemicolon = new HashSet<int>
        {
            IDENTIFIER, R_PAREN, R_CURLY, R_BRACKET, INTERPRETED_STRING_LIT, RAW_STRING_LIT, PLUS_PLUS, MINUS_MINUS,
            DECIMAL_LIT, IMAGINARY_LIT, RUNE_LIT, FLOAT_LIT, BREAK, FALLTHROUGH, CONTINUE, RETURN
        };
        private readonly HashSet<int> symbolsToSkip = new HashSet<int> { '\t', ' ', '\r'};

        protected GoLexerBase(ICharStream input) : base(input)
        {
        }

        public override IToken NextToken()
        {
            
            if (tokensToEmmitSemicolon.Contains(Type))
            {
                var index = 1;
                int next;
                do
                {
                    next = _input.La(index);
                    index++;
                } while (symbolsToSkip.Contains(next));

                if (next == '\n' || (next == '/' && _input.La(index) == '/'))
                {
                    //for case when eos is followed by comment location will be equal to location of comment start symbol ('/')
                    IToken token = _factory.Create(_tokenFactorySourcePair, SEMI, "", _channel, CharIndex + 1,
                        CharIndex + 1, _tokenStartLine, CharIndex + 1);
                    Emit(token);
                    Type = token.Type;
                    return token;
                }
            }

            return base.NextToken();
        }
    }
}