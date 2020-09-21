/*
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2019, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2019, Thierry Marianne (thierry.marianne@weaving-the-web.org)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

using Antlr4.Runtime;
using static PhpParseTree.PhpLexer;

public enum Php
{
    Autodetect,
    V521 = 521,
    V53  = 530,
    V54  = 540,
    V55  = 550,
    V7   = 700,
    V71  = 710,
    V72  = 720,
    V74  = 740
}

public abstract class PhpLexerBase : Lexer
{
    private bool AspTags = true;
    private string _heredocIdentifier;
    private int _prevTokenType;
    private string _htmlNameText;
    private bool _phpScript;
    private bool _insideString;
    protected bool _scriptTag;
    protected bool _styleTag;

    public Php Version { get; set; }

    protected PhpLexerBase(ICharStream input)
        : base(input)
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

    public override IToken NextToken()
    {
        IToken token = base.NextToken();

        int type = token.Type;
        int channel = token.Channel;

        if (token.Type == PHPEnd || token.Type == PHPEndSingleLineComment)
        {
            if (_mode == SingleLineCommentMode)
            {
                // SingleLineCommentMode for such allowed syntax:
                // <?php echo "Hello world"; // comment ?>
                PopMode(); // exit from SingleLineComment mode.
            }
            PopMode(); // exit from PHP mode.

            if (string.Equals(token.Text, "</script>", System.StringComparison.Ordinal))
            {
                _phpScript = false;
                type = HtmlScriptClose;
            }
            else
            {
                // Add semicolon to the end of statement if it is absence.
                // For example: <?php echo "Hello world" ?>
                if (_prevTokenType == SemiColon || _prevTokenType == Colon
                    || _prevTokenType == OpenCurlyBracket || _prevTokenType == CloseCurlyBracket)
                {
                    channel = SkipChannel;
                }
                else
                {
                    type = SemiColon;
                }
            }
        }
        else if (token.Type == HtmlName)
        {
            _htmlNameText = token.Text;
        }
        else if (token.Type == HtmlDoubleQuoteString)
        {
            if (string.Equals(token.Text, "php", System.StringComparison.OrdinalIgnoreCase) &&
                string.Equals(_htmlNameText, "language"))
            {
                _phpScript = true;
            }
        }
        else if (_mode == HereDoc)
        {
            // Heredoc and Nowdoc syntax support: http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
            switch (token.Type)
            {
                case StartHereDoc:
                case StartNowDoc:
                    _heredocIdentifier = token.Text.Substring(3).Trim().Trim('\'');
                    break;

                case HereDocText:
                    string tokenText = token.Text;

                    if (_input.La(-tokenText.Length - 1) == '\n' && string.Equals(tokenText.Trim(), _heredocIdentifier, System.StringComparison.Ordinal))
                    {
                        type = HereDocEnd;
                        PopMode();
                    }
                    break;
            }
        }
        else if (_mode == PHP)
        {
            if (_channel != Hidden)
            {
                _prevTokenType = token.Type;
            }
        }

        return type == token.Type && channel == token.Channel
            ? token
#if LIGHT_TOKEN
            : new PT.PM.AntlrUtils.LightToken((PT.PM.AntlrUtils.LightInputStream)token.InputStream, type, channel,
                token.TokenIndex, token.StartIndex, token.StopIndex);
#else
            : new CommonToken(new System.Tuple<ITokenSource, ICharStream>(token.TokenSource, token.InputStream), type,
                channel, token.StartIndex, token.StopIndex)
            {
                Text = token.Text,
                Line = token.Line,
                Column = token.Column
            };
#endif
    }

    protected bool IsNewLineOrStart(int pos)
    {
        return _input.La(pos) <= 0 || _input.La(pos) == '\r' || _input.La(pos) == '\n';
    }

    protected void PushModeOnHtmlClose()
    {
        PopMode();
        if (_scriptTag)
        {
            PushMode(!_phpScript ? SCRIPT : PHP);
            _scriptTag = false;
        }
        else if (_styleTag)
        {
            PushMode(STYLE);
            _styleTag = false;
        }
    }

    protected bool HasAspTags()
    {
        return AspTags;
    }

    protected bool HasPhpScriptTag()
    {
        return _phpScript;
    }

    protected void PopModeOnCurlyBracketClose()
    {
        if (_insideString)
        {
            _insideString = false;
            Channel = SkipChannel;
            PopMode();
        }
    }

    protected bool IsCurlyDollar(int pos)
    {
        return _input.La(pos) == '$';
    }

    protected void SetInsideString()
    {
        _insideString = true;
    }
}