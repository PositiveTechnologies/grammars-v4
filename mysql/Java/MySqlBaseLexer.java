import org.antlr.v4.runtime.*;

public abstract class MySqlBaseLexer extends Lexer{    
    
    public String _delimiter = ";";

    private String _custom_delimiter = ";";
    
    public MySqlBaseLexer(CharStream input){
		super(input);
    }

    protected boolean isDelimiterPrevious(){
        String delimiterTokenText = "DELIMITER ";
        int startIndex = _input.index() - delimiterTokenText.length();
        if (startIndex < 0){
            return false;
        }
        String prevToken = _input.getText(Interval.of(startIndex, _input.Index - 1));
        return delimiterTokenText == prevToken;
    }

    protected boolean setCustomDelimiter(){
        int i = 1;
        StringBuilder builder = new StringBuilder();
        var currentSymbol = (char)_input.LA(i);
        while (currentSymbol != '\n'){

            builder.append(currentSymbol);
            
            i++;
            currentSymbol = (char)_input.LA(i);
            
            if (currentSymbol == '\0'){
                break;
            }
        }

        _custom_delimiter = builder.ToString();
        _input.Seek(_input.index() + _custom_delimiter.length() - 1);
        return true;
    }

    protected boolean isDelimiterChanged(){
        return _delimiter != _custom_delimiter;
    }

    protected boolean isCustomDelimiter(){
        for (int i = 0; i < _custom_delimiter.length(); i++){
            char symbol = (char)_input.LA(i+1);
            if (symbol != _custom_delimiter.charAt(i)){
                return false;
            }
        }

        _input.Seek(_input.index() + _custom_delimiter.length() - 1);
        return true;
    }
}
