import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.*;

public class KotlinBaseLexer extends Lexer {

    public KotlinLexer(TokenStream input) {
            super(input);
            _interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
        }

    public void PopModeIfNecessary(){
        if (!_modeStack.isEmpty()) {
            popMode();
        }
    }
}