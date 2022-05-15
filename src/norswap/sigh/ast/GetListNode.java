package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;
import java.util.ArrayList;

public class GetListNode extends ExpressionNode {

    public final ExpressionNode liste;
    public final ExpressionNode index;


    public GetListNode (Span span, Object liste, Object index) {
        super(span);
        this.liste = Util.cast(liste, ExpressionNode.class);
        this.index = Util.cast(index, ExpressionNode.class);
    }

    @Override
    public String contents () {
        return liste.toString() + ".(" + index.toString() + ")";
    }
}
