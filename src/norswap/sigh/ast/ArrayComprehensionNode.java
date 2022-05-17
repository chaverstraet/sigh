package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;
import java.util.List;

public class ArrayComprehensionNode extends ExpressionNode
{
    public final String expression;
    public final String ref;
    public final ArrayLiteralNode array;


    @SuppressWarnings("unchecked")
    public ArrayComprehensionNode (Span span, Object expression, Object ref, Object array) {
        super(span);
        this.expression = Util.cast(expression, String.class);
        this.ref = Util.cast(ref, String.class);
        this.array = Util.cast(array, ArrayLiteralNode.class);
    }

    @Override public String contents () {
        return "for k in k array";
    }
}
