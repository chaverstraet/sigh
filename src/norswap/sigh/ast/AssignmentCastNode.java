package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;

public class AssignmentCastNode extends ExpressionNode
{
    public final TypeNode left;
    public final ExpressionNode right;
    //public final TypeNode cast;

    public AssignmentCastNode (Span span, Object left, Object right) {
        super(span);
        this.left = Util.cast(left, TypeNode.class);
        this.right = Util.cast(right, ExpressionNode.class);
        //this.cast = Util.cast(cast, TypeNode.class);
    }

    @Override public String contents ()
    {
        String leftEqual = left.contents() + " = ";

        String candidate = leftEqual + right.contents();
        if (candidate.length() <= contentsBudget())
            return candidate;

        candidate = leftEqual + "(?)";
        return candidate.length() <= contentsBudget()
            ? candidate
            : "(?) = (?)";
    }
}
