package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;

public final class AppendDeclarationNode extends ExpressionNode
{
    public final String name;
    public final ExpressionNode added;

    public AppendDeclarationNode (Span span,  Object identifier, Object added ) {
        super(span);
        this.name = Util.cast(identifier, String.class);
        this.added = Util.cast(added, ExpressionNode.class);

    }

    @Override
    public String contents () {
        return null;
    }

}
