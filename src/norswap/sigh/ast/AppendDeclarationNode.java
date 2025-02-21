package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;

public final class AppendDeclarationNode extends StatementNode
{
    public final ExpressionNode name;
    public final ExpressionNode added;

    public AppendDeclarationNode (Span span,  Object identifier, Object added ) {
        super(span);
        this.name = Util.cast(identifier, ExpressionNode.class);
        this.added = Util.cast(added, ExpressionNode.class);

    }

    @Override
    public String contents () {
        return name + ".append(" + added + ")";
    }

}
