package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;

public final class SwitchElseNode extends SwitchLineNode
{
    public final StatementNode statement;

    public SwitchElseNode (Span span,Object statement ) {
        super(span);
        this.statement = Util.cast(statement , StatementNode.class);
    }

    @Override public String contents ()
    {
        String candidate = String.format("else: statement ...");

        return candidate.length() <= contentsBudget()
            ? candidate
            : "else: statement ...";
    }
}
