package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;

public final class SwitchValueNode extends SwitchLineNode
{
    public final IntLiteralNode basic_switch_value;
    public final StatementNode statement;

    public SwitchValueNode (Span span, Object basic_switch_value, Object statement ) {
        super(span);
        this.basic_switch_value = Util.cast(basic_switch_value, IntLiteralNode.class);
        this.statement = Util.cast(statement , StatementNode.class);
    }

    @Override public String contents ()
    {
        String candidate = String.format("%s: statement ...", basic_switch_value.contents());

        return candidate.length() <= contentsBudget()
            ? candidate
            : "?: statement ...";
    }
}
