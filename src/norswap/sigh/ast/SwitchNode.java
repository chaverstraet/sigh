package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;

public final class SwitchNode extends StatementNode
{
    public final String identifier;
    public final SwitchBlockNode switch_block;

    public SwitchNode (Span span, Object identifier, Object switch_block) {
        super(span);
        this.identifier = Util.cast(identifier, String.class);
        this.switch_block = Util.cast(switch_block, SwitchBlockNode.class);
    }

    @Override public String contents ()
    {
        String candidate = String.format("switch (%s) ...", identifier);

        return candidate.length() <= contentsBudget()
            ? candidate
            : "switch (?) ...";
    }
}
