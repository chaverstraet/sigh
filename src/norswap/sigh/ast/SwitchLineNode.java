package norswap.sigh.ast;

import norswap.autumn.positions.Span;

public abstract class SwitchLineNode extends SighNode {
    public SwitchLineNode (Span span) {
        super(span);
    }
}
