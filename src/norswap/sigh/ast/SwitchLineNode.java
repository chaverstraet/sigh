package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;
import java.util.List;


public class SwitchLineNode extends StatementNode {
    private List<SwitchValueNode> valueNodes;
    private SwitchElseNode elseNode;

    @SuppressWarnings("unchecked")
    public SwitchLineNode (Span span, Object valueNodes,  Object elseNode ) {
        super(span);
        this.valueNodes = Util.cast(valueNodes, List.class);
        this.elseNode = Util.cast(elseNode, SwitchElseNode.class);
    }

    public String contents() {
        return "switchline";
    }
}
