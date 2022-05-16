package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;
import java.util.List;

public final class LambdaTypeNode extends TypeNode
{
    public final String name;
    public final List<SimpleTypeNode> parameterTypes;
    public final SimpleTypeNode returnType;

    public LambdaTypeNode (Span span, Object parameterTypes, Object returnType) {
        super(span);
        this.name = "Lambda";
        this.parameterTypes = Util.cast(parameterTypes, List.class);
        this.returnType = Util.cast(returnType, SimpleTypeNode.class);
    }

    @Override public String contents () {
        return name;
    }
}
