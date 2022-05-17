package norswap.sigh.ast;

import norswap.autumn.positions.Span;
import norswap.utils.Util;
import java.util.List;

public final class LambdaDeclarationNode extends DeclarationNode
{
    public final String name;
    public final List<ParameterNode> param;
    public final LambdaReturnNode expression;
    public final TypeNode typeNode;

    public LambdaDeclarationNode (Span span, Object name, Object type, Object param, Object expression) {
        super(span);
        this.name = Util.cast(name, String.class);
        this.param = Util.cast(param, List.class);
        this.expression = Util.cast(expression, LambdaReturnNode.class);
        this.typeNode = Util.cast(type, TypeNode.class);;
    }

    @Override public String name () {
        return name;
    }

    @Override public String contents () {
        return "var " + name;
    }

    @Override public String declaredThing () {
        return "variable";
    }
}
