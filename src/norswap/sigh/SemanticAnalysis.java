package norswap.sigh;

import norswap.sigh.ast.*;
import norswap.sigh.scopes.DeclarationContext;
import norswap.sigh.scopes.DeclarationKind;
import norswap.sigh.scopes.RootScope;
import norswap.sigh.scopes.Scope;
import norswap.sigh.scopes.SyntheticDeclarationNode;
import norswap.sigh.types.*;
import norswap.uranium.Attribute;
import norswap.uranium.Reactor;
import norswap.uranium.Rule;
import norswap.utils.Util;
import norswap.utils.visitors.ReflectiveFieldWalker;
import norswap.utils.visitors.Walker;
import javax.sound.midi.Soundbank;
import java.sql.SQLOutput;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.IntStream;

import static java.lang.String.format;
import static norswap.sigh.ast.BinaryOperator.*;
import static norswap.utils.Util.cast;
import static norswap.utils.Vanilla.forEachIndexed;
import static norswap.utils.Vanilla.list;
import static norswap.utils.visitors.WalkVisitType.POST_VISIT;
import static norswap.utils.visitors.WalkVisitType.PRE_VISIT;

/**
 * Holds the logic implementing semantic analyzis for the language, including typing and name
 * resolution.
 *
 * <p>The entry point into this class is {@link #createWalker(Reactor)}.
 *
 * <h2>Big Principles
 * <ul>
 *     <li>Every {@link DeclarationNode} instance must have its {@code type} attribute to an
 *     instance of {@link Type} which is the type of the value declared (note that for struct
 *     declaration, this is always {@link TypeType}.</li>
 *
 *     <li>Additionally, {@link StructDeclarationNode} (and default
 *     {@link SyntheticDeclarationNode} for types) must have their {@code declared} attribute set to
 *     an instance of the type being declared.</li>
 *
 *     <li>Every {@link ExpressionNode} instance must have its {@code type} attribute similarly
 *     set.</li>
 *
 *     <li>Every {@link ReferenceNode} instance must have its {@code decl} attribute set to the the
 *     declaration it references and its {@code scope} attribute set to the {@link Scope} in which
 *     the declaration it references lives. This speeds up lookups in the interpreter and simplifies the compiler.</li>
 *
 *     <li>For the same reasons, {@link VarDeclarationNode} and {@link ParameterNode} should have
 *     their {@code scope} attribute set to the scope in which they appear (this also speeds up the
 *     interpreter).</li>
 *
 *     <li>All statements introducing a new scope must have their {@code scope} attribute set to the
 *     corresponding {@link Scope} (only {@link RootNode}, {@link BlockNode} and {@link
 *     FunDeclarationNode} (for parameters)). These nodes must also update the {@code scope}
 *     field to track the current scope during the walk.</li>
 *
 *     <li>Every {@link TypeNode} instance must have its {@code value} set to the {@link Type} it
 *     denotes.</li>
 *
 *     <li>Every {@link ReturnNode}, {@link BlockNode} and {@link IfNode} must have its {@code
 *     returns} attribute set to a boolean to indicate whether its execution causes
 *     unconditional exit from the surrounding function or main script.</li>
 *
 *     <li>The rules check typing constraints: assignment of values to variables, of arguments to
 *     parameters, checking that if/while conditions are booleans, and array indices are
 *     integers.</li>
 *
 *     <li>The rules also check a number of other constraints: that accessed struct fields exist,
 *     that variables are declared before being used, etc...</li>
 * </ul>
 */
public final class SemanticAnalysis
{
    // =============================================================================================
    // region [Initialization]
    // =============================================================================================

    private final Reactor R;

    /** Current scope. */
    private Scope scope;

    /** Current context for type inference (currently only to infer the type of empty arrays). */
    private SighNode inferenceContext;

    /** Index of the current function argument. */
    private int argumentIndex;

    // ---------------------------------------------------------------------------------------------

    private SemanticAnalysis(Reactor reactor) {
        this.R = reactor;
    }

    // ---------------------------------------------------------------------------------------------

    /**
     * Call this method to create a tree walker that will instantiate the typing rules defined
     * in this class when used on an AST, using the given {@code reactor}.
     */
    public static Walker<SighNode> createWalker (Reactor reactor)
    {
        ReflectiveFieldWalker<SighNode> walker = new ReflectiveFieldWalker<>(
            SighNode.class, PRE_VISIT, POST_VISIT);

        SemanticAnalysis analysis = new SemanticAnalysis(reactor);

        // expressions
        walker.register(IntLiteralNode.class,           PRE_VISIT,  analysis::intLiteral);
        walker.register(FloatLiteralNode.class,         PRE_VISIT,  analysis::floatLiteral);
        walker.register(StringLiteralNode.class,        PRE_VISIT,  analysis::stringLiteral);
        walker.register(ReferenceNode.class,            PRE_VISIT,  analysis::reference);
        walker.register(ConstructorNode.class,          PRE_VISIT,  analysis::constructor);
        walker.register(ArrayLiteralNode.class,         PRE_VISIT,  analysis::arrayLiteral);
        walker.register(ArrayComprehensionNode.class,         PRE_VISIT,  analysis::arrayComprehension);
        walker.register(ListLiteralNode.class,          PRE_VISIT,  analysis::ListLiteral);
        walker.register(ParenthesizedNode.class,        PRE_VISIT,  analysis::parenthesized);
        walker.register(FieldAccessNode.class,          PRE_VISIT,  analysis::fieldAccess);
        walker.register(ArrayAccessNode.class,          PRE_VISIT,  analysis::arrayAccess);
        walker.register(AppendDeclarationNode.class,    PRE_VISIT,  analysis::appendList);
        walker.register(GetListNode.class,              PRE_VISIT,  analysis::getList);
        walker.register(FunCallNode.class,              PRE_VISIT,  analysis::funCall);
        walker.register(UnaryExpressionNode.class,      PRE_VISIT,  analysis::unaryExpression);
        walker.register(BinaryExpressionNode.class,     PRE_VISIT,  analysis::binaryExpression);
        walker.register(AssignmentNode.class,           PRE_VISIT,  analysis::assignment);

        // types
        walker.register(SimpleTypeNode.class,           PRE_VISIT,  analysis::simpleType);
        walker.register(LambdaTypeNode.class,           PRE_VISIT,  analysis::LambdaType);
        walker.register(ArrayTypeNode.class,            PRE_VISIT,  analysis::arrayType);
        walker.register(ListTypeNode.class,             PRE_VISIT,  analysis::ListType);

        // declarations & scopes
        walker.register(RootNode.class,                 PRE_VISIT,  analysis::root);
        walker.register(BlockNode.class,                PRE_VISIT,  analysis::block);
        walker.register(VarDeclarationNode.class,       PRE_VISIT,  analysis::varDecl);
        walker.register(LambdaDeclarationNode.class,    PRE_VISIT,  analysis::lambdaDecl);
        walker.register(VarDeclarationWithCastNode.class,POST_VISIT,analysis::varDeclCast);
        walker.register(VarDeclarationWithCastNode.class,PRE_VISIT, analysis::varPREDeclCast);
        walker.register(FieldDeclarationNode.class,     PRE_VISIT,  analysis::fieldDecl);
        walker.register(ParameterNode.class,            PRE_VISIT,  analysis::parameter);
        //walker.register(ParamNode.class,                PRE_VISIT,  analysis::param);
        walker.register(FunDeclarationNode.class,       PRE_VISIT,  analysis::funDecl);
        walker.register(StructDeclarationNode.class,    PRE_VISIT,  analysis::structDecl);

        walker.register(RootNode.class,                 POST_VISIT, analysis::popScope);
        walker.register(BlockNode.class,                POST_VISIT, analysis::popScope);
        walker.register(FunDeclarationNode.class,       POST_VISIT, analysis::popScope);
        walker.register(LambdaDeclarationNode.class,    POST_VISIT, analysis::popScope);

        // statements
        walker.register(ExpressionStatementNode.class,  PRE_VISIT,  node -> {});
        walker.register(IfNode.class,                   PRE_VISIT,  analysis::ifStmt);
        walker.register(WhileNode.class,                PRE_VISIT,  analysis::whileStmt);
        walker.register(ReturnNode.class,               PRE_VISIT,  analysis::returnStmt);
        walker.register(LambdaReturnNode.class,               PRE_VISIT,  analysis::lambdaReturnStmt);
        walker.register(SwitchValueNode.class,          PRE_VISIT,  analysis::switchValuePREStmt);
        walker.register(SwitchElseNode.class,           PRE_VISIT,  analysis::switchElseStmt);
        walker.register(SwitchBlockNode.class,          PRE_VISIT,  analysis::switchBlock);
        walker.register(SwitchNode.class,               PRE_VISIT,  analysis::switchPREStmt);
        walker.register(SwitchValueNode.class,          POST_VISIT, analysis::switchValueStmt);
        walker.register(SwitchNode.class,               POST_VISIT, analysis::switchStmt);


        walker.registerFallback(POST_VISIT, node -> {});

        return walker;
    }

    // endregion
    // =============================================================================================
    // region [Expressions]
    // =============================================================================================

    private void intLiteral (IntLiteralNode node) {
        R.set(node, "type", IntType.INSTANCE);
        R.set(node, "value", node.value);
    }

    // ---------------------------------------------------------------------------------------------

    private void floatLiteral (FloatLiteralNode node) {
        R.set(node, "type", FloatType.INSTANCE);
        R.set(node, "value", node.value);

    }

    // ---------------------------------------------------------------------------------------------

    private void stringLiteral (StringLiteralNode node) {
        R.set(node, "type", StringType.INSTANCE);
        R.set(node, "value", node.value);

    }

    // ---------------------------------------------------------------------------------------------

    private void reference (ReferenceNode node)
    {
        final Scope scope = this.scope;

        // Try to lookup immediately. This must succeed for variables, but not necessarily for
        // functions or types. By looking up now, we can report looked up variables later
        // as being used before being defined.
        DeclarationContext maybeCtx = scope.lookup(node.name);

        if (maybeCtx != null) {
            R.set(node, "decl",  maybeCtx.declaration);
            R.set(node, "scope", maybeCtx.scope);

            R.rule(node, "type")
            .using(maybeCtx.declaration, "type")
            .by(Rule::copyFirst);
            return;
        }


        // Re-lookup after the scopes have been built.
        R.rule(node.attr("decl"), node.attr("scope"))
        .by(r -> {
            DeclarationContext ctx = scope.lookup(node.name);
            DeclarationNode decl = ctx == null ? null : ctx.declaration;

            if (ctx == null) {
                r.errorFor("Could not resolve: " + node.name,
                    node, node.attr("decl"), node.attr("scope"), node.attr("type"));
            }
            else {

                r.set(node, "scope", ctx.scope);
                r.set(node, "decl", decl);

                if (decl instanceof VarDeclarationNode)
                    r.errorFor("Variable used before declaration: " + node.name,
                        node, node.attr("type"));
                if (decl instanceof VarDeclarationWithCastNode)
                    r.errorFor("Variable used before declaration: " + node.name,
                        node, node.attr("type"));
                else
                    R.rule(node, "type")
                    .using(decl, "type")
                    .by(Rule::copyFirst);

            }
        });
    }

    // ---------------------------------------------------------------------------------------------

    private void constructor (ConstructorNode node)
    {
        R.rule()
        .using(node.ref, "decl")
        .by(r -> {
            DeclarationNode decl = r.get(0);

            if (!(decl instanceof StructDeclarationNode)) {
                String description =
                        "Applying the constructor operator ($) to non-struct reference for: "
                        + decl;
                r.errorFor(description, node, node.attr("type"));
                return;
            }

            StructDeclarationNode structDecl = (StructDeclarationNode) decl;

            Attribute[] dependencies = new Attribute[structDecl.fields.size() + 1];
            dependencies[0] = decl.attr("declared");
            forEachIndexed(structDecl.fields, (i, field) ->
                dependencies[i + 1] = field.attr("type"));

            R.rule(node, "type")
            .using(dependencies)
            .by(rr -> {
                Type structType = rr.get(0);
                Type[] params = IntStream.range(1, dependencies.length).<Type>mapToObj(rr::get)
                        .toArray(Type[]::new);
                rr.set(0, new FunType(structType, params));
            });
        });
    }

    // ---------------------------------------------------------------------------------------------

    private void arrayLiteral (ArrayLiteralNode node)
    {
        if (node.components.size() == 0) { // []
            // Empty array: we need a type int to know the desired type.

            final SighNode context = this.inferenceContext;

            if (context instanceof VarDeclarationNode)
                R.rule(node, "type")
                .using(context, "type")
                .by(Rule::copyFirst);
            else if (context instanceof FunCallNode) {
                R.rule(node, "type")
                .using(((FunCallNode) context).function.attr("type"), node.attr("index"))
                .by(r -> {
                    FunType funType = r.get(0);
                    r.set(0, funType.paramTypes[(int) r.get(1)]);
                });
            }
            return;
        }

        Attribute[] dependencies =
            node.components.stream().map(it -> it.attr("type")).toArray(Attribute[]::new);

        R.rule(node, "type")
        .using(dependencies)
        .by(r -> {
            Type[] types = IntStream.range(0, dependencies.length).<Type>mapToObj(r::get)
                    .distinct().toArray(Type[]::new);

            int i = 0;
            Type supertype = null;
            for (Type type: types) {
                if (type instanceof VoidType)
                    // We report the error, but compute a type for the array from the other elements.
                    r.errorFor("Void-valued expression in array literal", node.components.get(i));
                else if (supertype == null)
                    supertype = type;
                else {
                    supertype = commonSupertype(supertype, type);
                    if (supertype == null) {
                        r.error("Could not find common supertype in array literal.", node);
                        return;
                    }
                }
                ++i;
            }

            if (supertype == null)
                r.error(
                    "Could not find common supertype in array literal: all members have Void type.",
                    node);
            else
                r.set(0, new ArrayType(supertype));
        });
    }

    private void arrayComprehension(ArrayComprehensionNode node) {
        if (node.array.components.size() == 0) { // []
            // Empty array: we need a type int to know the desired type.

            final SighNode context = this.inferenceContext;

            if (context instanceof VarDeclarationNode)
                R.rule(node, "type")
                    .using(context, "type")
                    .by(Rule::copyFirst);
            else if (context instanceof FunCallNode) {
                R.rule(node, "type")
                    .using(((FunCallNode) context).function.attr("type"), node.attr("index"))
                    .by(r -> {
                        FunType funType = r.get(0);
                        r.set(0, funType.paramTypes[(int) r.get(1)]);
                    });
            }
            return;
        }

        Attribute[] dependencies =
            node.array.components.stream().map(it -> it.attr("type")).toArray(Attribute[]::new);

        R.rule(node, "type")
            .using(dependencies)
            .by(r -> {
                Type[] types = IntStream.range(0, dependencies.length).<Type>mapToObj(r::get)
                    .distinct().toArray(Type[]::new);

                int i = 0;
                Type supertype = null;
                for (Type type: types) {
                    if (type instanceof VoidType)
                        // We report the error, but compute a type for the array from the other elements.
                        r.errorFor("Void-valued expression in array literal", node.array.components.get(i));
                    else if (supertype == null)
                        supertype = type;
                    else {
                        supertype = commonSupertype(supertype, type);
                        if (supertype == null) {
                            r.error("Could not find common supertype in array literal.", node);
                            return;
                        }
                    }
                    ++i;
                }

                if (supertype == null)
                    r.error(
                        "Could not find common supertype in array literal: all members have Void type.",
                        node);
                else
                    r.set(0, new ArrayType(supertype));
            });
    }
    // ---------------------------------------------------------------------------------------------

    private void parenthesized (ParenthesizedNode node)
    {
        R.rule(node, "type")
        .using(node.expression, "type")
        .by(Rule::copyFirst);
    }

    // ---------------------------------------------------------------------------------------------

    private void fieldAccess (FieldAccessNode node)
    {
        R.rule()
        .using(node.stem, "type")
        .by(r -> {
            Type type = r.get(0);

            if (type instanceof ArrayType) {
                if (node.fieldName.equals("length"))
                    R.rule(node, "type")
                    .by(rr -> rr.set(0, IntType.INSTANCE));
                else
                    r.errorFor("Trying to access a non-length field on an array", node,
                        node.attr("type"));
                return;
            }
            
            if (!(type instanceof StructType)) {
                r.errorFor("Trying to access a field on an expression of type " + type,
                        node,
                        node.attr("type"));
                return;
            }

            StructDeclarationNode decl = ((StructType) type).node;

            for (DeclarationNode field: decl.fields)
            {
                if (!field.name().equals(node.fieldName)) continue;

                R.rule(node, "type")
                .using(field, "type")
                .by(Rule::copyFirst);

                return;
            }

            String description = format("Trying to access missing field %s on struct %s",
                    node.fieldName, decl.name);
            r.errorFor(description, node, node.attr("type"));
        });
    }

    private void ListLiteral (ListLiteralNode node)
    {
        if (node.components.size() == 0) { // []
            // Empty array: we need a type int to know the desired type.

            final SighNode context = this.inferenceContext;

            if (context instanceof VarDeclarationNode)
                R.rule(node, "type")
                    .using(context, "type")
                    .by(Rule::copyFirst);
            else if (context instanceof FunCallNode) {
                R.rule(node, "type")
                    .using(((FunCallNode) context).function.attr("type"), node.attr("index"))
                    .by(r -> {
                        FunType funType = r.get(0);
                        r.set(0, funType.paramTypes[(int) r.get(1)]);
                    });
            }
            return;
        }

        Attribute[] dependencies =
            node.components.stream().map(it -> it.attr("type")).toArray(Attribute[]::new);

        R.rule(node, "type")
            .using(dependencies)
            .by(r -> {
                Type[] types = IntStream.range(0, dependencies.length).<Type>mapToObj(r::get)
                    .distinct().toArray(Type[]::new);

                int i = 0;
                Type supertype = null;
                for (Type type: types) {
                    if (type instanceof VoidType)
                        // We report the error, but compute a type for the array from the other elements.
                        r.errorFor("Void-valued expression in list literal", node.components.get(i));
                    else if (supertype == null)
                        supertype = type;
                    else {
                        supertype = commonSupertype(supertype, type);
                        if (supertype == null) {
                            r.error("Could not find common supertype in list literal.", node);
                            return;
                        }
                    }
                    ++i;
                }

                if (supertype == null)
                    r.error(
                        "Could not find common supertype list literal: all members have Void type.",
                        node);
                else
                    r.set(0, new ListType(supertype));
            });
    }



    // ---------------------------------------------------------------------------------------------

    private void appendList (AppendDeclarationNode node) {
        R.rule()
            .using(node.added.attr("type"), node.name.attr("type"))
            .by(r-> {
                Type type = r.get(0);
                Type type2 = r.get(1);
                String expected = type2.toString().substring(0, type2.toString().length()-2);

                if (!expected.equals(type.toString())) {
                    r.error(format("Can not append a %s to a list of %s", type.toString(), expected), type2);
                }
            });
    }

    private void getList (GetListNode node) {
        R.rule()
            .using(node.index.attr("type"))
            .by(r-> {
                Type type = r.get(0);

                if (!type.toString().equals("Int")) {
                    r.error("Index is not an integer but: " + type.toString(), type);
                }
            });
        R.rule(node, "type")
            .using(node.liste, "type")
            .by(r -> {
                Type type = r.get(0);
                if (type instanceof ListType)
                    r.set(0, ((ListType) type).componentType);
                else
                    r.error("Trying to index a non-array expression of type " + type, node);
            });
    }


    private void arrayAccess (ArrayAccessNode node)
    {
        R.rule()
        .using(node.index, "type")
        .by(r -> {
            Type type = r.get(0);
            if (!(type instanceof IntType))
                r.error("Indexing an array using a non-Int-valued expression", node.index);
        });

        R.rule(node, "type")
        .using(node.array, "type")
        .by(r -> {
            Type type = r.get(0);
            if (type instanceof ArrayType)
                r.set(0, ((ArrayType) type).componentType);
            else
                r.error("Trying to index a non-array expression of type " + type, node);
        });
    }

    // ---------------------------------------------------------------------------------------------

    private void funCall (FunCallNode node)
    {
        this.inferenceContext = node;

        Attribute[] dependencies = new Attribute[node.arguments.size() + 1];
        dependencies[0] = node.function.attr("type");
        forEachIndexed(node.arguments, (i, arg) -> {
            dependencies[i + 1] = arg.attr("type");
            R.set(arg, "index", i);
        });

        R.rule(node, "type")
        .using(dependencies)
        .by(r -> {
            Type maybeFunType = r.get(0);

            if (!(maybeFunType instanceof FunType)) {
                r.error("trying to call a non-function expression: " + node.function, node.function);
                return;
            }

            FunType funType = cast(maybeFunType);
            r.set(0, funType.returnType);

            Type[] params = funType.paramTypes;
            List<ExpressionNode> args = node.arguments;

            if (params.length != args.size())
                r.errorFor(format("wrong number of arguments, expected %d but got %d",
                        params.length, args.size()),
                    node);

            int checkedArgs = Math.min(params.length, args.size());

            for (int i = 0; i < checkedArgs; ++i) {
                Type argType = r.get(i + 1);
                Type paramType = funType.paramTypes[i];
                if (!isAssignableTo(argType, paramType))
                    r.errorFor(format(
                            "incompatible argument provided for argument %d: expected %s but got %s",
                            i, paramType, argType),
                        node.arguments.get(i));
            }
        });
    }

    // ---------------------------------------------------------------------------------------------

    private void unaryExpression (UnaryExpressionNode node)
    {
        assert node.operator == UnaryOperator.NOT; // only one for now
        R.set(node, "type", BoolType.INSTANCE);

        R.rule()
        .using(node.operand, "type")
        .by(r -> {
            Type opType = r.get(0);
            if (!(opType instanceof BoolType))
                r.error("Trying to negate type: " + opType, node);
        });
    }

    // endregion
    // =============================================================================================
    // region [Binary Expressions]
    // =============================================================================================

    private void binaryExpression (BinaryExpressionNode node)
    {
        R.rule(node, "type")
        .using(node.left.attr("type"), node.right.attr("type"))
        .by(r -> {
            Type left  = r.get(0);
            Type right = r.get(1);

            if (node.operator == ADD && (left instanceof StringType || right instanceof StringType))
                r.set(0, StringType.INSTANCE);
            else if (isArithmetic(node.operator))
                binaryArithmetic(r, node, left, right);
            else if (isComparison(node.operator))
                binaryComparison(r, node, left, right);
            else if (isLogic(node.operator))
                binaryLogic(r, node, left, right);
            else if (isEquality(node.operator))
                binaryEquality(r, node, left, right);
        });
    }

    // ---------------------------------------------------------------------------------------------

    private boolean isArithmetic (BinaryOperator op) {
        return op == ADD || op == MULTIPLY || op == SUBTRACT || op == DIVIDE || op == REMAINDER;
    }

    private boolean isComparison (BinaryOperator op) {
        return op == GREATER || op == GREATER_EQUAL || op == LOWER || op == LOWER_EQUAL;
    }

    private boolean isLogic (BinaryOperator op) {
        return op == OR || op == AND;
    }

    private boolean isEquality (BinaryOperator op) {
        return op == EQUALITY || op == NOT_EQUALS;
    }

    // ---------------------------------------------------------------------------------------------

    private void binaryArithmetic (Rule r, BinaryExpressionNode node, Type left, Type right)
    {
        if (left instanceof IntType)
            if (right instanceof IntType)
                r.set(0, IntType.INSTANCE);
            else if (right instanceof FloatType)
                r.set(0, FloatType.INSTANCE);
            else
                r.error(arithmeticError(node, "Int", right), node);
        else if (left instanceof FloatType)
            if (right instanceof IntType || right instanceof FloatType)
                r.set(0, FloatType.INSTANCE);
            else
                r.error(arithmeticError(node, "Float", right), node);
        else
            r.error(arithmeticError(node, left, right), node);
    }

    // ---------------------------------------------------------------------------------------------

    private static String arithmeticError (BinaryExpressionNode node, Object left, Object right) {
        return format("Trying to %s %s with %s", node.operator.name().toLowerCase(), left, right);
    }

    // ---------------------------------------------------------------------------------------------

    private void binaryComparison (Rule r, BinaryExpressionNode node, Type left, Type right)
    {
        r.set(0, BoolType.INSTANCE);

        if (!(left instanceof IntType) && !(left instanceof FloatType))
            r.errorFor("Attempting to perform arithmetic comparison on non-numeric type: " + left,
                node.left);
        if (!(right instanceof IntType) && !(right instanceof FloatType))
            r.errorFor("Attempting to perform arithmetic comparison on non-numeric type: " + right,
                node.right);
    }

    // ---------------------------------------------------------------------------------------------

    private void binaryEquality (Rule r, BinaryExpressionNode node, Type left, Type right)
    {
        r.set(0, BoolType.INSTANCE);

        if (!isComparableTo(left, right))
            r.errorFor(format("Trying to compare incomparable types %s and %s", left, right),
                node);
    }

    // ---------------------------------------------------------------------------------------------

    private void binaryLogic (Rule r, BinaryExpressionNode node, Type left, Type right)
    {
        r.set(0, BoolType.INSTANCE);

        if (!(left instanceof BoolType))
            r.errorFor("Attempting to perform binary logic on non-boolean type: " + left,
                node.left);
        if (!(right instanceof BoolType))
            r.errorFor("Attempting to perform binary logic on non-boolean type: " + right,
                node.right);
    }

    // ---------------------------------------------------------------------------------------------

    private void assignment (AssignmentNode node)
    {
        R.rule(node, "type")
        .using(node.left.attr("type"), node.right.attr("type"))
        .by(r -> {
            Type left  = r.get(0);
            Type right = r.get(1);

            r.set(0, r.get(0)); // the type of the assignment is the left-side type

            if (node.left instanceof ReferenceNode
            ||  node.left instanceof FieldAccessNode
            ||  node.left instanceof    ArrayAccessNode) {
                if (!isAssignableTo(right, left))
                    r.errorFor("Trying to assign a value to a non-compatible lvalue.", node);
            }
            else
                r.errorFor("Trying to assign to an non-lvalue expression.", node.left);
        });
    }

    // endregion
    // =============================================================================================
    // region [Types & Typing Utilities]
    // =============================================================================================

    private void simpleType (SimpleTypeNode node)
    {
        final Scope scope = this.scope;

        R.rule()
            .by(r -> {
                // type declarations may occur after use
                DeclarationContext ctx = scope.lookup(node.name);
                DeclarationNode decl = ctx == null ? null : ctx.declaration;

                if (ctx == null)
                    r.errorFor("could not resolve: " + node.name,
                        node,
                        node.attr("value"));

                else if (!isTypeDecl(decl))
                    r.errorFor(format(
                        "%s did not resolve to a type declaration but to a %s declaration",
                        node.name, decl.declaredThing()),
                        node,
                        node.attr("value"));

                else
                    R.rule(node, "value")
                        .using(decl, "declared")
                        .by(Rule::copyFirst);
            });
    }

    private void LambdaType (LambdaTypeNode node)
    {
        final Scope scope = this.scope;

        Attribute[] dependencies = new Attribute[node.parameterTypes.size() + 1];
        dependencies[0] = node.returnType.attr("value");
        forEachIndexed(node.parameterTypes, (i, param) ->
            dependencies[i + 1] = param.attr("value"));

        R.rule(node, "value")
            .using(dependencies)
            .by (r -> {
                Type[] paramTypes = new Type[node.parameterTypes.size()];
                for (int i = 0; i < paramTypes.length; ++i)
                    paramTypes[i] = r.get(i + 1);
                r.set(0, new FunType(r.get(0), paramTypes));
            });

        /*R.rule(node, "value")
            .using(node.returnType.attr("value"))
            .by(r -> r.set(0, new FunType(r.get(0))));
        //R.set(node,"value",);


        /*R.rule()
            .by(r -> {
                // type declarations may occur after use
                DeclarationContext ctx = scope.lookup(node.name);
                DeclarationNode decl = ctx == null ? null : ctx.declaration;

                if (ctx == null)
                    r.errorFor("could not resolve: " + node.name,
                        node,
                        node.attr("value"));

                else if (!isTypeDecl(decl))
                    r.errorFor(format(
                        "%s did not resolve to a type declaration but to a %s declaration",
                        node.name, decl.declaredThing()),
                        node,
                        node.attr("value"));

                else
                    R.rule(node, "value")
                        .using(decl, "declared")
                        .by(Rule::copyFirst);
            });*/
    }

    // ---------------------------------------------------------------------------------------------

    private void arrayType (ArrayTypeNode node)
    {
        R.rule(node, "value")
        .using(node.componentType, "value")
        .by(r -> r.set(0, new ArrayType(r.get(0))));
    }

    private void ListType (ListTypeNode node)
    {
        R.rule(node, "value")
            .using(node.componentType, "value")
            .by(r -> r.set(0, new ListType(r.get(0))));
    }


    // ---------------------------------------------------------------------------------------------

    private static boolean isTypeDecl (DeclarationNode decl)
    {
        if (decl instanceof StructDeclarationNode) return true;
        if (!(decl instanceof SyntheticDeclarationNode)) return false;
        SyntheticDeclarationNode synthetic = cast(decl);
        return synthetic.kind() == DeclarationKind.TYPE;
    }

    // ---------------------------------------------------------------------------------------------

    /**
     * Indicates whether a value of type {@code a} can be assigned to a location (variable,
     * parameter, ...) of type {@code b}.
     */
    private static boolean isAssignableTo (Type a, Type b)
    {
        if (a instanceof VoidType || b instanceof VoidType)
            return false;

        if (a instanceof IntType && b instanceof FloatType)
            return true;

        if (a instanceof ArrayType)
            return b instanceof ArrayType
                && isAssignableTo(((ArrayType)a).componentType, ((ArrayType)b).componentType);

        if (a instanceof ListType) {
            return b instanceof ListType
                && isAssignableTo(((ListType)a).componentType, ((ListType)b).componentType);
        }

        return a instanceof NullType && b.isReference() || a.equals(b);
    }

    private static boolean isCastableTo(Type a, Type b, Object sa) {
        if (a instanceof IntType && b instanceof StringType) {
            return true;
        }
        if (b instanceof IntType && a instanceof StringType){
            try {
                String str  = (String) sa;
                Integer.parseInt(str.substring(1,str.length()-1));
            } catch (NumberFormatException e){
                return false;
            }
            return true;
        }
        return false;
    }

    // ---------------------------------------------------------------------------------------------

    /**
     * Indicate whether the two types are comparable.
     */
    private static boolean isComparableTo (Type a, Type b)
    {
        if (a instanceof VoidType || b instanceof VoidType)
            return false;

        return a.isReference() && b.isReference()
            || a.equals(b)
            || a instanceof IntType && b instanceof FloatType
            || a instanceof FloatType && b instanceof IntType;
    }

    // ---------------------------------------------------------------------------------------------

    /**
     * Returns the common supertype between both types, or {@code null} if no such supertype
     * exists.
     */
    private static Type commonSupertype (Type a, Type b)
    {
        if (a instanceof VoidType || b instanceof VoidType)
            return null;
        if (isAssignableTo(a, b))
            return b;
        if (isAssignableTo(b, a))
            return a;
        else
            return null;
    }

    // endregion
    // =============================================================================================
    // region [Scopes & Declarations]
    // =============================================================================================

    private void popScope (SighNode node) {
        scope = scope.parent;
    }

    // ---------------------------------------------------------------------------------------------

    private void root (RootNode node) {
        assert scope == null;
        scope = new RootScope(node, R);
        R.set(node, "scope", scope);
    }

    // ---------------------------------------------------------------------------------------------

    private void block (BlockNode node) {
        scope = new Scope(node, scope);
        R.set(node, "scope", scope);

        Attribute[] deps = getReturnsDependencies(node.statements);
        R.rule(node, "returns")
        .using(deps)
        .by(r -> r.set(0, deps.length != 0 && Arrays.stream(deps).anyMatch(r::get)));
    }

    // ---------------------------------------------------------------------------------------------

    private void varDecl (VarDeclarationNode node)
    {
        this.inferenceContext = node;

        scope.declare(node.name, node);
        R.set(node, "scope", scope);

        R.rule(node, "type")
        .using(node.type, "value")
        .by(Rule::copyFirst);

        R.rule()
        .using(node.type.attr("value"), node.initializer.attr("type")) // var x: Int = 3
        .by(r -> {
            Type expected = r.get(0);
            Type actual = r.get(1);

            if (!isAssignableTo(actual, expected))
                r.error(format(
                    "incompatible initializer type provided for variable `%s`: expected %s but got %s",
                    node.name, expected, actual),
                    node.initializer);
        });
    }

    private void lambdaDecl(LambdaDeclarationNode node) {

        scope.declare(node.name, node);
        scope = new Scope(node, scope);
        R.set(node, "scope", scope);

        Attribute[] dependencies = new Attribute[node.param.size() + 1];
        dependencies[0] = node.typeNode.attr("value");
        forEachIndexed(node.param, (i, param) ->
            dependencies[i + 1] = param.attr("type"));


        R.rule(node, "type")
            .using(dependencies)
            .by (r -> {
                Type[] paramTypes = new Type[node.param.size()];
                for (int i = 0; i < paramTypes.length; ++i)
                    paramTypes[i] = r.get(i + 1);
                r.set(0, new FunType(r.get(0), paramTypes));
            });



    }


        // ---------------------------------------------------------------------------------------------
    private void varPREDeclCast (VarDeclarationWithCastNode node) {

    }
    private void varDeclCast (VarDeclarationWithCastNode node)
    {
        this.inferenceContext = node;

        scope.declare(node.name, node);
        R.set(node, "scope", scope);
        R.set(node, "cast", node.cast);

        R.rule(node, "type")
            .using(node.type, "value")
            .by(Rule::copyFirst);


        R.rule()
            .using(node.type.attr("value"), node.cast.attr("value")) // Var x: Int = (Int) "3"
            .by(r -> {
                Type expected = r.get(0);
                Type actual = r.get(1);

                if (!isAssignableTo(actual, expected))
                    r.error(format(
                        "incompatible initializer type provided for variable `%s` when casting: expected %s but got %s",
                        node.name, expected, actual),
                        node.initializer);
            });

        /*R.rule(node, "value")
            .using(node.initializer, "value")
            .by(Rule::copyFirst);
        //Attribute expression_value = new Attribute    (node.initializer.contents(), "expression_value");

        /*String expression_value = node.initializer.contents();

        //Attribute[] deps = getFinalExpressionDependencies(node.initializer);
        Object type = R.get(node, "value");

        R.rule()
            .using(node.initializer.attr("type"), node.cast.attr("value"))
            .by(r -> {
                Type expression_type = r.get(0);
                Type cast = r.get(1);

                if (!isAssignableTo(expression_type, cast))  {
                    if(!isCastableTo(expression_type, cast, expression_value)) {
                        r.error(format(
                            "incompatible expression type provided for variable `%s` when casting: %s not castable to %s with %s as value",
                            node.name, expression_type, cast, expression_value),
                            node.initializer);
                    }
                }

            });*/

    }

    private void fieldDecl (FieldDeclarationNode node)
    {
        R.rule(node, "type")
        .using(node.type, "value")
        .by(Rule::copyFirst);
    }

    // ---------------------------------------------------------------------------------------------

    private void parameter (ParameterNode node)
    {
        R.set(node, "scope", scope);
        scope.declare(node.name, node); // scope pushed by FunDeclarationNode

        R.rule(node, "type")
        .using(node.type, "value")
        .by(Rule::copyFirst);

    }


    /*private void param (ParamNode node)
    {
        R.set(node, "scope", scope);
        scope.declare(node.name, node); // scope pushed by LambdaDeclarationNode

        R.set(node, "type", "Int");
    }*/

    // ---------------------------------------------------------------------------------------------

    private void funDecl (FunDeclarationNode node)
    {
        scope.declare(node.name, node);
        scope = new Scope(node, scope);
        R.set(node, "scope", scope);

        Attribute[] dependencies = new Attribute[node.parameters.size() + 1];
        dependencies[0] = node.returnType.attr("value");
        forEachIndexed(node.parameters, (i, param) ->
            dependencies[i + 1] = param.attr("type"));

        R.rule(node, "type")
        .using(dependencies)
        .by (r -> {
            Type[] paramTypes = new Type[node.parameters.size()];
            for (int i = 0; i < paramTypes.length; ++i)
                paramTypes[i] = r.get(i + 1);
            r.set(0, new FunType(r.get(0), paramTypes));
        });

        R.rule()
        .using(node.block.attr("returns"), node.returnType.attr("value"))
        .by(r -> {
            boolean returns = r.get(0);
            Type returnType = r.get(1);
            if (!returns && !(returnType instanceof VoidType))
                r.error("Missing return in function.", node);
            // NOTE: The returned value presence & type is checked in returnStmt().
        });
    }

    // ---------------------------------------------------------------------------------------------

    private void structDecl (StructDeclarationNode node) {
        scope.declare(node.name, node);
        R.set(node, "type", TypeType.INSTANCE);
        R.set(node, "declared", new StructType(node));
    }

    // endregion
    // =============================================================================================
    // region [Other Statements]
    // =============================================================================================

    private void ifStmt (IfNode node) {
        R.rule()
        .using(node.condition, "type")
        .by(r -> {
            Type type = r.get(0);
            if (!(type instanceof BoolType)) {
                r.error("If statement with a non-boolean condition of type: " + type,
                    node.condition);
            }
        });

        Attribute[] deps = getReturnsDependencies(list(node.trueStatement, node.falseStatement));
        R.rule(node, "returns")
        .using(deps)
        .by(r -> r.set(0, deps.length == 2 && Arrays.stream(deps).allMatch(r::get)));
    }

    // ---------------------------------------------------------------------------------------------

    private void switchStmt (SwitchNode node) {

        Attribute[] deps = getSwitchValuesDependencies(node.switch_block.statements);
        R.rule()
            .using(node.identifier, "type")
            .by(r -> {
                Type type = r.get(0);
                for (int i = 0; i<deps.length; i++) {
                    if (type != R.get(deps[i])) {
                        r.error(format("Type of %s (%s) does not match proposed type:%s", node.identifier.contents(), R.get(deps[i]).toString(), type.toString()), node);
                    }
                }
            });

    }

    private void switchPREStmt (SwitchNode node) {

    }


    private void switchBlock(SwitchBlockNode node){
    }

    private void switchElseStmt(SwitchElseNode node){
    }

    private void switchValueStmt (SwitchValueNode node) {

        R.set(node, "type", R.get(node.basic_switch_value, "type"));

    }

    private void switchValuePREStmt (SwitchValueNode node) {
    }
    // ---------------------------------------------------------------------------------------------
    private void whileStmt (WhileNode node) {
        R.rule()
        .using(node.condition, "type")
        .by(r -> {
            Type type = r.get(0);
            if (!(type instanceof BoolType)) {
                r.error("While statement with a non-boolean condition of type: " + type,
                    node.condition);
            }
        });
    }

    // ---------------------------------------------------------------------------------------------

    private void returnStmt (ReturnNode node)
    {
        R.set(node, "returns", true);

        FunDeclarationNode function = currentFunction();
        LambdaDeclarationNode lambda = currentLambda();


        if (function == null && lambda == null) // top-level return
            return;

        if (function != null) {
            if (node.expression == null)
                R.rule()
                    .using(function.returnType, "value")
                    .by(r -> {
                        Type returnType = r.get(0);
                        if (!(returnType instanceof VoidType))
                            r.error("Return without value in a function with a return type.", node);
                    });
            else
                R.rule()
                    .using(function.returnType.attr("value"), node.expression.attr("type"))
                    .by(r -> {
                        Type formal = r.get(0);
                        Type actual = r.get(1);
                        if (formal instanceof VoidType)
                            r.error("Return with value in a Void function.", node);
                        else if (!isAssignableTo(actual, formal)) {
                            r.errorFor(format(
                                "Incompatible return type, expected %s but got %s", formal, actual),
                                node.expression);
                        }
                    });

        }
    }

    private void lambdaReturnStmt (LambdaReturnNode node)
    {
        R.set(node, "returns", true);

        LambdaDeclarationNode lambda = currentLambda();


        if (lambda == null) // top-level return
            return;

        if (node.expression == null)
            R.rule()
                .using(lambda.typeNode, "value")
                .by(r -> {
                    Type returnType = r.get(0);
                    if (!(returnType instanceof VoidType))
                        r.error("Return without value in a function with a return type.", node);
                });
        else
            R.rule()
                .using(lambda.typeNode.attr("value"), node.expression.attr("type"))
                .by(r -> {
                    Type formal = r.get(0);
                    Type actual = r.get(1);
                    if (formal instanceof VoidType)
                        r.error("Return with value in a Void function.", node);
                    else if (!isAssignableTo(actual, formal)) {
                        r.errorFor(format(
                            "Incompatible return type, expected %s but got %s", formal, actual),
                            node.expression);
                    }
                });


    }

    // ---------------------------------------------------------------------------------------------

    private FunDeclarationNode currentFunction()
    {
        Scope scope = this.scope;
        while (scope != null) {
            SighNode node = scope.node;
            if (node instanceof FunDeclarationNode)
                return (FunDeclarationNode) node;
            scope = scope.parent;
        }
        return null;
    }

    private LambdaDeclarationNode currentLambda()
    {
        Scope scope = this.scope;
        while (scope != null) {
            SighNode node = scope.node;
            if (node instanceof LambdaDeclarationNode)
                return (LambdaDeclarationNode) node;
            scope = scope.parent;
        }
        return null;
    }

    // ---------------------------------------------------------------------------------------------

    private boolean isReturnContainer (SighNode node) {
        return node instanceof BlockNode
            || node instanceof IfNode
            || node instanceof ReturnNode;
    }

    // ---------------------------------------------------------------------------------------------

    /** Get the depedencies necessary to compute the "returns" attribute of the parent. */
    private Attribute[] getReturnsDependencies (List<? extends SighNode> children) {
        return children.stream()
            .filter(Objects::nonNull)
            .filter(this::isReturnContainer)
            .map(it -> it.attr("returns"))
            .toArray(Attribute[]::new);
    }
    private boolean isSwitchValue (SighNode node) {
        return node instanceof SwitchValueNode;
    }
    private Attribute[] getSwitchValuesDependencies (List<? extends SighNode> children) {
        return children.stream()
            .filter(Objects::nonNull)
            .filter(this::isSwitchValue)
            .map(it -> it.attr("type"))
            .toArray(Attribute[]::new);
    }

    private boolean isFinalValue (SighNode node) {
        return node instanceof StringLiteralNode
            || node instanceof IntLiteralNode
            || node instanceof FloatLiteralNode;
    }

    private Attribute[] getFinalExpressionDependencies (List<? extends SighNode> children) {
        return children.stream()
            .filter(Objects::nonNull)
            .filter(this::isFinalValue)
            .map(it -> it.attr("value"))
            .toArray(Attribute[]::new);
    }

    // endregion
    // =============================================================================================
}