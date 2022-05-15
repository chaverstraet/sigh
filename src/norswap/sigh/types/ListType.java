package norswap.sigh.types;

public final class ListType extends Type
{
    public final Type componentType;

    public ListType (Type componentType) {
        this.componentType = componentType;
    }

    @Override public String name() {
        return componentType.toString() + "{}";
    }

    @Override public boolean equals (Object o) {
        return this == o || o instanceof ListType && componentType.equals(o);
    }

    @Override public int hashCode () {
        return componentType.hashCode();
    }
}
