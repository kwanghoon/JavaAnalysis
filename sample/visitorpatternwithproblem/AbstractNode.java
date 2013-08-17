
public abstract class AbstractNode implements IVisitable {
    private final String name;
    public AbstractNode(final String aName) {
 	this.name = aName;
    }

    public String getName() {
	return this.name;
    }
}
