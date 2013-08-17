
public class Field extends AbstractNode {
	public Field(final String aName) {
		super(aName);
	}

	public void accept(final IVisitor aVisitor) {
		aVisitor.visit(this);
	}

	/* To be implemented. */
}