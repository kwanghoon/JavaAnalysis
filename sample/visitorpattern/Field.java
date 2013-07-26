
class Field extends AbstractNode {
	// Field(String aName) {
	// 	super(aName);
	// }

	void accept(IVisitor aVisitor) {
		aVisitor.visit(this);
	}

	/* To be implemented. */
}