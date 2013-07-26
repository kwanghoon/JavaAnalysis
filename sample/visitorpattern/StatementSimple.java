
class StatementSimple extends Statement {
	// StatementSimple(fString aName) {
	// 	super(aName);
	// }
	void accept(IVisitor aVisitor) {
		aVisitor.visit(this);
	}
}
