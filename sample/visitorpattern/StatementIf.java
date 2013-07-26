
class StatementIf extends StatementCompound {
	// StatementIf(String aName) {
	// 	super(aName);
	// }
	/**
	 * Some particular behaviour.
	 */
	void accept(IVisitor aVisitor) {
		aVisitor.open(this);
		Iterator iterator = this.getStatements();
		while (iterator.hasNext()) {
			Statement statement = (Statement) iterator.next();
			statement.accept(aVisitor);
		}
		aVisitor.close(this);
	}
}
