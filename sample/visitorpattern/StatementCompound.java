
class StatementCompound extends Statement {
    Set statements; // = new HashSet();

    void initStatements() {
	statements = new HashSet();
    }

	// StatementCompound(String aName) {
	// 	super(aName);
	// }

	void addStatement(Statement aStatement) {
		this.statements.add(aStatement);
	}
	void removeStatement(Statement aStatement) {
		this.statements.remove(aStatement);
	}
	Statement getStatement(String aName) {
		Iterator iterator = this.statements.iterator();
		while (iterator.hasNext()) {
			Statement statement = (Statement) iterator.next();
			if (statement.getName().equals(aName)) {
				return statement;
			}
		}
		return null;
	}
	Iterator getStatements() {
		return this.statements.iterator();
	}
	void accept(IVisitor aVisitor) {
		aVisitor.open(this);
		Iterator iterator = this.statements.iterator();
		while (iterator.hasNext()) {
			Statement statement = (Statement) iterator.next();
			statement.accept(aVisitor);
		}
		aVisitor.close(this);
	}
}