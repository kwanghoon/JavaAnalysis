
class Method extends AbstractNode {
    private Set statements; // = new HashSet();
    void initStatements() {
	statements = new HashSet();
    }

    // Method(String aName) {
    // 	super(aName);
    // }
    void addStatement(Statement aStatement) {
	this.statements.add(aStatement);
    }
    void removeStatement(Statement aStatement) {
	this.statements.remove(aStatement);
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
