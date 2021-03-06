
public abstract class StatementCompound extends Statement {
    private final Set statements = new HashSet();

    public StatementCompound(final String aName) {
	super(aName);
    }

    public void addStatement(final Statement aStatement) {
	this.statements.add(aStatement);
    }
    public void removeStatement(final Statement aStatement) {
	this.statements.remove(aStatement);
    }
    public Statement getStatement(final String aName) {
	final Iterator iterator = this.statements.iterator();
	while (iterator.hasNext()) {
		final Statement statement = (Statement) iterator.next();
		if (statement.getName().equals(aName)) {
			return statement;
		}
	}
	return null;
    }
    public Iterator getStatements() {
	return this.statements.iterator();
    }
    public void accept(final IVisitor aVisitor) {
	aVisitor.open(this);
	final Iterator iterator = this.statements.iterator();
	while (iterator.hasNext()) {
		final Statement statement = (Statement) iterator.next();
		statement.accept(aVisitor);
	}
	aVisitor.close(this);
    }
}