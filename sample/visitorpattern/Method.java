
public class Method extends AbstractNode {
    private Set statements = new HashSet();

    public Method(final String aName) {
    	super(aName);
    }
    public void addStatement(final Statement aStatement) {
	this.statements.add(aStatement);
    }
    public void removeStatement(final Statement aStatement) {
	this.statements.remove(aStatement);
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
