
public class StatementIf extends StatementCompound {
	public StatementIf(final String aName) {
	 	super(aName);
	}
	/**
	 * Some particular behaviour.
	 */
	public void accept(final IVisitor aVisitor) {
		aVisitor.open(this);
		final Iterator iterator = this.getStatements();
		while (iterator.hasNext()) {
			final Statement statement = (Statement) iterator.next();
			statement.accept(aVisitor);
		}
		aVisitor.close(this);
	}
}
