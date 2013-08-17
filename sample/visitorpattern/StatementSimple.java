
public class StatementSimple extends Statement {
	public StatementSimple(final String aName) {
	 	super(aName);
	 }
	public void accept(final IVisitor aVisitor) {
		aVisitor.visit(this);
	}
}
