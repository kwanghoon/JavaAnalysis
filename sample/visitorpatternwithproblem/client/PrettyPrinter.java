
// TODO: Need to support Object.getClass(), Class.getName()

public class PrettyPrinter extends VisitorAdapter {
	private int temporaryIndentation;
	private final StringBuilder temporaryOutput = new StringBuilder();

	private void close(final AbstractNode aNode) {
		this.temporaryOutput.setLength(0);
		for (int i = 0; i < this.temporaryIndentation; i++) {
			this.temporaryOutput.append('\t');
		}
		this.temporaryOutput.append("(Closed)");

		System.out.println(this.temporaryOutput);
	}
	public void close(final Class aClass) {
		this.close((AbstractNode) aClass);
		this.temporaryIndentation--;
	}
	public void close(final CompilationUnit aCompilationUnit) {
		this.close((AbstractNode) aCompilationUnit);
		this.temporaryIndentation--;
	}
	public void close(final Method aMethod) {
		this.close((AbstractNode) aMethod);
		this.temporaryIndentation--;
	}
	public void close(final StatementCompound aStatementCompound) {
		this.close((AbstractNode) aStatementCompound);
		this.temporaryIndentation--;
	}
	public void close(final StatementIf aStatementIf) {
		this.close((AbstractNode) aStatementIf);
		this.temporaryIndentation--;
	}
	private void open(final AbstractNode aNode) {
		this.open(aNode, null);
	}
	private void open(final AbstractNode aNode, final String anAdditionalMessage) {
		this.temporaryOutput.setLength(0);
		for (int i = 0; i < this.temporaryIndentation; i++) {
			this.temporaryOutput.append('\t');
		}
		this.temporaryOutput.append("Visiting ");
		this.temporaryOutput.append(aNode.getName());
		this.temporaryOutput.append(" of type ");
		this.temporaryOutput.append(aNode.getClass().getName());
		if (anAdditionalMessage != null) {
			this.temporaryOutput.append(anAdditionalMessage);
		}

		System.out.println(this.temporaryOutput);
	}
	public void open(final Class aClass) {
		this.open((AbstractNode) aClass);
		this.temporaryIndentation++;
	}
	public void open(final CompilationUnit aCompilationUnit) {
		this.open((AbstractNode) aCompilationUnit);
		this.temporaryIndentation++;
	}
	public void open(final Method aMethod) {
		this.open((AbstractNode) aMethod);
		this.temporaryIndentation++;
	}
	public void open(final StatementCompound aStatementCompound) {
		this.open((AbstractNode) aStatementCompound);
		this.temporaryIndentation++;
	}
	public void open(final StatementIf aStatementIf) {
		this.open((AbstractNode) aStatementIf, " (this is open(StatementIf))");
		this.temporaryIndentation++;
	}
	public void visit(final Field aField) {
		this.open((AbstractNode) aField);
	}
	public void visit(final StatementSimple aStatementEmpty) {
		this.open((AbstractNode) aStatementEmpty);
	}
}
