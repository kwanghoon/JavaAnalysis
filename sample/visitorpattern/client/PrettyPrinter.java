
// Several things left to do!!

class PrettyPrinter extends VisitorAdapter {
	int temporaryIndentation;
	StringBuilder temporaryOutput = new StringBuilder();

	void close(AbstractNode aNode) {
		this.temporaryOutput.setLength(0);
		for (int i = 0; i < this.temporaryIndentation; i++) {
			this.temporaryOutput.append('\t');
		}
		this.temporaryOutput.append("(Closed)");

		System.out.println(this.temporaryOutput.toString()); // Added .toString
	}
	void close(Class aClass) {
		this.close((AbstractNode) aClass);
		this.temporaryIndentation--;
	}
	void close(CompilationUnit aCompilationUnit) {
		this.close((AbstractNode) aCompilationUnit);
		this.temporaryIndentation--;
	}
	void close(Method aMethod) {
		this.close((AbstractNode) aMethod);
		this.temporaryIndentation--;
	}
	void close(StatementCompound aStatementCompound) {
		this.close((AbstractNode) aStatementCompound);
		this.temporaryIndentation--;
	}
	void close(StatementIf aStatementIf) {
		this.close((AbstractNode) aStatementIf);
		this.temporaryIndentation--;
	}
	private void open(AbstractNode aNode) {
		this.open(aNode, null);
	}
	private void open(AbstractNode aNode, String anAdditionalMessage) {
		this.temporaryOutput.setLength(0);
		for (int i = 0; i < this.temporaryIndentation; i++) {
			this.temporaryOutput.append('\t');
		}
		this.temporaryOutput.append("Visiting ");
		this.temporaryOutput.append(aNode.getName());
		this.temporaryOutput.append(" of type ");
		CompilationUnit aCompilationUnit = (CompilationUnit)aNode; // Added this statement
		this.temporaryOutput.append(aCompilationUnit.getClass("").getName()); // Modified aNode to be aCompilationUnit, and added ""
		if (anAdditionalMessage != null) {
			this.temporaryOutput.append(anAdditionalMessage);
		}

		System.out.println(this.temporaryOutput.toString()); // Added .toString()
	}
	void open(Class aClass) {
		this.open((AbstractNode) aClass);
		this.temporaryIndentation++;
	}
	void open(CompilationUnit aCompilationUnit) {
		this.open((AbstractNode) aCompilationUnit);
		this.temporaryIndentation++;
	}
	void open(Method aMethod) {
		this.open((AbstractNode) aMethod);
		this.temporaryIndentation++;
	}
	void open(StatementCompound aStatementCompound) {
		this.open((AbstractNode) aStatementCompound);
		this.temporaryIndentation++;
	}
	void open(StatementIf aStatementIf) {
		this.open((AbstractNode) aStatementIf, " (this is open(StatementIf))");
		this.temporaryIndentation++;
	}
	void visit(Field aField) {
		this.open((AbstractNode) aField);
	}
	void visit(StatementSimple aStatementEmpty) {
		this.open((AbstractNode) aStatementEmpty);
	}
}
