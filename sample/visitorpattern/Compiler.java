
class Compiler {
    CompilationUnit compile() {
	// For example
	CompilationUnit compilationUnit = new CompilationUnit("A.java");

	Class classA = new Class("A");
	compilationUnit.addClass(classA);

	Method methodFoo = new Method("foo");
	classA.addMethod(methodFoo);
	{
		Statement statement;
		statement = new StatementSimple("Simple 1");
		methodFoo.addStatement(statement);
		statement = new StatementSimple("Simple 2");
		methodFoo.addStatement(statement);
		statement = new StatementIf("If 1");
		methodFoo.addStatement(statement);
		statement = new StatementIf("If 2");
		methodFoo.addStatement(statement);
		statement = new StatementSimple("Simple 3");
		methodFoo.addStatement(statement);
	}

	Method methodBar = new Method("bar");
	classA.addMethod(methodBar);

	Field fieldF = new Field("f");
	classA.addField(fieldF);

	return compilationUnit;
    }
}