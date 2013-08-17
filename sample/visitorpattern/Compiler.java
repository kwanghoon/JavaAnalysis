
public class Compiler {
    public CompilationUnit compile() {
	// For example
	final CompilationUnit compilationUnit = new CompilationUnit("A.java");

	final Class classA = new Class("A");
	compilationUnit.addClass(classA);

	final Method methodFoo = new Method("foo");
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

	final Method methodBar = new Method("bar");
	classA.addMethod(methodBar);

	final Field fieldF = new Field("f");
	classA.addField(fieldF);

	return compilationUnit;
    }
}