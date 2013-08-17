
public class Client {
	public static void main(final String[] args) {
		final Compiler compiler = new Compiler();
		final CompilationUnit compilationUnit = compiler.compile();

		// Changed: compilationUnit.accept(new PrettyPrinter());
		PrettyPrinter pp = new PrettyPrinter();
		compilationUnit.accept(pp);
	}
}
