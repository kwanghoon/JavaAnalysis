
class Client {
	void main() {
		Compiler compiler = new Compiler();
		CompilationUnit compilationUnit = compiler.compile();
		PrettyPrinter pp = new PrettyPrinter();
		compilationUnit.accept(pp);
	}
}
