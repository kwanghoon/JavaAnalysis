
public interface IVisitor {
    void close(final Class class1);
    void close(final CompilationUnit compilationUnit);
    void close(final Method method);
    void close(final StatementCompound statementCompound);
    void close(final StatementIf aStatementIf);
    void open(final Class class1);
    void open(final CompilationUnit compilationUnit);
    void open(final Method method);
    void open(final StatementCompound statementCompound);
    void open(final StatementIf aStatementIf);
    void visit(final Field field);
    void visit(final StatementSimple statementEmpty);
}
