
interface IVisitor {
    void close(Class class1);
    void close(CompilationUnit compilationUnit);
    void close(Method method);
    void close(StatementCompound statementCompound);
    void close(StatementIf aStatementIf);
    void open(Class class1);
    void open(CompilationUnit compilationUnit);
    void open(Method method);
    void open(StatementCompound statementCompound);
    void open(StatementIf aStatementIf);
    void visit(Field field);
    void visit(StatementSimple statementEmpty);
}
