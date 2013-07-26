package kr.ac.yonsei.it.cse3009.ast.visitor;

public interface IVisitable {
	void accept(final IVisitor aVisitor);
}
