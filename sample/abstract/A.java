package kr.ac.yonsei.it.cse3009.ast;

import kr.ac.yonsei.it.cse3009.ast.visitor.IVisitable;

public abstract class AbstractNode implements IVisitable {
	private final String name;
	public AbstractNode(final String aName) {
		this.name = aName;
	}
	public String getName() {
		return this.name;
	}
}
