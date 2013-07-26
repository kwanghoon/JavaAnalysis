/* (c) Copyright 2001 and following years, Yann-Ga�l Gu�h�neuc,
 * University of Montreal.
 * 
 * Use and copying of this software and preparation of derivative works
 * based upon this software are permitted. Any copy of this software or
 * of any derivative work must include the above copyright notice of
 * the author, this paragraph and the one after it.
 * 
 * This software is made available AS IS, and THE AUTHOR DISCLAIMS
 * ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, AND NOT WITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN,
 * ANY LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
 * EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
 * NEGLIGENCE) OR STRICT LIABILITY, EVEN IF THE AUTHOR IS ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGES.
 * 
 * All Rights Reserved.
 */
package kr.ac.yonsei.it.cse3009.ast;

import java.util.Iterator;
import kr.ac.yonsei.it.cse3009.ast.visitor.IVisitor;

public class StatementIf extends StatementCompound {
	public StatementIf(final String aName) {
		super(aName);
	}
	/**
	 * Some particular behaviour.
	 */
	public void accept(final IVisitor aVisitor) {
		aVisitor.open(this);
		final Iterator iterator = this.getStatements();
		while (iterator.hasNext()) {
			final Statement statement = (Statement) iterator.next();
			statement.accept(aVisitor);
		}
		aVisitor.close(this);
	}
}
