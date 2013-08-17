/* (c) Copyright 2001 and following years, Yann-Gaël Guéhéneuc,
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

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import kr.ac.yonsei.it.cse3009.ast.visitor.IVisitor;

public class CompilationUnit extends AbstractNode {
	private Set classes = new HashSet();

	public CompilationUnit(final String aName) {
		super(aName);
	}
	public void addClass(final Class aClass) {
		this.classes.add(aClass);
	}
	public void removeClass(final Class aClass) {
		this.classes.remove(aClass);
	}
	public Class getClass(final String aName) {
		final Iterator iterator = this.classes.iterator();
		while (iterator.hasNext()) {
			final Class aClass = (Class) iterator.next();
			if (aClass.getName().equals(aName)) {
				return aClass;
			}
		}
		return null;
	}
	public void accept(final IVisitor aVisitor) {
		aVisitor.open(this);
		final Iterator iterator = this.classes.iterator();
		while (iterator.hasNext()) {
			final Class aClass = (Class) iterator.next();
			aClass.accept(aVisitor);
		}
		aVisitor.close(this);
	}
}
