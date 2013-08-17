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

public class Class extends AbstractNode {
	private final Set methods = new HashSet();
	private Set fields = new HashSet();

	public Class(final String aName) {
		super(aName);
	}
	public void addMethod(final Method aMethod) {
		this.methods.add(aMethod);
	}
	public void removeMethod(final Method aMethod) {
		this.methods.remove(aMethod);
	}
	public void addField(final Field aField) {
		this.fields.add(aField);
	}
	public void removeField(final Field aField) {
		this.fields.remove(aField);
	}
	public void accept(final IVisitor aVisitor) {
		aVisitor.open(this);
		final Iterator iteratorOnFields = this.fields.iterator();
		while (iteratorOnFields.hasNext()) {
			final Field field = (Field) iteratorOnFields.next();
			field.accept(aVisitor);
		}
		final Iterator iteratorOnMethods = this.methods.iterator();
		while (iteratorOnMethods.hasNext()) {
			final Method method = (Method) iteratorOnMethods.next();
			method.accept(aVisitor);
		}
		aVisitor.close(this);
	}
}
