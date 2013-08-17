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
package kr.ac.yonsei.it.cse3009.ast.visitor;

import kr.ac.yonsei.it.cse3009.ast.Class;
import kr.ac.yonsei.it.cse3009.ast.CompilationUnit;
import kr.ac.yonsei.it.cse3009.ast.Field;
import kr.ac.yonsei.it.cse3009.ast.Method;
import kr.ac.yonsei.it.cse3009.ast.StatementCompound;
import kr.ac.yonsei.it.cse3009.ast.StatementIf;
import kr.ac.yonsei.it.cse3009.ast.StatementSimple;

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
