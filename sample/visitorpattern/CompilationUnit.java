
// TODO: Need to support HashSet and Iterator

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
