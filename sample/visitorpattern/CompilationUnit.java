
class CompilationUnit extends AbstractNode {
    private Set classes = new HashSet();

    void initClasses() {
	classes = new HashSet();
    }

    CompilationUnit(String aName) {
     	super(aName);
    }
    void addClass(Class aClass) {
	this.classes.add(aClass);
    }
    void removeClass(Class aClass) {
	this.classes.remove(aClass);
    }
    Class getClass(String aName) {
	Iterator iterator = this.classes.iterator();
	while (iterator.hasNext()) {
		Class aClass = (Class) iterator.next();
		if (aClass.getName().equals(aName)) {
			return aClass;
		}
	}
	return null;
    }
    void accept(IVisitor aVisitor) {
	aVisitor.open(this);
	Iterator iterator = this.classes.iterator();
	while (iterator.hasNext()) {
		Class aClass = (Class) iterator.next();
		aClass.accept(aVisitor);
	}
	aVisitor.close(this);
    }
}
