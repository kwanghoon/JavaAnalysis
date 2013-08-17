
// TODO: Need to support HashSet and Iterator.

public class Class extends AbstractNode {
    private final Set methods = new HashSet();
    private Set fields  = new HashSet();

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
