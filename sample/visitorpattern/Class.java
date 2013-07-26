
// Need to support while, HashSet, Iterator!

class Class extends AbstractNode {
    Set methods = new HashSet();
    Set fields  = new HashSet();

    void initMethodsFields() {
	methods = new HashSet();
	fields = new HashSet();
    }

    Class(String aName) {
     	super(aName);
    }

    void addMethod(Method aMethod) {
	this.methods.add(aMethod);
    }
    void removeMethod(Method aMethod) {
	this.methods.remove(aMethod);
    }
    void addField(Field aField) {
	this.fields.add(aField);
    }
    void removeField(Field aField) {
	this.fields.remove(aField);
    }
    void accept(IVisitor aVisitor) {
	aVisitor.open(this);
	Iterator iteratorOnFields = this.fields.iterator();
	while (iteratorOnFields.hasNext()) {
		Field field = (Field) iteratorOnFields.next();
		field.accept(aVisitor);
	}
	Iterator iteratorOnMethods = this.methods.iterator();
	while (iteratorOnMethods.hasNext()) {
		Method method = (Method) iteratorOnMethods.next();
		method.accept(aVisitor);
	}
	aVisitor.close(this);
    }
}
