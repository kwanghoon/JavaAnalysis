
class AbstractNode implements IVisitable {
    String name;
    AbstractNode(String aName) {
 	this.name = aName;
    }

    void setName(String aName) {
	this.name = aName;
    }

    String getName() {
	return this.name;
    }
}
