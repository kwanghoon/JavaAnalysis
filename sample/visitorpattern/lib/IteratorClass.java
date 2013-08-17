
public class IteratorClass implements Iterator {
    private Object[] obj;
    private int index;

    IteratorClass (Object[] aObj) {
	this.obj = aObj;
	this.index = 0;
    }
    public boolean hasNext() {
	if (index < obj.length) {
	    return true;
	}
	else {
	    return false;
	}
    }

    public Object next() {
	Object ret = obj[index];
	index++;
	return ret;
    }

    public void remove() {
	// do nothing. just for type checking
    }

}

