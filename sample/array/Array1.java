

public class ArrayAssign {
    public static void main(String[] args) {
	Set set = new HashSet();
	String x = "abc";
	String y = "abc";

	set.add(x);
	set.add(y);

	Iterator i = set.iterator();

	Object o = i.next();
    }
}


public interface Set {
    public boolean add(Object e);
    public boolean remove(Object o);
    public Iterator iterator();
}


public class HashSet implements Set {
    private Object[] data;
    private int index;

    public HashSet() {
	this.data  = new Object[100];
	this.index = 0;
    }

    public boolean add(Object o) {
	data[index] = o;
	index++;
	return true;
    }

    public boolean remove(Object o) {
	// do nothing. just for type checking
	return true;
    }

    public Iterator iterator() {
	IteratorClass i = new IteratorClass( data );
	return i;
    }
}


public interface Iterator {
    public boolean hasNext();
    public Object next();
    public void remove();
}



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


