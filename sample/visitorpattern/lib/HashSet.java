
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
