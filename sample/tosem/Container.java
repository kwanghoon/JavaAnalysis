
class Container {
    Object[] data;
    Container(int size) {
	Object[] data_tmp = new Object[size];
	this.data = data_tmp;
    }
    void put(Object e, int at) {
	Object[] data_tmp = this.data;
	data_tmp[at] = e;
    }
    Object get(int at) {
	Object[] data_tmp = this.data;
	return data_tmp[at];
    }
}

class X {
}

class Y {
}

class Main {
    void main(String[] args) {
	Container c1 = new Container (100);
	Container c2 = new Container (200);

	X x = new X();
	c1.put(x, 0);

	Y y = new Y();
	c2.put(y, 1);
    }
}