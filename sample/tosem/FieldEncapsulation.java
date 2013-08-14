
class X {
}

class Y {
    X f;
    void set(X x) { this.f = x; }
}

class Main {
    public static void main(String[] args) {
	X x1 = new X();
	X x2 = new X();
	Y y1 = new Y();
	Y y2 = new Y();

	y1.set(x1);
	y2.set(x2);
    }
}
