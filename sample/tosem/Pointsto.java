
class Y {
}

class X {
    Y f;

    void set(Y r) {
	this.f = r;
    }

    public static void main() {
	X p = new X();
	Y q = new Y();
	p.set(q);
    }
}

