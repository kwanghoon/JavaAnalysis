
class X {
    void n() { }
}

class Y extends X {
    void n() { }
}

class Z extends X {
    void n() { }
}

class A {
    X f;
    A(X xa) { this.f = xa; }
}

class B extends A {
    B(X xb) {
	super(xb);
    }

    void m() {
	X xb = this.f;
	xb.n();
    }
}

class C extends A {
    C(X xc) {
	super(xc);
    }

    void m() {
	X xc = this.f;
	xc.n();
    }
}

class Main {
    void main(String[] args) {
	Y y = new Y();
	Z z = new Z();
	B b = new B(y);
	C c = new C(z);

	b.m();
	c.m();
    }
}