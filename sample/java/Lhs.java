
public class Lhs {
    public static void main(String[] args) {
	B b = new B();
	b.me().b = new B();
	b.me().i = 0;
	//b.me() = new B();   // This is wrong because b.me() is a value.
    }
}

class B {
    public B b;
    public int i;

    public B me() {
	return this;
    }
}

