
class Elem {
}

class List {
    List copy() {
	return new List(); 
    }
}

class Nil extends List {
    Nil copy() {
	return new Nil();
    }
}

class Cons extends List {
    Elem elem;
    List next;
    Cons copy() {
	Cons res = new Cons();
	res.elem = this.elem;

	List l = this.next; // 	res.next = this.next.copy();
	res.next = l.copy();

	return res;
    }
}

