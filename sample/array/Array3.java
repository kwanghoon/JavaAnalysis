
class A {
    Object[] brr;
    A(Object[] brr) {
	this.brr = brr;
    }
}

public class ArrayTest {
    public static void main (String[] args) {
	Object[] arr;

	arr = new Object[100];
	arr[0] = "aaaaaa";
	arr[1] = "bbbbbb";

	A a = new A(arr);
    }
}
