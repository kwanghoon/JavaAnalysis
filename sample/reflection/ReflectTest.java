
import java.lang.reflect.*;

class ReflectTest {
    public static void main(String[] args) {
	try {
	    Class cls = Class.forName("java.util.HashSet");
	    String name = cls.getName();
	    System.out.println("Class : " + name);

	    Method[] method = cls.getDeclaredMethods();
	    System.out.println("Method : ");
	    for (Method i : method)
		System.out.println(" " + i);
	}
	catch (Throwable e) {
	    System.err.print(e);
	}
    }
}
