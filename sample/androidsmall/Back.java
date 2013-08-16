class Main extends Activity {
    void onCreate() {
	this.addButton(1);
	this.addButton(2);
	this.addButton(3);
    }
    void onClick(int button) {
	if (button == 1) {
//L4:
	    Intent i = new Intent();
//L5:
	    i.setTarget("Game");
//L6:
	    this.startActivity(i);
	} else	if (button == 3) {
//L10:
	    Intent i = new Intent();
//L11:
	    i.setTarget("Help");
//L12:
	    i.setData("Main");
//L13:
	    this.startActivity(i);
	} else {
	}
    }
}

class Game extends Activity {
    void onCreate() {
	this.addButton(1);
	this.addButton(2);
	this.addButton(3);
    }
    void onClick(int button) {
	if (button == 1) {
//L14:
	    Intent i = new Intent();
//L15:
	    i.setTarget("Help");
//L16:
	    i.setData("Game");
//L17:
	    this.startActivity(i);
	} else {
	}
    }
    
}

class Help extends Activity {
    void onCreate() {
	this.addButton(1);
    }
    void onClick(int button) {
//L21:
	Intent i = new Intent();
//L22:
	Intent j = this.getIntent();
        Object o = j.getData();
	String t = (String)o;
	i.setTarget(t);
//L23:
	this.startActivity(i);
    }
}

