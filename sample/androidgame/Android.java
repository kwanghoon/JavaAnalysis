
/* Android Library
 */

class Activity {
    Intent intent;
    Intent getIntent() { return this.intent; }
    void onCreate() { }
    void onClick(int button) { }
    void addButton(int button) { primAddButton(button); }
    void startActivity(Intent i) { primStartActivity(i); }
}

class Intent {
    String target;
    Object data;
    void setTarget(String s) { this.target = s; }
    void setData(Object d) { this.data = d; }
    Object getData() { return this.data; }
}

