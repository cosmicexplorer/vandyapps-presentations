import java.io.*;
import java.net.*;

class TCPPinger implements Pinger {
  public String Ping() {
    Socket client = null;
    try {
      client = new Socket("localhost", 8080);
    } catch (UnknownHostException e) {
      e.printStackTrace();
      System.exit(-1);
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(-1);
    }
    BufferedReader fromServer = null;
    try {
      fromServer =
          new BufferedReader(new InputStreamReader(client.getInputStream()));
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(-1);
    }
    DataOutputStream outToServer = null;
    String res = null;
    try {
      outToServer = new DataOutputStream(client.getOutputStream());
      outToServer.writeBytes("hey\n");
      res = fromServer.readLine();
      client.close();
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(-1);
    }
    return res;
  }
}
