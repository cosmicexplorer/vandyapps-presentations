import java.io.*;
import java.net.*;

class UDPPinger implements Pinger {
  public String Ping() {
    DatagramSocket client = null;
    try {
      client = new DatagramSocket();
    } catch (SocketException e) {
      e.printStackTrace();
      System.exit(-1);
    }
    InetAddress ip = null;
    try {
      ip = InetAddress.getByName("localhost");
    } catch (UnknownHostException e) {
      e.printStackTrace();
      System.exit(-1);
    }
    byte[] sendData = new String("hey\n").getBytes();
    byte[] recvData = new byte[1024];
    DatagramPacket sendP =
        new DatagramPacket(sendData, sendData.length, ip, 8081);
    DatagramPacket recvP = new DatagramPacket(recvData, recvData.length);
    String res = null;
    try {
      client.send(sendP);
      client.receive(recvP);
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(-1);
    }
    recvData = recvP.getData();
    client.close();
    return new String(recvData);
  }
}
