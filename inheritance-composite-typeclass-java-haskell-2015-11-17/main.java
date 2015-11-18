// -*- compile-command: "javac *.java" -*-
// run with:
// java main tcp
// java main udp

import java.net.*;

public class main {
  public static PingResult MakeResult(String msg) {
    msg = msg.replaceAll("\n.*$", "");
    if (msg.startsWith("Success")) {
      return new SuccessPingResult(msg.replaceAll("^[^:]*:\\s*", ""));
    } else if (msg.startsWith("Failure")) {
      return new FailurePingResult(msg.replaceAll("^[^:]*:\\s*", ""));
    } else {
      return new PingResult(msg.replaceAll("^[^:]*:\\s*", ""));
    }
  }
  public static void main(String[] args) {
    Pinger res;
    if (args[0].equals("udp")) {
      res = new UDPPinger();
    } else {
      res = new TCPPinger();
    }
    MakeResult(res.Ping()).displayResult();
  }
}
