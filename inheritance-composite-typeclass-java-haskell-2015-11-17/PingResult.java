public class PingResult {
  protected String inp;
  PingResult(String in) {
    inp = in;
  }
  public void displayResult() {
    System.out.println("info: " + inp);
  }
}
