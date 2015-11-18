public class FailurePingResult extends PingResult {
  FailurePingResult(String in) {
    super(in);
  }
  @Override
  public void displayResult() {
    System.out.println("failure: " + inp);
  }
}
