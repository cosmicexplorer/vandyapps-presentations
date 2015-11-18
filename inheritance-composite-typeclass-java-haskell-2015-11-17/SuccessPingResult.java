public class SuccessPingResult extends PingResult {
  SuccessPingResult(String in) {
    super(in);
  }
  @Override
  public void displayResult() {
    System.out.println("success: " + inp);
  }
}
