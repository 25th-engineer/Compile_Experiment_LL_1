package pojo;

public class Analyse {
    private String step;
    private String analysisStack;
    private String remainingString;
    private String productionType;
    private String action;

    public Analyse(){

    }

    public Analyse(String step, String analysisStack, String remainingString, String productionType, String action) {
        this.step = step;
        this.analysisStack = analysisStack;
        this.remainingString = remainingString;
        this.productionType = productionType;
        this.action = action;
    }
    public String getStep() {
        return step;
    }
    public String getAnalysisStack() {
        return analysisStack;
    }
    public String getRemainingString() {
        return remainingString;
    }
    public String getProductionType() {
        return productionType;
    }
    public String getAction() {
        return action;
    }
}
