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

    public void setStep(String step) {
        this.step = step;
    }

    public String getAnalysisStack() {
        return analysisStack;
    }

    public void setAnalysisStack(String analysisStack) {
        this.analysisStack = analysisStack;
    }

    public String getRemainingString() {
        return remainingString;
    }

    public void setRemainingString(String remainingString) {
        this.remainingString = remainingString;
    }

    public String getProductionType() {
        return productionType;
    }

    public void setProductionType(String productionType) {
        this.productionType = productionType;
    }

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }
}