package it.pagopa.afm.marketplacebe.entity;

public enum PaymentMethod {
    PPAL("PPAL"),
    BPAY("BPAY"),
    PayBP("PayBP"),
    BBT("BBT"),
    AD("AD"),
    CP("CP"),
    PO("PO"),
    JIF("JIF"),
    MYBK("MYBK");

    private final String paymentMethod;

    PaymentMethod(final String paymentMethod) {
        this.paymentMethod = paymentMethod;
    }

    public String getPaymentMethod() {
        return paymentMethod;
    }
}
