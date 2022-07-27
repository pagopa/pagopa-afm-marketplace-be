package it.pagopa.afm.marketplacebe.task;

import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;

@Slf4j
public class TaskManager implements Runnable {

    private final BundleTaskExecutor bundleArchiver;
    private final BundleOfferTaskExecutor bundleOfferArchiver;
    private final BundleRequestTaskExecutor bundleRequestArchiver;
    private final CiBundleTaskExecutor ciBundleArchiver;

    private final CalculatorTaskExecutor calculatorTaskExecutor;

    private final ScheduledThreadPoolExecutor scheduledThreadPoolExecutor;

    public TaskManager(BundleTaskExecutor bundleArchiver,
                       BundleOfferTaskExecutor bundleOfferArchiver,
                       BundleRequestTaskExecutor bundleRequestArchiver,
                       CiBundleTaskExecutor ciBundleArchiver,
                       CalculatorTaskExecutor calculatorTaskExecutor,
                       ScheduledThreadPoolExecutor scheduledThreadPoolExecutor) {
        this.bundleArchiver = bundleArchiver;
        this.bundleOfferArchiver = bundleOfferArchiver;
        this.bundleRequestArchiver = bundleRequestArchiver;
        this.ciBundleArchiver = ciBundleArchiver;

        this.calculatorTaskExecutor = calculatorTaskExecutor;
        this.scheduledThreadPoolExecutor = scheduledThreadPoolExecutor;
    }

    @Override
    public void run() {

        CompletableFuture<Void> cf1 = CompletableFuture.runAsync(new TaskRunnable(bundleArchiver), scheduledThreadPoolExecutor);
        CompletableFuture<Void> cf2 = CompletableFuture.runAsync(new TaskRunnable(bundleOfferArchiver), scheduledThreadPoolExecutor);
        CompletableFuture<Void> cf3 = CompletableFuture.runAsync(new TaskRunnable(bundleRequestArchiver), scheduledThreadPoolExecutor);
        CompletableFuture<Void> cf4 = CompletableFuture.runAsync(new TaskRunnable(ciBundleArchiver), scheduledThreadPoolExecutor);

        CompletableFuture.allOf(cf1, cf2, cf3, cf4).thenRunAsync(new TaskRunnable(calculatorTaskExecutor), scheduledThreadPoolExecutor);
    }
}
