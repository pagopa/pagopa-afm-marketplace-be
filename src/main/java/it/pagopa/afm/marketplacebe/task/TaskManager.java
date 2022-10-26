package it.pagopa.afm.marketplacebe.task;

import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.CompletableFuture;

@Slf4j
public class TaskManager implements Runnable {

    private final BundleTaskExecutor bundleArchiver;
    private final BundleOfferTaskExecutor bundleOfferArchiver;
    private final BundleRequestTaskExecutor bundleRequestArchiver;
    private final CiBundleTaskExecutor ciBundleArchiver;
    private final ValidBundlesTaskExecutor validBundlesTaskExecutor;

    public TaskManager(BundleTaskExecutor bundleArchiver,
                       BundleOfferTaskExecutor bundleOfferArchiver,
                       BundleRequestTaskExecutor bundleRequestArchiver,
                       CiBundleTaskExecutor ciBundleArchiver,
                       ValidBundlesTaskExecutor validBundlesTaskExecutor) {
        this.bundleArchiver = bundleArchiver;
        this.bundleOfferArchiver = bundleOfferArchiver;
        this.bundleRequestArchiver = bundleRequestArchiver;
        this.ciBundleArchiver = ciBundleArchiver;

        this.validBundlesTaskExecutor = validBundlesTaskExecutor;
    }

    @Override
    public void run() {
        CompletableFuture<Void> cf1 = CompletableFuture.runAsync(new TaskRunnable(bundleArchiver));
        CompletableFuture<Void> cf2 = CompletableFuture.runAsync(new TaskRunnable(bundleOfferArchiver));
        CompletableFuture<Void> cf3 = CompletableFuture.runAsync(new TaskRunnable(bundleRequestArchiver));
        CompletableFuture<Void> cf4 = CompletableFuture.runAsync(new TaskRunnable(ciBundleArchiver));

        CompletableFuture.allOf(cf1, cf2, cf3, cf4).thenRunAsync(new TaskRunnable(validBundlesTaskExecutor));
    }
}
