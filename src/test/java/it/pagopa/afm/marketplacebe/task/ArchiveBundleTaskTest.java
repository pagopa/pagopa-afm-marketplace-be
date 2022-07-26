package it.pagopa.afm.marketplacebe.task;


import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

class ArchiveBundleTaskTest {

    @MockBean
    BundleTaskExecutor bundleTaskExecutor;

    @MockBean
    BundleOfferTaskExecutor bundleOfferTaskExecutor;

    @MockBean
    BundleRequestTaskExecutor bundleRequestTaskExecutor;

    @MockBean
    CiBundleTaskExecutor ciBundleTaskExecutor;

    @Test
    public void archiveBundleTask() throws InterruptedException {
        TaskRunnable task = new TaskRunnable(bundleTaskExecutor);

        ExecutorService executor = Executors.newCachedThreadPool();
        executor.execute(task);
//        executor.awaitTermination(60, TimeUnit.SECONDS);
//        Mockito.verify(bundleTaskExecutor).execute();
    }

    @Test
    public void archiveBundleOfferTask() {
        TaskRunnable task = new TaskRunnable(bundleOfferTaskExecutor);

        ExecutorService executor = Executors.newCachedThreadPool();
        executor.execute(task);
    }

    @Test
    public void archiveBundleRequestTask() {
        TaskRunnable task = new TaskRunnable(bundleRequestTaskExecutor);

        ExecutorService executor = Executors.newCachedThreadPool();
        executor.execute(task);
    }

    @Test
    public void archiveCiBundleTask() {
        TaskRunnable task = new TaskRunnable(ciBundleTaskExecutor);

        ExecutorService executor = Executors.newCachedThreadPool();
        executor.execute(task);
    }

}
