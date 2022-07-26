package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedCiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.service.CalculatorService;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDate;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;

@Slf4j
public class TaskManager implements Runnable {

    private BundleRepository bundleRepository;
    private ArchivedBundleRepository archivedBundleRepository;

    private BundleOfferRepository bundleOfferRepository;
    private ArchivedBundleOfferRepository archivedBundleOfferRepository;

    private BundleRequestRepository bundleRequestRepository;
    private ArchivedBundleRequestRepository archivedBundleRequestRepository;

    private CiBundleRepository ciBundleRepository;
    private ArchivedCiBundleRepository archivedCiBundleRepository;

    private CalculatorService calculatorService;

    private LocalDate now;

    private ScheduledThreadPoolExecutor scheduledThreadPoolExecutor;

    public TaskManager(BundleRepository bundleRepository,
                       BundleOfferRepository bundleOfferRepository,
                       BundleRequestRepository bundleRequestRepository,
                       CiBundleRepository ciBundleRepository,
                       ArchivedBundleRepository archivedBundleRepository,
                       ArchivedBundleOfferRepository archivedBundleOfferRepository,
                       ArchivedBundleRequestRepository archivedBundleRequestRepository,
                       ArchivedCiBundleRepository archivedCiBundleRepository,
                       LocalDate now,
                       CalculatorService calculatorService, ScheduledThreadPoolExecutor scheduledThreadPoolExecutor) {
        this.bundleRepository = bundleRepository;
        this.archivedBundleRepository = archivedBundleRepository;

        this.bundleOfferRepository = bundleOfferRepository;
        this.archivedBundleOfferRepository = archivedBundleOfferRepository;

        this.bundleRequestRepository = bundleRequestRepository;
        this.archivedBundleRequestRepository = archivedBundleRequestRepository;

        this.ciBundleRepository = ciBundleRepository;
        this.archivedCiBundleRepository = archivedCiBundleRepository;

        this.now = now;

        this.calculatorService = calculatorService;

        this.scheduledThreadPoolExecutor = scheduledThreadPoolExecutor;
    }

    @Override
    public void run() {
        CompletableFuture<Void> cf1 = CompletableFuture.runAsync(new ArchiveBundleTask(bundleRepository, archivedBundleRepository, now), scheduledThreadPoolExecutor);
        CompletableFuture<Void> cf2 = CompletableFuture.runAsync(new ArchiveBundleOfferTask(bundleOfferRepository, archivedBundleOfferRepository, now), scheduledThreadPoolExecutor);
        CompletableFuture<Void> cf3 = CompletableFuture.runAsync(new ArchiveBundleRequestTask(bundleRequestRepository, archivedBundleRequestRepository, now), scheduledThreadPoolExecutor);
        CompletableFuture<Void> cf4 = CompletableFuture.runAsync(new ArchiveCiBundleTask(ciBundleRepository, archivedCiBundleRepository, now), scheduledThreadPoolExecutor);

        CompletableFuture.allOf(cf1, cf2, cf3, cf4).thenRunAsync(new CalculatorTask(calculatorService, bundleRepository, ciBundleRepository), scheduledThreadPoolExecutor);
    }
}
