package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.ArchivedCiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleOfferRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import it.pagopa.afm.marketplacebe.service.CalculatorService;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.time.LocalDate;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest
class ArchiveBundleTaskTest {

    @MockBean
    BundleOfferRepository bundleOfferRepository;

    @MockBean
    ArchivedBundleOfferRepository archivedBundleOfferRepository;

    @MockBean
    BundleRequestRepository bundleRequestRepository;

    @MockBean
    ArchivedBundleRequestRepository archivedBundleRequestRepository;

    @MockBean
    BundleRepository bundleRepository;

    @MockBean
    ArchivedBundleRepository archivedBundleRepository;

    @MockBean
    CiBundleRepository ciBundleRepository;

    @MockBean
    ArchivedCiBundleRepository archivedCiBundleRepository;

    @MockBean
    CalculatorService calculatorService;

    @Test
    void archiveBundleTask() {
        when(bundleRepository.findByValidityDateToBefore(any(LocalDate.class))).thenReturn(List.of(TestUtil.getMockBundle()));
        BundleTaskExecutor taskExecutor = spy(new BundleTaskExecutor(bundleRepository, archivedBundleRepository));
        archiveTask(taskExecutor);
    }

    @Test
    void archiveBundleOfferTask() {
        when(bundleOfferRepository.findByValidityDateToBefore(any(LocalDate.class))).thenReturn(List.of(TestUtil.getMockBundleOffer()));
        BundleOfferTaskExecutor taskExecutor = spy(new BundleOfferTaskExecutor(bundleOfferRepository, archivedBundleOfferRepository));
        archiveTask(taskExecutor);
    }

    @Test
    void archiveBundleRequestTask() {
        when(bundleRequestRepository.findByValidityDateToBefore(any(LocalDate.class))).thenReturn(List.of(TestUtil.getMockBundleRequestE()));
        BundleRequestTaskExecutor taskExecutor = spy(new BundleRequestTaskExecutor(bundleRequestRepository, archivedBundleRequestRepository));
        archiveTask(taskExecutor);
    }

    @Test
    void archiveCiBundleTask() {
        when(ciBundleRepository.findByValidityDateToBefore(any(LocalDate.class))).thenReturn(List.of(TestUtil.getMockCiBundle()));
        CiBundleTaskExecutor taskExecutor = spy(new CiBundleTaskExecutor(ciBundleRepository, archivedCiBundleRepository));
        archiveTask(taskExecutor);
    }

    @Test
    void configureCalculatorTask() {
        when(bundleRepository.findAll()).thenReturn(List.of(TestUtil.getMockBundle()));
        when(ciBundleRepository.findAll()).thenReturn(List.of(TestUtil.getMockCiBundle()));
        CalculatorTaskExecutor taskExecutor = spy(new CalculatorTaskExecutor(calculatorService, bundleRepository, ciBundleRepository));
        archiveTask(taskExecutor);
    }

    private void archiveTask(TaskExecutor taskExecutor) {
        TaskRunnable task = new TaskRunnable(taskExecutor);

        ExecutorService executorService = Executors.newCachedThreadPool();
        executorService.execute(task);
        try {
            executorService.awaitTermination(5, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            fail();
        }

        verify(taskExecutor).execute();
    }
}
