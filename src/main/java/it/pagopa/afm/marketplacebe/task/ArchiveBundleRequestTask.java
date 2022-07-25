package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.entity.ArchivedBundleRequest;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class ArchiveBundleRequestTask implements Runnable {

    private BundleRequestRepository bundleRequestRepository;

    private ArchivedBundleRequestRepository archivedBundleRequestRepository;

    private ModelMapper modelMapper;

    private LocalDate now;

    public ArchiveBundleRequestTask(
            BundleRequestRepository bundleRequestRepository,
            ArchivedBundleRequestRepository archivedBundleRequestRepository,
            LocalDate now) {
        this.bundleRequestRepository = bundleRequestRepository;
        this.archivedBundleRequestRepository = archivedBundleRequestRepository;
        this.now = now;
        this.modelMapper = new ModelMapper();
    }

    @Override
    public void run() {
        List<BundleRequestEntity> requests = bundleRequestRepository.findByValidityDateToBefore(now);

        List<ArchivedBundleRequest> archivedBundles = requests.parallelStream().map(b -> modelMapper.map(b, ArchivedBundleRequest.class)).collect(Collectors.toList());
        archivedBundleRequestRepository.saveAll(archivedBundles);

        bundleRequestRepository.deleteAll(requests);
    }
}
