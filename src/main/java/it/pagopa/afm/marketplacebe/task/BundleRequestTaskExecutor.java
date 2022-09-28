package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.entity.ArchivedBundleRequest;
import it.pagopa.afm.marketplacebe.entity.BundleRequestEntity;
import it.pagopa.afm.marketplacebe.repository.ArchivedBundleRequestRepository;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class BundleRequestTaskExecutor extends TaskExecutor {

    private final BundleRequestRepository bundleRequestRepository;

    private final ArchivedBundleRequestRepository archivedBundleRequestRepository;

    public BundleRequestTaskExecutor(
            BundleRequestRepository bundleRequestRepository,
            ArchivedBundleRequestRepository archivedBundleRequestRepository) {
        super();

        this.bundleRequestRepository = bundleRequestRepository;
        this.archivedBundleRequestRepository = archivedBundleRequestRepository;
    }

    @Override
    public void execute() {
        List<BundleRequestEntity> requests = bundleRequestRepository.findByValidityDateToBefore(now);

        List<ArchivedBundleRequest> archivedBundles = requests.parallelStream().map(b -> modelMapper.map(b, ArchivedBundleRequest.class)).collect(Collectors.toList());
        archivedBundleRequestRepository.saveAll(archivedBundles);

        bundleRequestRepository.deleteAll(requests);
        log.debug("Bundle Request archived");
    }
}
