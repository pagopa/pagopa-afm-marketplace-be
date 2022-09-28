package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.entity.ArchivedCiBundle;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.repository.ArchivedCiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class CiBundleTaskExecutor extends TaskExecutor {

    private final CiBundleRepository ciBundleRepository;

    private final ArchivedCiBundleRepository archivedCiBundleRepository;

    public CiBundleTaskExecutor(
            CiBundleRepository ciBundleRepository,
            ArchivedCiBundleRepository archivedCiBundleRepository) {
        super();

        this.ciBundleRepository = ciBundleRepository;
        this.archivedCiBundleRepository = archivedCiBundleRepository;
    }

    @Override
    public void execute() {
        List<CiBundle> bundles = ciBundleRepository.findByValidityDateToBefore(now);

        List<ArchivedCiBundle> archivedBundles = bundles.parallelStream().map(b -> modelMapper.map(b, ArchivedCiBundle.class)).collect(Collectors.toList());
        archivedCiBundleRepository.saveAll(archivedBundles);

        ciBundleRepository.deleteAll(bundles);
        log.debug("Ci Bundle archived");
    }
}
