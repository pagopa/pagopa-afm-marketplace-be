package it.pagopa.afm.marketplacebe.task;

import it.pagopa.afm.marketplacebe.entity.ArchivedCiBundle;
import it.pagopa.afm.marketplacebe.entity.CiBundle;
import it.pagopa.afm.marketplacebe.repository.ArchivedCiBundleRepository;
import it.pagopa.afm.marketplacebe.repository.CiBundleRepository;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class ArchiveCiBundleTask implements Runnable {

    private CiBundleRepository ciBundleRepository;

    private ArchivedCiBundleRepository archivedCiBundleRepository;

    private ModelMapper modelMapper;

    private LocalDate now;

    public ArchiveCiBundleTask(
            CiBundleRepository cibundleRepository,
            ArchivedCiBundleRepository archivedCiBundleRepository,
            LocalDate now) {
        this.ciBundleRepository = cibundleRepository;
        this.archivedCiBundleRepository = archivedCiBundleRepository;
        this.now = now;
        this.modelMapper = new ModelMapper();
    }

    @Override
    public void run() {
        List<CiBundle> bundles = ciBundleRepository.findByValidityDateToBefore(now);

        List<ArchivedCiBundle> archivedBundles = bundles.parallelStream().map(b -> modelMapper.map(b, ArchivedCiBundle.class)).collect(Collectors.toList());
        archivedCiBundleRepository.saveAll(archivedBundles);

        ciBundleRepository.deleteAll(bundles);

    }
}
