package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.repository.BundleRepository;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class BundleService {

    @Autowired
    private BundleRepository bundleRepository;

    @Autowired
    private ModelMapper modelMapper;

    // TODO: add pagination
    // TODO: add filter
    public Bundles getBundlesByIdPsp(String idPsp, Integer pageNumber, Integer limit) {
         List<it.pagopa.afm.marketplacebe.model.bundle.Bundle> bundleList = bundleRepository
                 .findByIdPsp(idPsp)
                .stream()
                .map(bundle -> modelMapper.map(bundle, it.pagopa.afm.marketplacebe.model.bundle.Bundle.class))
                 .collect(Collectors.toList());

        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(bundleList.size())
                .totalPages(1)
                .build();

        return Bundles.builder().bundleList(bundleList).pageInfo(pageInfo).build();
    }

    /*
    public Mono<Bundle> addBundle(Bundle bundle){
        return bundleRepository.save(bundle);
    }

    public Mono<Void> deleteBundle(String idBundle){
        return bundleRepository.deleteById(idBundle);
    }

    public Mono<Bundle> updateBundle(Bundle bundle){
        return bundleRepository.save(bundle);
    }
     */
}
