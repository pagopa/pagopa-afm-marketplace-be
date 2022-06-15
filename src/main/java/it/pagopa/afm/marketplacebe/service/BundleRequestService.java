package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.entity.Bundle;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.entity.CiBundleAttribute;
import it.pagopa.afm.marketplacebe.model.request.*;
import it.pagopa.afm.marketplacebe.repository.BundleRequestRepository;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class BundleRequestService {

    @Autowired
    BundleRequestRepository bundleRequestRepository;

    @Autowired
    ModelMapper modelMapper;

    /**
     * Get requests for creditor institution
     * @param ciFiscalCode
     * @param size
     * @param cursor
     * @param idPsp optional
     * @return
     */
    public CiRequests getRequestsByCI(String ciFiscalCode, Integer size, String cursor, String idPsp) {
        // TODO: pageable

        List<BundleRequest> requests = (idPsp == null) ?
                bundleRequestRepository.findByCiFiscalCode(ciFiscalCode) :
                bundleRequestRepository.findByCiFiscalCodeAndIdPsp(ciFiscalCode, idPsp);

        List<CiBundleRequest> ciBundleRequestList = requests.stream()
                .map(request -> modelMapper.map(request, CiBundleRequest.class)).collect(Collectors.toList());

        return CiRequests.builder()
                .requestsList(ciBundleRequestList)
                .build();
    }

    public BundleRequestId createRequest(String ciFiscalCode, CiBundleSubscriptionRequest ciBundleSubscriptionRequest) {

        // TODO retrieve bundle by idBundle
        Bundle bundle = new Bundle();

        List<CiBundleAttribute> attributes = ciBundleSubscriptionRequest.getCiBundleAttributeList()
                .stream()
                .map(attribute ->
                        CiBundleAttribute.builder()
                                .insertedDate(LocalDateTime.now())
                                .maxPaymentAmount(attribute.getMaxPaymentAmount())
                                .transferCategory(attribute.getTransferCategory())
                                .transferCategoryRelation(attribute.getTransferCategoryRelation())
                                .build()
                ).collect(Collectors.toList());

        BundleRequest request = BundleRequest.builder()
                .idBundle(ciBundleSubscriptionRequest.getIdBundle())
                .ciFiscalCode(ciFiscalCode)
                .idPsp(bundle.getIpPsP())
                .ciBundleAttributes(attributes)
                .build();

        bundleRequestRepository.save(request);

        return BundleRequestId.builder().idBundleRequest(request.getId()).build();
    }
}
