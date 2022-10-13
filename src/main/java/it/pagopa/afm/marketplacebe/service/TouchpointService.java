package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.PageInfo;
import it.pagopa.afm.marketplacebe.model.touchpoint.Touchpoint;
import it.pagopa.afm.marketplacebe.model.touchpoint.TouchpointRequest;
import it.pagopa.afm.marketplacebe.model.touchpoint.Touchpoints;
import it.pagopa.afm.marketplacebe.repository.TouchpointRepository;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
@Slf4j
public class TouchpointService {

    @Autowired
    private TouchpointRepository touchpointRepository;

    @Autowired
    private ModelMapper modelMapper;

    public Touchpoints getTouchpoints() {
        List<Touchpoint> touchpointList = new ArrayList<>();

        touchpointRepository
                .findAll()
                .forEach(t -> touchpointList.add(modelMapper.map(t, Touchpoint.class)));

        PageInfo pageInfo = PageInfo.builder()
                .itemsFound(touchpointList.size())
                .totalPages(1)
                .build();

        return Touchpoints.builder()
                .touchpointList(touchpointList)
                .pageInfo(pageInfo)
                .build();
    }

    public Touchpoint getTouchpoint(String idTouchpoint){
        return modelMapper.map(getTouchpointById(idTouchpoint), Touchpoint.class);
    }

    public Touchpoint createTouchpoint(TouchpointRequest request){
        if(touchpointRepository.findByName(request.getName()).isPresent()){
            throw new AppException(AppError.TOUCHPOINT_CONFLICT, request.getName());
        }

        LocalDateTime now = LocalDateTime.now();
        it.pagopa.afm.marketplacebe.entity.Touchpoint entry = it.pagopa.afm.marketplacebe.entity.Touchpoint.builder()
                .createdDate(now)
                .name(request.getName())
                .build();

        return modelMapper.map(touchpointRepository.save(entry), Touchpoint.class);
    }

    public void deleteTouchpoint(String idTouchpoint){
        touchpointRepository.delete(getTouchpointById(idTouchpoint));
    }

    private it.pagopa.afm.marketplacebe.entity.Touchpoint getTouchpointById(String idTouchpoint){
        Optional<it.pagopa.afm.marketplacebe.entity.Touchpoint> touchpoint = touchpointRepository.findById(idTouchpoint);

        if(touchpoint.isEmpty()){
            throw new AppException(AppError.TOUCHPOINT_NOT_FOUND, idTouchpoint);
        }

        return touchpoint.get();
    }

    private it.pagopa.afm.marketplacebe.entity.Touchpoint getTouchpointByName(String name){
        Optional<it.pagopa.afm.marketplacebe.entity.Touchpoint> touchpoint = touchpointRepository.findByName(name);

        if(touchpoint.isEmpty()){
            throw new AppException(AppError.TOUCHPOINT_NOT_FOUND, name);
        }

        return touchpoint.get();
    }
}
