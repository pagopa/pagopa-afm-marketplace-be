package it.pagopa.afm.marketplacebe.service;

import it.pagopa.afm.marketplacebe.TestUtil;
import it.pagopa.afm.marketplacebe.exception.AppException;
import it.pagopa.afm.marketplacebe.model.touchpoint.Touchpoint;
import it.pagopa.afm.marketplacebe.model.touchpoint.TouchpointRequest;
import it.pagopa.afm.marketplacebe.model.touchpoint.Touchpoints;
import it.pagopa.afm.marketplacebe.repository.TouchpointRepository;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest
public class TouchpointServiceTest {

    @Captor
    ArgumentCaptor<it.pagopa.afm.marketplacebe.entity.Touchpoint> touchpointArgumentCaptor = ArgumentCaptor.forClass(it.pagopa.afm.marketplacebe.entity.Touchpoint.class);
    @MockBean
    private TouchpointRepository touchpointRepository;
    @Autowired
    @InjectMocks
    private TouchpointService touchpointService;

    @Test
    void shouldGetTouchpoints() {

        // Precondition
        when(touchpointRepository.findAll()).thenReturn(TestUtil.getMockTouchpoints());

        // Tests
        Touchpoints touchpoints = touchpointService.getTouchpoints();

        // Assertions
        assertEquals(3, touchpoints.getTouchpointList().size());
    }

    @Test
    void shouldGetTouchpoint() {
        it.pagopa.afm.marketplacebe.entity.Touchpoint mockTouchpoint = TestUtil.getMockTouchpoint();
        String id = mockTouchpoint.getId();

        // Precondition
        when(touchpointRepository.findById(id)).thenReturn(Optional.of(mockTouchpoint));

        // Tests
        Touchpoint touchpoint = touchpointService.getTouchpoint(id);

        // Assertions
        assertEquals(mockTouchpoint.getId(),
                touchpoint.getId());
    }

    @Test
    void shouldCreateTouchpoint() {
        String TOUCHPOINT_NAME = "IO";
        TouchpointRequest touchpointRequest = TouchpointRequest
                .builder()
                .name(TOUCHPOINT_NAME)
                .build();

        // Precondition
        when(touchpointRepository.findByName(TOUCHPOINT_NAME)).thenReturn(Optional.empty());
        when(touchpointRepository.save(any())).thenReturn(TestUtil.getMockTouchpoint(TOUCHPOINT_NAME));

        // Tests
        Touchpoint entry = touchpointService.createTouchpoint(touchpointRequest);

        // Assertions
        assertEquals(TOUCHPOINT_NAME, entry.getName());
    }

    @Test
    void shouldThrowConflictCreateTouchpoint() {
        String TOUCHPOINT_NAME = "IO";
        TouchpointRequest touchpointRequest = TouchpointRequest
                .builder()
                .name(TOUCHPOINT_NAME)
                .build();

        // Precondition
        when(touchpointRepository.findByName(TOUCHPOINT_NAME)).thenReturn(Optional.of(TestUtil.getMockTouchpoint()));

        // Assertions
        AppException exception = assertThrows(AppException.class, () -> {
            touchpointService.createTouchpoint(touchpointRequest);
        });

        assertEquals(HttpStatus.CONFLICT, exception.getHttpStatus());
    }

    @Test
    void shouldDeleteTouchpoint() {
        it.pagopa.afm.marketplacebe.entity.Touchpoint mockTouchpoint = TestUtil.getMockTouchpoint();
        String id = mockTouchpoint.getId();

        // Precondition
        when(touchpointRepository.findById(id)).thenReturn(Optional.of(mockTouchpoint));

        // Tests
        touchpointService.deleteTouchpoint(id);

        verify(touchpointRepository).delete(touchpointArgumentCaptor.capture());

        assertEquals(id, touchpointArgumentCaptor.getValue().getId());
    }

    @Test
    void shouldThrowNotFoundDeleteTouchpoint() {
        it.pagopa.afm.marketplacebe.entity.Touchpoint mockTouchpoint = TestUtil.getMockTouchpoint();
        String id = mockTouchpoint.getId();

        // Precondition
        when(touchpointRepository.findById(id)).thenReturn(Optional.empty());

        // Tests
        AppException exception = assertThrows(AppException.class, () -> {
            touchpointService.deleteTouchpoint(id);
        });

        assertEquals(HttpStatus.NOT_FOUND, exception.getHttpStatus());
    }
}
