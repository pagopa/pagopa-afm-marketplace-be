package it.pagopa.afm.marketplacebe.config;

import it.pagopa.afm.marketplacebe.exception.AppError;
import it.pagopa.afm.marketplacebe.exception.AppException;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;

import javax.validation.ConstraintViolation;
import javax.validation.Validator;

@Aspect
@Component
public class EntityValidator {

    @Autowired
    private Validator validator;


    /**
     * This method validates the response annotated with the {@link javax.validation.constraints}
     *
     * @param joinPoint not used
     * @param result    the response to validate
     */
    @AfterReturning(pointcut = "execution(* it.pagopa.afm.marketplacebe.repository.*.*(..))", returning = "result")
    public void validateEntity(JoinPoint joinPoint, Object result) {
        if (result instanceof Flux) {
//            ((Flux<?>) result).subscribe(this::validate);
        }
    }

    private void validate(Object result) {
        if (result != null) {
            var validationResults = validator.validate(result);

            if (!validationResults.isEmpty()) {
                var sb = new StringBuilder();
                for (ConstraintViolation<Object> error : validationResults) {
                    sb.append(error.getPropertyPath()).append(" ").append(error.getMessage()).append(". ");
                }
                var msg = StringUtils.chop(sb.toString());
                throw new AppException(AppError.ENTITY_VALIDATION_FAIL, msg);
            }
        }
    }
}
