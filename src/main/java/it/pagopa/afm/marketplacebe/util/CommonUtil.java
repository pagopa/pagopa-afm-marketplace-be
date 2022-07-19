package it.pagopa.afm.marketplacebe.util;

import it.pagopa.afm.marketplacebe.model.PageInfo;
import lombok.experimental.UtilityClass;
import org.springframework.data.domain.Page;

import java.time.LocalDate;

@UtilityClass
public class CommonUtil {

    /**
     * @param page Page returned from the database
     * @return return the page info
     */
    public <T> PageInfo buildPageInfo(Page<T> page) {
        return PageInfo.builder()
                .page(page.getNumber())
                .limit(page.getSize())
                .totalPages(page.getTotalPages())
                .itemsFound(page.getNumberOfElements())
                .build();
    }

    /**
     * Verify if validityDateTo is after now
     * @param validityDateTo
     * @return
     */
    public boolean isValidityDateToAcceptable(LocalDate validityDateTo) {
        LocalDate now = LocalDate.now();
        return validityDateTo == null || !(validityDateTo.isEqual(now) || validityDateTo.isBefore(now));
    }
}
