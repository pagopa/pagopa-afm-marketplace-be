package it.pagopa.afm.marketplacebe.util;

import it.pagopa.afm.marketplacebe.model.PageInfo;
import lombok.experimental.UtilityClass;
import org.springframework.data.domain.Page;

import java.time.LocalDate;
import java.util.Optional;

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
     *
     * @param validityDateTo
     * @return
     */
    public boolean isValidityDateToAcceptable(LocalDate validityDateTo) {
        LocalDate now = LocalDate.now();
        return validityDateTo == null || !(validityDateTo.isEqual(now) || validityDateTo.isBefore(now));
    }

    /**
     * @param value value to deNullify.
     * @return return false if value is null
     */
    public static Boolean deNull(Boolean value) {
        return Optional.ofNullable(value).orElse(false);
    }

    /**
     * Util to calculate total pages of a Cosmos paginated query
     *
     * @param limit page size
     * @param totalItems total items found in Cosmos
     * @return the total pages
     */
    public static int calculateTotalPages(Integer limit, double totalItems) {
        return (int) Math.ceil(totalItems / limit);
    }
}
