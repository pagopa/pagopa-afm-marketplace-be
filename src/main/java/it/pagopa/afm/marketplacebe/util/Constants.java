package it.pagopa.afm.marketplacebe.util;

import lombok.experimental.UtilityClass;

@UtilityClass
public class Constants {

    public static final String HEADER_REQUEST_ID = "X-Request-Id";

    @UtilityClass
    public static class DateTimeFormat {
        public static final String DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";
    }

}
