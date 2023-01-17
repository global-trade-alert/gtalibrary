# Roxygen documentation

#' Convert HS codes of unkown vintage into HS 2012.
#'
#' Returns it as a vector of 6-digit level HS 2012 codes.
#'
#' @param codes Supply the HS codes you want to check. Values with 2 or more digits are allowed. Values with more than 6 digits will be limited to 6. Please input the codes as integer.
#' @param origin State the origin vintage, if you know it (options: '2002','2007','2012','2017','best', 'any'). Where 'best' selects the HS vintage with the most matches and 'any' selects HS codes from multiple origin vintages. Default is 'best'.
#' @param as_list returns the results as a list with the same length as the codes input vector, FALSE by default to ensure compatability with earlier version
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_hs_vintage_converter <- function(codes, origin = "best", as_list = FALSE) {
    codes <- as.numeric(as.character(codes))

    # Load HS vintages
    hs.vintages <- gtalibrary::hs.vintages

    if (!origin %in% c("best", "any")) {
        hs.vintages <- subset(hs.vintages, origin.vintage == paste("HS ", origin, sep = ""))

        # return matches for each hs code in the codes vector as vectors in a list
        # unconverted codes will have an empty character vector within the list
        codes.converted <- sapply(
            codes,
            \(x) unique(hs.vintages$hs.2012[hs.vintages$origin.code %in% x])
        )
    }

    if (origin == "best") {
        hs.02 <- subset(hs.vintages, origin.vintage == "HS 2002")
        hs.07 <- subset(hs.vintages, origin.vintage == "HS 2007")
        hs.12 <- subset(hs.vintages, origin.vintage == "HS 2012")
        hs.17 <- subset(hs.vintages, origin.vintage == "HS 2017")

        # is only done once for the entire vector --> If check needs to be done per code, move inside sapply()
        # --> Could result in different results as to when function is run within loop on a vector (regarding multiple outputs
        matches <- c(
            length(intersect(codes, hs.02$origin.code)),
            length(intersect(codes, hs.07$origin.code)),
            length(intersect(codes, hs.12$origin.code)),
            length(intersect(codes, hs.17$origin.code))
        )

        use.hs <- c(2002, 2007, 2012, 2017)[matches == max(matches)]
        hs.vintages <- subset(hs.vintages, origin.vintage == paste("HS ", max(use.hs), sep = ""))

        codes.converted <- lapply(
            codes,
            \(x) unique(hs.vintages$hs.2012[hs.vintages$origin.code %in% x])
        )
    }

    if (origin == "any") {
        codes.converted <- lapply(
            codes,
            \(x) unique(hs.vintages$hs.2012[hs.vintages$origin.code %in% x])
        )
    }

    # only print information for non-vectorized code
    if (!as_list) {
        not.converted <- sapply(codes.converted, \(x) all(x == 0))
        if (length(not.converted) > 0) {
            print(paste("Could not match the following codes: ", paste(codes[not.converted], collapse = ", "), sep = ""))
        } else {
            print("All supplied codes were matched.")
        }

        # returns the converted codes as a vector
        return(unlist(codes.converted))
    } else {
        # returns the converted codes as a list
        return(codes.converted)
    }
}
