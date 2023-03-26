# Roxygen documentation

#' Extract the complete DOM of an XML file.
#'
#'
#' @param node Provide an xmlRoot object.
#' @param dom.position Specify the starting level for your DOM. Default is 1 (=root).
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_collect_dom <- function(node = NULL,
                            dom.pos = c()) {
    # if (is.null(node)) {
    #   #leaf node reached. Turn back
    #   return()
    # }
    # print(paste("Node: ", xmlName(node)))

    e.name <- xmlName(node)
    e.value <- xmlValue(node)
    e.type <- F

    ted.parsed <<- rbind(
        ted.parsed,
        data.frame(
            position = dom.pos,
            element.name = e.name,
            is.attribute = e.type,
            element.value = e.value,
            stringsAsFactors = F
        )
    )

    if (length(xmlAttrs(node)) > 0) {
        x.attr <- xmlAttrs(node)

        e.name <- names(x.attr)
        e.value <- x.attr
        e.type <- T

        ted.parsed <<- rbind(
            ted.parsed,
            data.frame(
                position = dom.pos,
                element.name = e.name,
                is.attribute = e.type,
                element.value = e.value,
                stringsAsFactors = F
            )
        )
        rm(x.attr)
    }

    num.children <- xmlSize(node)

    # Go one level deeper
    if (num.children > 0) {
        for (i in 1:num.children) {
            gta_collect_dom(node[[i]], paste(dom.pos, sprintf("%02i", i), sep = "")) # the i-th child of node
        }
    }
}
