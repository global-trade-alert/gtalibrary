# Roxygen documentation

#' Extract the complete DOM of an XML file.
#'
#'
#' @param node Provide an xmlRoot object.
#' @param dom.position Specify the starting level for your DOM. Default is 1 (=root).
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#'
gta_collect_dom <- function(node=NULL,
                            dom.position=c()) {

  # defining sub-function
  parse_dom <- function(node=NULL,
                        dom.position=c()) {
      if (is.null(node)) {
        #leaf node reached. Turn back
        return()
      }

      e.name=xmlName(node)
      e.value=xmlValue(node)
      e.type=F

      complete.dom<-rbind(complete.dom,
                          data.frame(position=dom.position,
                                     element.name=e.name,
                                     is.attribute=e.type,
                                     element.value=e.value,
                                     stringsAsFactors = F))

      if(length(xmlAttrs(node))>0){

        x.attr=xmlAttrs(node)

        e.name=names(x.attr)
        e.value=x.attr
        e.type=T

        complete.dom<<-rbind(complete.dom,
                             data.frame(position=dom.position,
                                        element.name=e.name,
                                        is.attribute=e.type,
                                        element.value=e.value,
                                        stringsAsFactors = F))
        rm(x.attr)
      }

      num.children = xmlSize(node)

      #Go one level deeper
      if (num.children > 0) {
        for (i in 1 : num.children) {
          parse_dom(node[[i]],paste(dom.position, i, sep="")) #the i-th child of node
        }
      }
  }

  complete.dom=data.frame()

  parse_dom(node=node,
            dom.position=dom.position)


  return(complete.dom)

}
