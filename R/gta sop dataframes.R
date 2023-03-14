# Roxygen documentation

#' Create a data frame that contains the names of the data frames used in the data dumps with their respective variables.
#'
#' Returns a data frame with the necessary information for the data dumps team.
#'
#' @param dataframes Specify any optional data frame you wish to use. Default FALSE.
#'
#' @return Dataframe with the necessary information for a given data dump.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

# Last Update of the function: 03.11.2022
# Names and columns based on the names provided in the SOP (last update: 28.06.2022)
#' @export
gta_sop_dataframes=function(aff.jur.df=F, dist.market.df=F, int.level.df=F, hs.codes.df=F, cpc.codes.df=F,
                            firms.df=F, products.df=F, investigations.df=F, frameworks.df=F, related.acts.df=F,
                            locations.df=F) {
  # Mandatory data frames
  # master
  nam.amas<- data.frame(df="master",nam=c("intervention.id", "state.act.id", "title", "announcement.description", "source",
                                          "is.source.official", "announcement.date", "intervention.description", "intervention.type",
                                          "evaluation", "research.evaluation", "eligible.firms", "implementation.level", "affected.flow",
                                          "inception.date", "removal.date", "is.horizontal.measure","freeze.dm","freeze.aj",
                                          "is.jumbo", "is.duration.limited", "fta.exempt", "is.non.trade.related.rationale",
                                          "inception.date.type.id","is.chain.measure"))

  # impl.jur.df
  nam.bimpl.jur<-data.frame(df="impl.jur.df",nam=c("intervention.id", "implementing.jurisdiction"))

  # Optional dataframes
  # aff.jur.df
  if (aff.jur.df) {
    nam.caff.jur<-data.frame(df="aff.jur.df",nam=c("intervention.id", "affected.jurisdiction"))
  }

  # dist.market.df
  if (dist.market.df) {
    nam.ddis.mar<-data.frame(df="dist.market.df",nam=c("intervention.id", "distorted.market"))
  }

  # int.level.df
  if (int.level.df) {
    nam.eint.lev<-data.frame(df="int.level.df",nam=c("intervention.id", "prior.level", "new.level", "tariff.peak", "level.unit",
                                                     "level.type.id"))
  }

  # hs.codes.df
  if (hs.codes.df) {
    nam.fhs.cod<-data.frame(df="hs.codes.df",nam=c("intervention.id", "hs.codes", "inception.date", "removal.date", "prior.level" ,
                                                   "new.level" ,"level.unit", "tariff.peak", "is.tariff.official"))
  }

  # cpc.codes.df
  if (cpc.codes.df) {
    nam.gcpc.cod<-data.frame(df="cpc.codes.df",nam=c("intervention.id", "cpc.codes"))
  }

  # firms.df
  if (firms.df) {
    nam.hfirms<-data.frame(df="firms.df",nam=c("firm.name","firm.name.original","role.id","location","assignment.id",
                                               "intervention.id","melitz.status"))
  }

  # products.df
  if (products.df) {
    nam.iprod<-data.frame(df="products.df",nam=c("product.name","assignment.id","intervention.id"))
  }

  # investigations.df
  if (investigations.df) {
    nam.jinvest<-data.frame(df="investigations.df",nam=c("intervention.id","investigation.status","investigation.date"))
  }

  # frameworks.df
  if (frameworks.df) {
    nam.kframework<-data.frame(df="frameworks.df",nam=c('state.act.id','framework.name'))
  }

  # related.acts.df
  if (related.acts.df) {
    nam.lrelat.act<-data.frame(df="related.acts.df",nam=c("state.act.id", "related.state.act.id", "related.is.database.id"))
  }

  # locations.df
  if (locations.df) {
    nam.mlocation<-data.frame(df="locations.df",nam=c("intervention.id","location","location.type.id", "location.jurisdiction"))
  }

  # Creation of the dataframe with the required information
  sop.data.dump<-data.table::rbindlist(mget(ls(pattern = "nam.")), fill = T, use.names = T); sop.data.dump<-data.frame(sop.data.dump)
  sop.data.dump$df<-as.character(sop.data.dump$df)
  sop.data.dump$nam<-as.character(sop.data.dump$nam); rm(list = ls(pattern = "nam\\."))
  names(sop.data.dump) = c("df", "cols")

  return(sop.data.dump)

}
