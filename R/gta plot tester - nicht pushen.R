# #############################################################################
#
# # MUSS AUSKOMMENTIERT SEIN UM DAS PACKAGE KOMPILIEREN ZU K??NNEN
#
# #############################################################################
#
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/0 gtalibrary/")
# load("data/plot-test - nicht pushen.Rdata")
#
# library("ggplot2")
# library("gtalibrary")
# gta_colour_palette()
# #
# #
# # gta_colour_palette()
# p = ggplot(origins.year, aes(x=Year, y=surge.value.prior, fill=origin))+
#   geom_bar(stat="identity", position="fill")+
#   gta_plot_wrapper(x.bottom.breaks = seq(2006,2016,1),
#               plot.title = "Testplot One",
#               plot.subtitle = "Testplot with Facets",
#               y.data.type = "continuous",
#               x.bottom.name = "Year",
#               y.left.name = "Share",
#               fill.legend.col = 2,
#               fill.legend.title = "Countries",
#               y.right.enable = T,
#               y.left.breaks = seq(0,1,.1)) +
#   gta_theme(
#     x.bottom.angle = 90,
#     legend.title.align = 0
#   )
#
# p
#
# load("../data/master_plus.Rdata")
#
# master.0 <- aggregate(intervention.id~affected.jurisdiction + year(date.implemented)+month(date.implemented), subset(master, affected.jurisdiction %in% c("China","United States of America","Switzerland","Germany")), function(x) length(unique(x)))
#
#
# ggplot()+
#   geom_line(data=subset(master.0, affected.jurisdiction=="Switzerland"), aes(x=`year(date.implemented)`, y=intervention.id, colour=factor(affected.jurisdiction)))+
#   gta_plot_wrapper(data=master.0,
#                    value.x = "year(date.implemented)",
#                    value.y="intervention.id",
#                    group="month(date.implemented)",
#                    colour.palette = gta_colour$blue.shades(12))+
#   gta_theme()

#
# gta_plot_saver(path="../../../../",
#                plot = p,
#                aspect.ratio = 0.5,
#                name = "plot-test")
#
#
# ggplot(surge.share.agg, aes(x=Year, y=share.manufac, colour=type))+
#   geom_line(size=1)+
#   gta_plot_wrapper(
#     x.bottom.name = "Year",
#     y.left.name = "Share of G20 manufacturing imports\nthat is subject to a surge",
#     x.bottom.breaks = seq(2006,2016,1),
#     colour.palette = qualitative,
#     y.right.enable = T) +
#   gta_theme(
#     legend.title.align = 0)
#
#
# ggplot(surge.share.agg, aes(x=Year, y=share.manufac, fill=as.factor(type)))+
#   geom_bar(stat="identity", position="fill")+
#   gta_plot_wrapper(
#     fill.palette = qualitative,
#     x.bottom.name = "Year",
#     y.left.name = "Composition of G20 manufacturing value\nwhich is subject to an import surge",
#     x.bottom.breaks = seq(2006,2016,1),
#     y.left.breaks = seq(0,1,0.2),
#     y.right.enable = T,
#     fill.legend.title = "Type of sector") +
#   gta_theme(
#     legend.title.align = 0,
#     x.bottom.angle = 45)
#
#
# ggplot(fig1, aes(x=year, y=value, colour=variable)) +
#   geom_line(size=0.5) +
#   gta_plot_wrapper(
#     plot.title="Testplot Margin",
#     plot.subtitle="subtitle",
#     y.left.name = "Margin",
#     x.bottom.name = "Year",
#     x.top.name=NULL,
#     x.bottom.limits = c(2008,2017),
#     x.bottom.breaks = seq(2008, 2017, 1),
#     x.bottom.expand = c(0.05,0.05),
#     y.left.labels = percent,
#     y.left.expand = c(0.01,0.01),
#     y.right.enable = T,
#     y.right.transform = 1,
#     y.left.limits = c(-0.03, 0.16),
#     y.left.breaks = c(-0.03, seq(0,0.16,0.02)),
#     colour.palette = qualitative,
#     flip.plot = F) +
#   gta_theme()
#
# ggplot(fig3) +
#   geom_line(aes(x=year, y=value, colour=variable),size=1) +
#   gta_plot_wrapper(
#     x.bottom.name = "Years",
#     y.left.name = "Number of new policy interventions\nimplemented in a given year",
#     x.bottom.breaks = seq(2008,2018,1),
#     y.left.breaks = seq(0,120,20),
#     y.left.limits = c(0,120),
#     y.right.enable = T,
#     colour.palette = c(red[1:2], green[1]),
#     colour.legend.title = "Type of measure") +
#   gta_theme()
#
#
# b = ggplot(subset(fig5.bars)) +
#   geom_bar(data = subset(fig5.bars, variable !="Share tax-based subsidies (right axis)"), aes(year, weight=value, fill = variable))+
#   geom_line(data = subset(fig5, variable=="Share tax-based subsidies (right axis)"), aes(year, value*2000, colour=variable), size=1.5) +
#   gta_plot_wrapper(
#     x.bottom.name = "Year",
#     y.left.name = "Subsidies reported, \nmillions of US dollars",
#     x.bottom.limits = c(2008.5,2016.5),
#     x.bottom.breaks = seq(2009,2016,1),
#     y.left.limits = c(0,2000),
#     y.left.breaks = seq(0,2000,200),
#     y.right.enable = T,
#     y.right.transform = 1/2000,
#     y.right.breaks = seq(0,1,0.1),
#     colour.palette = qualitative[5],
#     colour.legend.col = 1,
#     fill.palette = qualitative[4:1],
#     fill.labels = c("Grants to others (left axis)", "Grants to Tata steel (left axis)", "Tax-based subsidies to rest (left axis)", "Tax-based subsidies to Steel Authority of India (left axis)"),
#     fill.legend.title="Bars",
#     fill.legend.col = 2) +
#   gta_theme(
#     legend.box.align = "horizontal",
#     legend.title.align = 0.5)
#
# b
#
# gta_plot_saver(path = "../../../../",
#                name="test-plot-2",
#                aspect.ratio = 0.5,
#                plot = b)




# # 3D MAP TESTER
#
# library(gtalibrary)
#
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
# load("4 data queries/171122 Doris Leuthard/data/Top CH export destinations.Rdata")
#
# map <- gta_plot_3d_map(data=top.destinations,
#                        value.x="Value",
#                        value.y="Year",
#                        countries="Reporter.un",
#                        save.eps = F,
#                        save.path = "../../",
#                        save.name = "plotplot",
#                        x.axis.labels = c("test","test","test","test"),
#                        y.axis.labels = c("test","test","test","test","test"),
#                        value.x.breaks=4,
#                        value.y.breaks=5)
#
#
# map

gtalibrary::gta_setwd("H")
load("data/support tables/Goods support table for gtalibrary.Rdata")
trade.base.1921 <- aggregate(
    trade.value ~ i.un + a.un + hs6,
    subset(trade.annual, year > 2019 & year <= 2021),
    sum
)

save(trade.base.1921, file = "C:/Users/sveng/OneDrive/Dokumente/GitHub/GTA/gtalibrary/datatrade.base.1921.rda")
