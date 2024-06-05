###
# brexit impact
# 220615
###
# getwd()
setwd("~/work/Teaching/DSIER23/05-event-data")
wdir <- getwd()
dir.create(file.path(wdir, "output"), showWarnings = FALSE)


if (!require("pacman")) install.packages("pacman"); library("pacman")
p_load(data.table)
p_load(stringr)
p_load(lubridate)
p_load(countrycode)
p_load(purrr)
p_load(ggplot2)
p_load(scales)
p_load(fixest)
p_load(etable)


# definitions
EU27 = c("DEU", "AUT", "BEL", "DNK",
         "FIN", "FRA", "GRC", "IRL",
         "ITA", "LUX", "NLD", "PRT",
         "ESP", "SWE", "MLT", "CYP",
         "EST", "LTU", "LVA", "CZE", 
         "HUN", "BGR", "ROU", "POL",
         "SVK", "SVN", "HRV")

# list of files
files = list.files("input/monthly_hs2", full.names = T)
data = fread(files[1])
data = fread(cmd = str_c("gunzip -c ", files[1])) # this command is equivalent
# data = fread(cmd = str_c("unzip -p ", files[1]))  # automatically unzips file and then reads into data.table

# first look at the data
View(data)
head(data)
data[, uniqueN(Reporter)]
data[, uniqueN(Partner)]
data[, uniqueN(Commodity)]
data[, unique(`Trade Flow`)] 
data[, .N]

# check trade zeros
data[`Trade Flow Code` == 2, .N]

# check trade zeros
data[, uniqueN(Reporter)] * data[, uniqueN(Partner)] * data[, uniqueN(Commodity)] 
data[`Trade Flow Code` == 1, .N] / (data[, uniqueN(Reporter)] * data[, uniqueN(Partner)] * data[, uniqueN(Commodity)])

wdir <- getwd()
dir.create(file.path(wdir, "temp"), showWarnings = FALSE)

# read all files
files = list.files("input/monthly_hs2", full.names = T)

# f = files[1]
for (f in files) {
    print(f)

    # load
    data = fread(f)

    # format and clean data
    data = data[, .(date = Period,
                    origin = Reporter,
                    destination = Partner,
                    hs = `Commodity Code`,
                    flow = `Trade Flow Code`,
                    value = `Trade Value (US$)`)]

    # make dates pretty
    data[, date := ymd(str_c(date, "01"))]
    data[, date := date + months(1) - days(1)]
    # str(data)

    # use country codes
    data[, origin := countryname(origin, "iso3c")]
    data[, destination := countryname(destination, "iso3c")]
    # countrycode("Germany", "country.name", "iso3c")
    # countrycode("Deutschland", "country.name.de", "iso3c")
    # countryname(c("Deutschland", "Alemania", "Allemagne", "Duitsland"), "iso3c")

    # transform value from integer to numeric
    data[, value := as.numeric(value)]

    # only take obs with non-missing variable
    data = data[complete.cases(data)]

    fwrite(data,
           "temp/monthly_1921.csv.gz",
           compress = "gzip",
           append = T)

}
 
# # alternativ
# extract_data = function (...) {}
# map(files, ~ extract_data(.x), .progress = T)

# load full dataset
data = fread("temp/monthly_1921.csv.gz")
data = data[flow == 2]
gc()


data[, .N, by = origin] # greater or equal than 125
data[, .(date, origin)] %>%
  unique() %>%
  .[, .N, by = .(year(date), origin)] %>%
  .[, .(N = sum(N)), by = origin]


# brexit impact
data[, unique(hs)]
exports_uk = data[origin == "GBR" & hs == "TOTAL", -c("origin", "hs", "flow")]

# plot: aggregate over time
plot_data = exports_uk[, .(value = sum(value)), by = date]

plot=ggplot(plot_data) +
    theme_minimal() +
    geom_line(aes(x = date, y = value / 1000000)) +
    geom_vline(aes(xintercept = ymd("2020-01-31")), color = "red") +
    geom_vline(aes(xintercept = ymd("2020-12-31")), color = "red") +
    scale_x_date(name = NULL) +
    scale_y_continuous(name = "Total exports in mn USD",
                       labels = scales::dollar) +
    ggtitle(label = "Total value of UK exports",
            subtitle = "UN Comtrade Data, 2019 – 2021")
ggsave(plot,
       filename = str_c(wdir, "/output/UK_exp.png"),
       width = 20,
       height = 20,
       units = "cm")

# plot: compare to other countries
plot_data = data[origin %in% c("GBR", "IRL", "ISL", "SWE"), .(value = sum(value)), by = .(date, origin)]

plot_data[, value_norm := value / mean(value[year(date) == "2019"]), by = origin]

plot= ggplot(plot_data) +
    theme_minimal() +
    geom_line(aes(x = date, y = value_norm, group = origin, color = origin)) +
    geom_vline(aes(xintercept = ymd("2020-01-31")), color = "red") +
    geom_vline(aes(xintercept = ymd("2020-12-31")), color = "red") +
    scale_x_date(name = NULL) +
    scale_y_continuous(name = "Total exports compared to average of 2019") +
    ggtitle(label = "Impact of Brexit",
            subtitle = "UN Comtrade Data, 2019 – 2021")

ggsave(plot,
       filename = str_c(wdir, "/output/exp_to_all_dest.png"),
       width = 20,
       height = 20,
       units = "cm")


# plot: compare to other countries the exports only to EU27
plot_data = data[origin %in% c("GBR", "IRL", "ISL", "SWE") & destination %in% EU27, .(value = sum(value)), by = .(date, origin)]

plot_data[, value_norm := value / mean(value[year(date) == "2019"]), by = origin]

plot_data[, Country := countrycode(origin, "iso3c", "country.name")]

plot = ggplot(plot_data) +
    theme_minimal() +
    geom_line(aes(x = date, y = value_norm, group = Country, color = Country)) +
    geom_vline(aes(xintercept = ymd("2020-01-31")), color = "red") +
    geom_vline(aes(xintercept = ymd("2020-12-31")), color = "red") +
    scale_x_date(name = NULL) +
    scale_y_continuous(name = "Exports to EU compared to average of 2019") +
    ggtitle(label = "Impact of Brexit",
            subtitle = "UN Comtrade Data, 2019 – 2021")

ggsave(plot,
       filename = str_c(wdir, "/output/exp_to_EU27.png"),
       width = 20,
       height = 20,
       units = "cm")


# regressions instead of eyeball econometrics:all
  # 1. regression export value from the 4 exporters toward EU destinations on Brexit treatment
        # Identification: use variation within country and controlling for (monthly) time trends common to all 4 exporters.
  # 2. the same as 1 but for exports towards all destinations? What do you expect? 
  # 3. the same but including zeros and estimating via PPML



reg_data = data[origin %in% c("GBR", "IRL", "ISL", "SWE") & destination %in% EU27, .(value = sum(value)), by = .(date, origin)]

reg_data[, treatment := (origin == "GBR") * (date > ymd("2020-12-31"))]

fixest::etable(reg1)

# all countries
reg_data = data[, .(value = sum(value)), by = .(date, origin)]

reg_data[, treatment := (origin == "GBR") * (date > ymd("2020-12-31"))]

# 162 zeros
reg2 = feols(log(value) ~ treatment | date + origin, data = reg_data)

reg3 = fepois(value ~ treatment | date + origin, data = reg_data)
# etable(reg1, reg2, reg3)

# gravity
reg_data = data[hs == "TOTAL", -c("hs", "flow")]
reg_data[, origin_date := str_c(origin, date)]
reg_data[, destination_date := str_c(destination, date)]
reg_data[, origin_destination := str_c(origin, destination)]

reg_data[, treatment := (origin == "GBR") * (destination %in% EU27)* (date > ymd("2020-12-31"))]

reg4 = feols(log(value) ~ treatment | origin_date + destination_date + origin_destination, data = reg_data)

reg5 = fepois(value ~ treatment | origin_date + destination_date + origin_destination, data = reg_data)


# hwo to create zeros (for on month only)
data_zeros = data[date == ymd("2020-12-31") & hs == "TOTAL", -c("hs", "flow")]

data_zeros[, uniqueN(origin)] * data_zeros[, uniqueN(destination)]
data_zeros[, .N]

data_grid = CJ(origin = data_zeros[, unique(origin)],
               destination = data_zeros[, unique(destination)],
               date = data_zeros[, unique(date)])
data_grid = data_grid[origin != destination]

data_zeros = merge(data_grid,
                   data_zeros,
                   by = c("date", "origin", "destination"),
                   all.x = T)
data_zeros[is.na(value), value := 0]


