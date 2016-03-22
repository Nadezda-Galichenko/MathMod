eddy = read.csv("eddypro.csv", skip = 1)[-1, 1:77]
eddy$daytime = as.logical(eddy$daytime)
eddy = subset(eddy, as.Date(date) >= as.Date("2013-09-01") & as.Date(date) <= as.Date("2013-09-30") & daytime == F)
eddy = eddy[which(!is.na(eddy$daytime) & (eddy$rand_err_Tau != "NA")), ]
cor(as.numeric(eddy$x_90), as.numeric(eddy$h2o_flux))
cor(as.numeric(eddy$x_90), as.numeric(eddy$sonic_temperature))
cor(x = c(eddy$h2o_flux, eddy$h2o_strg, eddy$qc_h2o_flux), y = c(eddy$co2_flux, eddy$co2_strg, eddy$qc_co2_flux))

mod = lm(
  as.numeric(x_90) 
  ~
    as.numeric(h2o_flux) + as.numeric(sonic_temperature) + as.numeric(air_temperature) + as.numeric(air_pressure) + as.numeric(wind_speed),
  data = eddy
)

plot(x = mod)


round(cor(x = data.frame(H = as.double(eddy[ ,11]), qc_H = as.numeric(eddy[ ,12]), rand_err_H = as.numeric(eddy[ ,13]))), 2)

round(cor(x = data.frame(H = as.double(eddy$H), qc_H = as.numeric(eddy$qc_H), rand_err_H = as.numeric(eddy$rand_err_H))), 2)


