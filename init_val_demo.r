# source the population dynamics script
source("demo_limit_sim.r")

#test = demo_limit_sim.f(S = 15000,N = 500,iter = 110, Stream = "UVol_BT",mort.list = uppvol_surv_06_14.list, year_max = 2008)

###### values of parameters to be simulated

S.vett = 10000
N.vett = 500
iter.vett = 110
Stream.vett = c(1)
mort.list.vett = c(1,2,3,4)
year_max.vett = c(2006,2008,2010,2012,2014)


###### remember that the columns must be named!!!

dataforpar = expand.grid(S = S.vett, N = N.vett ,iter = iter.vett , Stream = Stream.vett, mort.list = mort.list.vett, year_max = year_max.vett)

dataforpar = as.matrix(do.call("rbind", rep(list(dataforpar), 100)))

y = list() # list of input values

for (i in 1:nrow(dataforpar)) {
  y[[i]] = as.list(dataforpar[i,])

     if (y[[i]]$mort.list == 1) {
       y[[i]]$mort.list = readRDS("data/uppidri_surv_06_14.list")
       y[[i]]$Stream = "UIdri_MT"
      } else if (y[[i]]$mort.list == 2) {
      y[[i]]$mort.list = readRDS("data/loidri_surv_06_14.list")
      y[[i]]$Stream = "LIdri_MT"
      } else if (y[[i]]$mort.list == 3) {
        y[[i]]$mort.list = readRDS("data/rtidri_surv_06_14.list")
        y[[i]]$Stream = "LIdri_RT"
      } else if (y[[i]]$mort.list == 4) {
        y[[i]]$mort.list = readRDS("data/uppvol_surv_06_14.list")
        y[[i]]$Stream = "UVol_BT"}
}

# I divide the simulations in chunks, so if something fails only a subset of the replicates are affected

num_sim = 1:length(y)
num_sim = matrix(num_sim, nrow = 10, byrow = T)
num_chunks = nrow(num_sim)

for (aa in 1:num_chunks) {
  res_list = mclapply(num_sim[aa,],function (x) do.call(demo_limit_sim.f,y[[x]]),mc.cores = 8)
  saveRDS(res_list, paste("res_list",aa,".rds",sep=""))
}

