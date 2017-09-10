source("limit_sim.r")
test = pp_sim.f(S = 1000,N = 500,iter = 110, Stream = "UIdri_MT",  var.env = 1,mort.list = uppidri_surv_06_14.list)
S = 1000
N = 300
iter = 30
num.loci = 5
num.alleles = 2
sd.alleles = 0.1
recomb = 1
mat_prop = rep(0.9,7)
age_repr = 3:9
alpha_stock = 50
Rp_stock = 5000
L_inf = 3750
k_vb = 0.057
t0_vb = -0.21
alpha_w = 1.44
beta_w = 3.12
sd_e = 0.2
Stream = "LIdri_MT"
stock.r.mod = stock.r.mod
mort.df = loidri_surv_14.df


######

S.vett = 1000
N.vett = 500
iter.vett = 110
num.loci.vett = 2
num.alleles.vett = 2
sd.alleles.vett = 0.05
recomb.vett = 0
L_inf.vett = 3750
k_vb.vett = 0.057
t0_vb.vett = -0.21
alpha_w.vett = 1.44
beta_w.vett = 3.12
sd_e.vett = 0.2
Stream.vett = c(1)
var.env.vett = 1
mort.list.vett = 1
year_max.vett = c(2006,2008,2010,2012,2014)
  
 # loidri_surv_14.df


###### remember that the columns must be named!!!

dataforpar = expand.grid(S = S.vett, N = N.vett ,iter = iter.vett , num.loci = num.loci.vett,num.alleles = num.alleles.vett,sd.alleles = sd.alleles.vett ,recomb = recomb.vett,L_inf = L_inf.vett, k_vb = k_vb.vett,t0_vb = t0_vb.vett, alpha_w = alpha_w.vett,beta_w = beta_w.vett,sd_e = sd_e.vett, Stream = Stream.vett,  var.env = var.env.vett,mort.list = mort.list.vett, year_max = year_max.vett)

dataforpar = as.matrix(do.call("rbind", rep(list(dataforpar), 100)))

# x = dataforpar
# test = convertRowsToList(x, name.list = TRUE, name.vector = FALSE,
#                   factors.as.char = TRUE, as.vector = TRUE)


y = list()

for (i in 1:nrow(dataforpar)) {
  y[[i]] = as.list(dataforpar[i,])
  y[[i]]$Stream = "UIdri_MT"
  y[[i]]$mort.list = uppidri_surv_06_14.list
#   if (y[[i]]$mort.df == 1) {
#     y[[i]]$mort.df = uppidri_surv_14.df} else {y[[i]]$mort.df = uppidri_surv_08.df}
}



# test = as.list(dataforpar[1,])
# test = lapply(seq_len(nrow(x)), function(i) x[i,])


prova = mclapply(1:length(y),function (x) do.call(pp_sim.f,y[[x]]),
         mc.cores=6)

#uppidri_sim = prova

uppidri_sim = prova
saveRDS(uppidri_sim,"uppidri_sim.RDS")




dataforpar = expand.grid(S = S.vett, N = N.vett ,iter = iter.vett , num.loci = num.loci.vett,num.alleles = num.alleles.vett,sd.alleles = sd.alleles.vett ,recomb = recomb.vett,L_inf = L_inf.vett, k_vb = k_vb.vett,t0_vb = t0_vb.vett, alpha_w = alpha_w.vett,beta_w = beta_w.vett,sd_e = sd_e.vett, Stream = Stream.vett,  var.env = var.env.vett,mort.df = mort.df.vett )

dataforpar = as.matrix(do.call("rbind", rep(list(dataforpar), 200)))

# x = dataforpar
# test = convertRowsToList(x, name.list = TRUE, name.vector = FALSE,
#                   factors.as.char = TRUE, as.vector = TRUE)


y = list()

for (i in 1:nrow(dataforpar)) {
  y[[i]] = as.list(dataforpar[i,])
  y[[i]]$Stream = "LIdri_MT"
  if (y[[i]]$mort.df == 1) {
    y[[i]]$mort.df = loidri_surv_14.df} else {y[[i]]$mort.df = loidri_surv_08.df}
}



# test = as.list(dataforpar[1,])
# test = lapply(seq_len(nrow(x)), function(i) x[i,])


prova = mclapply(1:length(y),function (x) do.call(pp_sim.f,y[[x]]),
                 mc.cores=4)

#uppidri_sim = prova

loidri_sim = prova
saveRDS(loidri_sim,"loidri_sim.RDS")

# tomaint = c(1,2,9,10,13:17)
ris.tot = c(loidri_sim, uppidri_sim)
ris.tot = unlist(ris.tot,recursive = F)
ris.tot = prova
ris.df = data.frame(
  ext = sapply(ris.tot, with, extinct), 
  stream = sapply(ris.tot, with, Stream),
  quasi_ext = sapply(ris.tot, with, quasi_ext),
  dens_mean = sapply(ris.tot, with, dens_mean),
  dens_cv = sapply(ris.tot, with, dens_cv),
  year_max = sapply(ris.tot, with, year_max)
  
)

ris.sum = ris.df %>%
  group_by(stream, year_max) %>%
  summarise(n = n(),
               mean_cv = mean(dens_cv),
               sd_cv = sd(dens_cv),
               mean_mean = mean(dens_mean),
               sd_mean = sd(dens_mean),
               quasi_prop = mean(quasi_ext))