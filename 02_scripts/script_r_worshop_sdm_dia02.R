#' ---
#' titulo: oficina sdm moco
#' autor: mauricio vancine
#' data: 26-09-2021
#' ---

# prepare r -------------------------------------------------------------
# packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(rnaturalearth)) install.packages("rnaturalearth")
if(!require(nngeo)) install.packages("nngeo")
if(!require(sf)) install.packages("sf")
if(!require(tmap)) install.packages("tmap")
if(!require(mapview)) install.packages("mapview")
if(!require(raster)) install.packages("raster")
if(!require(viridis)) install.packages("viridis")
if(!require(dismo)) install.packages("dismo")
if(!require(gam)) install.packages("gam")
if(!require(kernlab)) install.packages("kernlab")
if(!require(e1071)) install.packages("e1071")
if(!require(randomForest)) install.packages("randomForest")
if(!require(sdm)) install.packages("sdm")
if(!require(ggspatial)) install.packages("ggspatial")

# install java - windows - https://www.java.com/pt_BR/download/
# install java - ubuntu - sudo apt install -y default-jre default-jdk && sudo R CMD javareconf ## rjava
if(!require(rJava)) install.packages("rJava")

# options
options(timeout = 1e5)

# maxent
download.file(url = "https://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download",
              destfile = paste0(system.file("java", package = "dismo"), "/maxent.zip"), mode = "wb")
unzip(zipfile = paste0(system.file("java", package = "dismo"), "/maxent.zip"),
      exdir = system.file("java", package = "dismo"), junkpaths = TRUE)
dir(system.file("java", package = "dismo"))

# directories
dir.create("04_modelos")

# dismo ------------------------------------------------------------------
# data ----
li <- rnaturalearth::ne_countries(scale = 50, continent = "South America", returnclass = "sf") %>%
  sf::st_union(rnaturalearth::ne_countries(scale = 50, country = "France", returnclass = "sf")) %>%
  sf::st_crop(rnaturalearth::ne_countries(continent = "South America", returnclass = "sf")) %>%
  sf::st_union() %>%
  nngeo::st_remove_holes() %>%
  sf::st_as_sf()
li

occ <- readr::read_csv("03_dados/01_ocorrencias/occ_data_filter_edit.csv")
occ

env <- dir(path = "03_dados/02_variaveis/", pattern = ".tif", full.names = TRUE) %>%
  raster::stack() %>%
  raster::brick()
env

tm_shape(env$bio02) +
  tm_raster(pal = "RdBu") +
  tm_shape(li) +
  tm_borders(col = "black") +
  tm_shape(occ %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)) +
  tm_bubbles(size = .1, col = "steelblue") +
  tm_layout(legend.position = c("right", "bottom"))

# criar mascara sem ocorrencias
pr_buffer <- occ %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_buffer(dist = 50000) %>%
  sf::st_union() %>%
  sf::as_Spatial()
pr_buffer

plot(li)
plot(pr_buffer, col = "gray", add= TRUE)

env_mask_buffer <- raster::mask(env$bio02, pr_buffer, inverse = TRUE)
env_mask_buffer

tm_shape(env_mask_buffer) +
  tm_raster() +
  tm_shape(occ %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)) +
  tm_bubbles(size = .02, col = "steelblue") +
  tm_layout(legend.show = FALSE)

# selecting presence and pseudo-absence data ----
pr_specie <- occ %>%
  dplyr::select(longitude, latitude) %>%
  dplyr::mutate(id = seq(nrow(.)))
pr_specie

set.seed(42)
pa_specie <- dismo::randomPoints(mask = env_mask_buffer, n = nrow(pr_specie)) %>%
  tibble::as_tibble() %>%
  dplyr::rename(longitude = x, latitude = y) %>%
  dplyr::mutate(id = seq(nrow(.)))
pa_specie

set.seed(42)
bkg <- dismo::randomPoints(mask = env, n = 7000, warn = FALSE) %>%
  tibble::as_tibble() %>%
  dplyr::rename(longitude = x, latitude = y)
bkg

tm_shape(li) +
  tm_borders(col = "black") +
  tm_shape(bkg %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)) +
  tm_bubbles(size = .1, col = "gray") +
  tm_shape(pr_specie %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)) +
  tm_bubbles(size = .1, col = "steelblue") +
  tm_shape(pa_specie %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)) +
  tm_bubbles(size = .1, col = "orange")

# partitioning data ----
pr_sample_train <- pr_specie %>%
  dplyr::sample_frac(.7) %>%
  dplyr::select(id) %>%
  dplyr::pull()
pr_sample_train

pa_sample_train <- pa_specie %>%
  dplyr::sample_frac(.7) %>%
  dplyr::select(id) %>%
  dplyr::pull()
pa_sample_train

# train and test data
train_pa <- dismo::prepareData(x = env,
                               p = pr_specie[pr_specie$id %in% pr_sample_train, 1:2],
                               b = pa_specie[pa_specie$id %in% pa_sample_train, 1:2])
train_pa
nrow(train_pa)
table(train_pa$pb)

train_pb <- dismo::prepareData(x = env,
                               p = pr_specie[pr_specie$id %in% pr_sample_train, 1:2],
                               b = bkg)
train_pb
nrow(train_pb)
table(train_pb$pb)

test_pa <- dismo::prepareData(x = env,
                              p = pr_specie[!pr_specie$id %in% pr_sample_train, 1:2],
                              b = pa_specie[!pa_specie$id %in% pa_sample_train, 1:2])
test_pa
nrow(test_pa)
table(test_pa$pb)

# 3. model fitting ----
# presence-only - envelope
BIO <- dismo::bioclim(x = train_pa[train_pa$pb == 1, -1])
plot(BIO, a = 1, b = 2, p = 0.85)
plot(BIO, a = 1, b = 2, p = 0.95)
plot(BIO, a = 1, b = 2, p = 1)

# presence-only - distance-based
DOM <- dismo::domain(x = train_pa[train_pa$pb == 1, -1])

# presence-only - distance-based
MAH <- dismo::mahal(x = train_pa[train_pa$pb == 1, -1])

# presence-absence - statistics
GLM <- glm(formula = pb ~ ., data = train_pa, family = "binomial")
GLM
summary(GLM)

GAM <- gam::gam(formula = as.formula(paste0("pb", "~", paste0("gam::s(", colnames(train_pa)[-1], ")", collapse = "+"))),
                family = "binomial", data = train_pa, warning = FALSE)
GAM
summary(GAM)

# presence-absence - machine learning
RFR <- randomForest::randomForest(formula = pb ~ ., data = train_pa)

SVM <- e1071::svm(formula = pb ~ ., data = train_pa)

# presence-background
Sys.setenv(NOAWT = TRUE)
MAX <- dismo::maxent(x = train_pb[, -1], p = train_pb[, 1])
MAX
plot(MAX)

# 4. evaluation ----
# eval bioclim
eval_BIO <- dismo::evaluate(p = test_pa[test_pa$pb == 1, -1],
                            a = test_pa[test_pa$pb == 0, -1],
                            model = BIO)
eval_BIO
plot(eval_BIO, "ROC")

dismo::threshold(eval_BIO, "spec_sens")
id_eval_spec_sens_BIO <- which(eval_BIO@t == dismo::threshold(eval_BIO, "spec_sens"))
tss_spec_sens_BIO <- eval_BIO@TPR[id_eval_spec_sens_BIO] + eval_BIO@TNR[id_eval_spec_sens_BIO] - 1
tss_spec_sens_BIO

# eval domain
eval_DOM <- dismo::evaluate(p = test_pa[test_pa$pb == 1, -1],
                            a = test_pa[test_pa$pb == 0, -1],
                            model = DOM)
eval_DOM
plot(eval_DOM, "ROC")

dismo::threshold(eval_DOM, "spec_sens")
id_eval_spec_sens_DOM <- which(eval_DOM@t == dismo::threshold(eval_DOM, "spec_sens"))
tss_spec_sens_DOM <- eval_DOM@TPR[id_eval_spec_sens_DOM] + eval_DOM@TNR[id_eval_spec_sens_DOM] - 1
tss_spec_sens_DOM

# eval mahalanobis
eval_MAH <- dismo::evaluate(p = test_pa[test_pa$pb == 1, -1],
                            a = test_pa[test_pa$pb == 0, -1],
                            model = MAH)
eval_MAH
plot(eval_MAH, "ROC")

dismo::threshold(eval_MAH, "spec_sens")
id_eval_spec_sens_MAH <- which(eval_MAH@t == dismo::threshold(eval_MAH, "spec_sens"))
tss_spec_sens_MAH <- eval_MAH@TPR[id_eval_spec_sens_MAH] + eval_MAH@TNR[id_eval_spec_sens_MAH] - 1
tss_spec_sens_MAH

# eval glm
eval_GLM <- dismo::evaluate(p = test_pa[test_pa$pb == 1, -1],
                            a = test_pa[test_pa$pb == 0, -1],
                            model = GLM)
eval_GLM
plot(eval_GLM, "ROC")

dismo::threshold(eval_GLM, "spec_sens")
id_eval_spec_sens_GLM <- which(eval_GLM@t == dismo::threshold(eval_GLM, "spec_sens"))
tss_spec_sens_GLM <- eval_GLM@TPR[id_eval_spec_sens_GLM] + eval_GLM@TNR[id_eval_spec_sens_GLM] - 1
tss_spec_sens_GLM

# eval gam
eval_GAM <- dismo::evaluate(p = test_pa[test_pa$pb == 1, -1],
                            a = test_pa[test_pa$pb == 0, -1],
                            model = GAM)
eval_GAM
plot(eval_GAM, "ROC")

dismo::threshold(eval_GAM, "spec_sens")
id_eval_spec_sens_GAM <- which(eval_GAM@t == dismo::threshold(eval_GAM, "spec_sens"))
tss_spec_sens_GAM <- eval_GAM@TPR[id_eval_spec_sens_GAM] + eval_GAM@TNR[id_eval_spec_sens_GAM] - 1
tss_spec_sens_GAM

# eval random forest
eval_RFR <- dismo::evaluate(p = test_pa[test_pa$pb == 1, -1],
                            a = test_pa[test_pa$pb == 0, -1],
                            model = RFR)
eval_RFR
plot(eval_RFR, "ROC")

dismo::threshold(eval_RFR, "spec_sens")
id_eval_spec_sens_RFR <- which(eval_RFR@t == dismo::threshold(eval_RFR, "spec_sens"))
tss_spec_sens_RFR <- eval_RFR@TPR[id_eval_spec_sens_RFR] + eval_RFR@TNR[id_eval_spec_sens_RFR] - 1
tss_spec_sens_RFR

# eval svm
eval_SVM <- dismo::evaluate(p = test_pa[test_pa$pb == 1, -1],
                            a = test_pa[test_pa$pb == 0, -1],
                            model = SVM)
eval_SVM
plot(eval_SVM, "ROC")

dismo::threshold(eval_SVM, "spec_sens")
id_eval_spec_sens_SVM <- which(eval_SVM@t == dismo::threshold(eval_SVM, "spec_sens"))
tss_spec_sens_SVM <- eval_SVM@TPR[id_eval_spec_sens_SVM] + eval_SVM@TNR[id_eval_spec_sens_SVM] - 1
tss_spec_sens_SVM

# eval maxent
eval_MAX <-dismo::evaluate(p = test_pa[test_pa$pb == 1, -1],
                           a = test_pa[test_pa$pb == 0, -1],
                           model = MAX)
eval_MAX
plot(eval_MAX, "ROC")

dismo::threshold(eval_MAX, "spec_sens")
id_eval_spec_sens_MAX <- which(eval_MAX@t == dismo::threshold(eval_MAX, "spec_sens"))
tss_spec_sens_MAX <- eval_MAX@TPR[id_eval_spec_sens_MAX] + eval_MAX@TNR[id_eval_spec_sens_MAX] - 1
tss_spec_sens_MAX

# evaluations
eval <- tibble::tibble(method = c("BIO", "DOM", "MAH", "GLM", "GAM", "RFR", "SVM", "MAX"),
                       auc = c(eval_BIO@auc, eval_DOM@auc, eval_MAH@auc, eval_GLM@auc, eval_GAM@auc, eval_RFR@auc, eval_SVM@auc, eval_MAX@auc),
                       tss = c(tss_spec_sens_BIO, tss_spec_sens_DOM, tss_spec_sens_MAH, tss_spec_sens_GLM, tss_spec_sens_GAM, tss_spec_sens_RFR,
                               tss_spec_sens_SVM, tss_spec_sens_MAX),
                       thr = c(dismo::threshold(eval_BIO, "spec_sens"), dismo::threshold(eval_DOM, "spec_sens"), dismo::threshold(eval_MAH, "spec_sens"),
                               dismo::threshold(eval_GLM, "spec_sens"), dismo::threshold(eval_GAM, "spec_sens"), dismo::threshold(eval_RFR, "spec_sens"),
                               dismo::threshold(eval_SVM, "spec_sens"), dismo::threshold(eval_MAX, "spec_sens")))
eval

# 5.  predict ----
# bioclim
model_predict_bio <- dismo::predict(env, BIO, progress = "text")
model_predict_bio

model_predict_bio_thr <- model_predict_bio >= eval[1, ]$thr
model_predict_bio_thr

plot(model_predict_bio_thr * model_predict_bio)

plot(model_predict_bio, col = viridis::turbo(100), main = "BIOCLIM - Contínuo")
plot(model_predict_bio_thr, col = c("gray", "blue"), main = "BIOCLIM - Binário")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

# domain
model_predict_dom <- dismo::predict(env, DOM, progress = "text")
model_predict_dom

model_predict_dom_thr <- model_predict_dom >= eval[2, ]$thr
model_predict_dom_thr

plot(model_predict_dom, col = viridis::turbo(100), main = "DOMAIN - Contínuo")
plot(model_predict_dom_thr, col = c("gray", "blue"), main = "DOMAIN - Binário")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

# mahalanobis
model_predict_mah <- dismo::predict(env, MAH, progress = "text")
model_predict_mah

model_predict_mah_thr <- model_predict_mah >= eval[3, ]$thr
model_predict_mah_thr

plot(model_predict_mah, col = viridis::turbo(100), main = "Mahalanobis - Contínuo")
plot(model_predict_mah_thr, col = c("gray", "blue"), main = "Mahalanobis - Binário")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

# glm
model_predict_glm <- dismo::predict(env, GLM, progress = "text")
model_predict_glm

model_predict_glm_thr <- model_predict_glm >= eval[4, ]$thr
model_predict_glm_thr

plot(model_predict_glm, col = viridis::turbo(100), main = "GLM - Contínuo")
plot(model_predict_glm_thr, col = c("gray", "blue"), main = "GLM - Binário")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

# gam
model_predict_gam <- dismo::predict(env, GAM, progress = "text")
model_predict_gam

model_predict_gam_thr <- model_predict_gam >= eval[5, ]$thr
model_predict_gam_thr

plot(model_predict_gam, col = viridis::turbo(100), main = "GAM - Contínuo")
plot(model_predict_gam_thr, col = c("gray", "blue"), main = "GAM - Binário")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

# random forest
model_predict_rfr <- dismo::predict(env, RFR, progress = "text", type = "response")
model_predict_rfr

model_predict_rfr_thr <- model_predict_rfr >= eval[6, ]$thr
model_predict_rfr_thr

plot(model_predict_rfr, col = viridis::turbo(100), main = "Random Forest - Contínuo")
plot(model_predict_rfr_thr, col = c("gray", "blue"), main = "Random Forest - Binário")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

# svm
model_predict_svm <- dismo::predict(env, SVM, progress = "text", type = "response")
model_predict_svm

model_predict_svm_thr <- model_predict_svm >= eval[7, ]$thr
model_predict_svm_thr

plot(model_predict_svm, col = viridis::turbo(100), main = "SVM - Contínuo")
plot(model_predict_svm_thr, col = c("gray", "blue"), main = "SVM - Binário")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

# maxent
model_predict_max <- dismo::predict(env, MAX, progress = "text", type = "response")
model_predict_max

model_predict_max_thr <- model_predict_max >= eval[8, ]$thr
model_predict_max_thr

plot(model_predict_max, col = viridis::turbo(100), main = "MaxEnt - Contínuo")
plot(model_predict_max_thr, col = c("gray", "blue"), main = "MaxEnt - Binário")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

# 6. ensembles ----
# models ----
models_cont <- raster::stack(model_predict_bio, model_predict_dom, model_predict_mah,
                             model_predict_glm, model_predict_gam, model_predict_rfr,
                             model_predict_svm, model_predict_max)
models_cont

models_bin <- raster::stack(model_predict_bio_thr, model_predict_dom_thr, model_predict_mah_thr,
                            model_predict_glm_thr, model_predict_gam_thr, model_predict_rfr_thr,
                            model_predict_svm_thr, model_predict_max_thr)
models_bin

# frequency ----
ens_freq <- sum(models_bin)/nlayers(models_bin)
ens_freq
plot(ens_freq, col = viridis::turbo(100), main = "Ensemble - Frequência")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")
points(occ$longitude, occ$latitude, cex = .7, pch = 20, col = "black")

# standard deviation
ens_freq_sd <- raster::calc(models_bin, sd)
ens_freq_sd
plot(ens_freq_sd, col = viridis::turbo(100), main = "Ensemble - Frequência - Desvio padrão")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

par(mfrow = c(1, 2))
plot(ens_freq, col = viridis::turbo(100), main = "Ensemble - Frequência")
plot(ens_freq_sd, col = viridis::turbo(100), main = "Ensemble - Frequência - Desvio padrão")
dev.off()

# mean ----
ens_mean <- mean(models_cont)
ens_mean
plot(ens_mean, col = viridis::turbo(100), main = "Ensemble - Frequência")

models_cont_pad <- raster::stack()
for(i in 1:nlayers(models_cont)){

  models_cont_pad <- raster::stack(models_cont_pad, climateStability::rescale0to1(models_cont[[i]]))

}
models_cont_pad

# mean
ens_mean_pad <- mean(models_cont_pad)
ens_mean_pad
plot(ens_mean_pad, col = viridis::turbo(100), main = "Ensemble - Média")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

# standard deviation
ens_mean_pad_sd <- raster::calc(models_cont_pad, sd)
ens_mean_pad_sd
plot(ens_mean_pad_sd, col = viridis::turbo(100), main = "Ensemble - Média - Desvio padrão")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

# weighted mean
w_auc <- (eval$auc-0.5)^2 # retirar 0.5 (aleatorio) e eleva ao quadrado para valores muito altos
ens_wei_mean_auc <- weighted.mean(models_cont_pad, w_auc)
plot(ens_wei_mean_auc, col = viridis::turbo(100), main = "Ensemble - Média pondera pelo AUC")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

w_tss <- (eval$tss-0.5)^2 # retirar 0.5 (aleatorio) e eleva ao quadrado para valores muito altos
ens_wei_mean_tss <- weighted.mean(models_cont_pad, w_tss)
plot(ens_wei_mean_tss, col = viridis::turbo(100), main = "Ensemble - Média pondera pelo AUC")
points(occ$longitude, occ$latitude, pch = 20, col = "steelblue")

par(mfrow = c(2, 2))
plot(ens_freq, col = viridis::turbo(100), main = "Ensemble - Frequência")
plot(ens_mean_pad, col = viridis::turbo(100), main = "Ensemble - Média")
plot(ens_wei_mean_auc, col = viridis::turbo(100), main = "Ensemble - Média ponderada AUC")
plot(ens_wei_mean_tss, col = viridis::turbo(100), main = "Ensemble - Média ponderada TSS")
dev.off()

# sdm ---------------------------------------------------------------------
# 1. import data ----
# occurrence
occ <- readr::read_csv("03_dados/01_ocorrencias/occ_data_filter_edit.csv")

occ_sp <- occ %>%
  dplyr::mutate(species = 1)
coordinates(occ_sp) <- c("longitude", "latitude")
occ_sp

# variables
env <- dir(path = "03_dados/02_variaveis", pattern = ".tif", full.names = TRUE) %>%
  raster::stack() %>%
  raster::brick()
env

# 2. data preparation -----------------------------------------------------
# prepare data
sdm_data <- sdm::sdmData(formula = species~.,
                         train = occ_sp,
                         predictors = env,
                         bg = list(n = nrow(occ),
                                   method = "gRandom", # "eRandom"
                                   remove = TRUE))
sdm_data

# 3. model fitting --------------------------------------------------------
# methods
sdm::getmethodNames()
sdm::getmethodNames() %>% names()

# parallel
parallel::detectCores()

# fit
sdm_fit <- sdm::sdm(species ~ .,
                    data = sdm_data,
                    replication = "subsampling",
                    n = 5,
                    test.percent = 30,
                    parallelSetting = list(ncores = 3, method = "parallel"),
                    methods = c(
                      #"bioclim",
                      "bioclim.dismo",
                      #"brt",
                      "domain.dismo",
                      #"fda",
                      "gam",
                      "glm",
                      #"glmnet",
                      #"mahal.dismo",
                      #"mars",
                      "maxent",
                      #"maxlike",
                      #"mda",
                      #"rpart",
                      "rf",
                      "svm"
                    ))
sdm_fit


# information
sdm::getModelInfo(sdm_fit)

# 4. assessment of model -------------------------------------------------
# evaluation
sdm::getEvaluation(sdm_fit, opt = 2)

# roc
x11()
sdm::roc(sdm_fit)
sdm::roc(sdm_fit, method = "maxent")
sdm::roc(sdm_fit, method = "glm", smooth = TRUE)

# response curve
sdm::rcurve(sdm_fit) + theme_bw()
sdm::rcurve(sdm_fit, id = 21:25) + theme_bw()

# variable importance
sdm::getVarImp(sdm_fit)
sdm::getVarImp(sdm_fit, method = "glm")
sdm::getVarImp(sdm_fit, method = "maxent")

sdm_var_import <- sdm::getVarImp(sdm_fit)
sdm_var_import

plot(sdm_var_import) + theme_bw()

# 5. predictions -----------------------------------------------------------
# predict
sdm_predict <- predict(object = sdm_fit,
                       newdata = env,
                       filename = "04_modelos/predict.tif",
                       mean = TRUE,
                       nc = 3,
                       overwrite = TRUE)
sdm_predict

# names
names(sdm_predict)

# map
plot(sdm_predict, col = viridis::turbo(100))

# 6. ensemble ----------------------------------------------------------------
# ensemble
ens <- sdm::ensemble(x = sdm_fit,
                     newdata = env,
                     filename = "04_modelos/ensemble.tif",
                     parallelSetting = list(ncores = 3, method = "parallel"),
                     setting = list(
                       method = "weighted",
                       stat = "TSS",
                       opt = 2
                     ))
ens
plot(ens, col = viridis::turbo(100))

# uncertainty
unc <- sdm::ensemble(x = sdm_fit,
                     newdata = env,
                     filename = "04_modelos/uncertainty.tif",
                     setting = list(
                       #id = c(1:14, 19:32),
                       method = "uncertainty",
                       stat = "TSS",
                       opt = 2
                     ))
unc
plot(unc, col = viridis::turbo(100))

# 7. threshold ---------------------------------------------------------------
# evaluations
evaluation <- sdm::getEvaluation(sdm_fit, stat = c("AUC", "TSS", "threshold"), opt = 2)
evaluation

# threshold max(se+sp)
thr <- mean(evaluation$threshold)
thr

# cut
ens_thr <- ens >= thr
ens_thr

# plot
plot(ens_thr, col = c("gray", "blue"))

# 8. maps -----------------------------------------------------------------
# data
da <- raster::rasterToPoints(ens) %>%
  tibble::as_tibble()
da

map_sui <- ggplot() +
  geom_raster(data = da, aes(x, y, fill = ensemble)) +
  geom_sf(data = li, fill = NA, color = "gray30") +
  geom_point(data = occ, aes(x = longitude, y = latitude)) +
  scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
  scale_fill_gradientn(colours = viridis::turbo(100)) +
  coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
  labs(x = "Longitude", y = "Latitude", fill = "Suitability") +
  annotation_scale(location = "br", width_hint = .3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(.3, "cm"), pad_y = unit(.3, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = "white",
                                         size = 0.3,
                                         linetype = "solid",
                                         colour = "black"),
        axis.title = element_text(size = 12, face = "plain"),
        legend.position = c(.85, .17))
map_sui

# data potential
# threshold
da_thr <- ens_thr %>%
  raster::rasterToPoints() %>%
  tibble::as_tibble()
da_thr

map_sui_thr <- ggplot() +
  geom_raster(data = da_thr, aes(x, y, fill = layer)) +
  geom_sf(data = li, fill = NA, color = "gray30") +
  geom_point(data = occ, aes(x = longitude, y = latitude)) +
  scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
  scale_fill_gradientn(colours = c("gray", "blue")) +
  coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
  labs(x = "Longitude", y = "Latitude", fill = "Suitability") +
  annotation_scale(location = "br", width_hint = .3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(.3, "cm"), pad_y = unit(.3, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = "white",
                                         size = 0.3,
                                         linetype = "solid",
                                         colour = "black"),
        axis.title = element_text(size = 12, face = "plain"),
        legend.position = c(.85, .17))
map_sui_thr

# end ---------------------------------------------------------------------
