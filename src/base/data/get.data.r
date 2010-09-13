library(foreign)


##www.statistik.tu-dortmund.de/download/datasets/UCI/arff
## http://repository.seasr.org/Datasets/UCI/arff/

urls <- c("http://repository.seasr.org/Datasets/UCI/arff/anneal.ORIG.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/anneal.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/arrhythmia.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/audiology.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/autos.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/balance-scale.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/breast-cancer.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/breast-w.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/bridges_version1.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/bridges_version2.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/car.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/cmc.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/colic.ORIG.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/colic.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/credit-a.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/credit-g.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/cylinder-bands.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/dermatology.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/diabetes.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/ecoli.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/glass.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/haberman.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/hayes-roth_test.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/hayes-roth_train.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/heart-c.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/heart-h.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/heart-statlog.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/hepatitis.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/hypothyroid.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/ionosphere.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/iris.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/kdd_JapaneseVowels_test.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/kdd_JapaneseVowels_train.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/kdd_synthetic_control.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/kr-vs-kp.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/labor.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/letter.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/liver-disorders.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/lung-cancer.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/lymph.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/mfeat-factors.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/mfeat-fourier.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/mfeat-karhunen.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/mfeat-morphological.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/mfeat-pixel.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/mfeat-zernike.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/molecular-biology_promoters.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/mushroom.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/nursery.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/optdigits.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/page-blocks.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/pendigits.arff",            
    "http://repository.seasr.org/Datasets/UCI/arff/postoperative-patient-data.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/primary-tumor.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/segment.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/shuttle-landing-control.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/sick.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/solar-flare_1.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/solar-flare_2.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/sonar.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/soybean.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/spambase.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/spect_test.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/spect_train.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/spectf_test.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/spectf_train.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/spectrometer.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/splice.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/tae.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/tic-tac-toe.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/trains.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/vehicle.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/vote.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/vowel.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/waveform-5000.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/wine.arff",
    "http://repository.seasr.org/Datasets/UCI/arff/zoo.arff"
)

# flags.arff -> Klassenattribut unklar
# kdd_SyskillWebert-Bands.arff-> nicht einlesbar
# kdd_SyskillWebert-BioMedical.arff -> nicht einlesbar
# kdd_SyskillWebert-Goats.arff -> nicht einlesbar
# kdd_SyskillWebert-Sheep.arff -> nicht einlesbar   
# kdd_UNIX_user_data.arff -> nicht einlesbar
# kdd_internet_usage.arff -> Id-Spalte und Klassenattribut ist nicht am Zeilenende
# kdd_ipums_la_97-small.arff -> Eine Variable mit einer Ausprägung und Klassenattribt ist unklar
# kdd_ipums_la_98-small.arff -> Eine Variable mit einer Ausprägung und Klassenattribt ist unklar
# kdd_ipums_la_99-small.arff -> Eine Variable mit einer Ausprägung und Klassenattribt ist unklar
# sponge.arff -> Id-Spalte und mehrere Klassenattribute wählbar


ds.names <- sapply(strsplit(urls, "/"), function(x) x[length(x)])

d.chars <- data.frame()

removes = list()
removes[["bridges_version1.arff"]] = "IDENTIF"
removes[["bridges_version2.arff"]] = "IDENTIF"
removes[["flags.arff"]] = "name"
removes[["kdd_synthetic_control.arff"]] = "index"
removes[["molecular-biology_promoters.arff"]] = "instance"      # index in instance geändert
removes[["solar-flare_1.arff"]] = c("C-class_flares_production_by_this_region", "M-class_flares_production_by_this_region")
removes[["solar-flare_2.arff"]] = c("C-class_flares_production_by_this_region", "M-class_flares_production_by_this_region")
removes[["segment.arff"]] = c("region-centroid-col", "region-centroid-row", "region-pixel-count")   # col und row number des Pixels und region-pixel-count (konstant) 
removes[["spectrometer.arff"]] = "LRS-name"
removes[["splice.arff"]] = "Instance_name"

targets = list()
targets[["lung-cancer.arff"]] = "class"
targets[["molecular-biology_promoters.arff"]] = "class"         # "molecular-biology_promoters.arff.arff": ein .arff gestrichen
targets[["shuttle-landing-control.arff"]] = "Class"
targets[["spect_train.arff"]] = "OVERALL_DIAGNOSIS"
targets[["spect_test.arff"]] = "OVERALL_DIAGNOSIS"
targets[["spectf_train.arff"]] = "OVERALL_DIAGNOSIS"            # test in train geändert
targets[["spectf_test.arff"]] = "OVERALL_DIAGNOSIS"
targets[["spectrometer.arff"]] = "LRS-class"
targets[["wine.arff"]] = "class"                                # whine in wine geändert
#targets[["bridges_version1.arff"]] = c("MATERIAL", "REL-L", "SPAN", "T-OR-D", "TYPE")
#targets[["bridges_version2.arff"]] = c("MATERIAL", "REL-L", "SPAN", "T-OR-D", "TYPE")
#targets[["flags.arff"]] = c("landmass", "zone", "language", "religion")   # keep option makes sense here
#targets[["solar-flare_1.arff"]] = c("C-class_flares_production_by_this_region", "M-class_flares_production_by_this_region", "X-class_flares_production_by_this_region") # exclude or remove
#targets[["solar-flare_2.arff"]] = c("C-class_flares_production_by_this_region", "M-class_flares_production_by_this_region", "X-class_flares_production_by_this_region") # exclude or remove

for(i in 1:length(urls)){
  dn = ds.names[i]
  print(urls[i])
  if (dn %in% names(removes))
    remove = removes[[dn]]
  else
    remove = character(0)
  data <- read.arff2(urls[i], remove=remove)
  
  if (dn %in% names(targets))
    dc = data.chars(data, dn, target=targets[[dn]])
  else
    dc = data.chars(data, dn)
  
  d.chars = rbind(d.chars, dc)
}

cs = c("ds", "n.obs", "n.classes", "q.maxminclass")
d.chars[d.chars$n.classes==2, cs]


d.chars[d.chars$n.obs < 8000 & d.chars$n.minclass > 5, c("ds", "n.inputs", "n.obs", "n.nas", "n.classes", "q.maxminclass", "n.minclass")]
