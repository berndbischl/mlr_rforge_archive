source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.setup(level="warn")
parallel.setup(level="local")

#library(mlbench)
#data(Sonar)



#source("c:/readarff2.r")
#source("c:/datachars.r")


urls <- c("http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/anneal.ORIG.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/anneal.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/arrhythmia.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/audiology.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/autos.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/balance-scale.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/breast-cancer.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/breast-w.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/bridges_version1.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/bridges_version2.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/car.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/cmc.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/colic.ORIG.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/colic.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/credit-a.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/credit-g.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/cylinder-bands.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/dermatology.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/diabetes.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/ecoli.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/glass.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/haberman.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/hayes-roth_test.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/hayes-roth_train.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/heart-c.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/heart-h.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/heart-statlog.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/hepatitis.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/hypothyroid.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/ionosphere.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/iris.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/kdd_JapaneseVowels_test.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/kdd_JapaneseVowels_train.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/kdd_synthetic_control.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/kr-vs-kp.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/labor.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/letter.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/liver-disorders.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/lung-cancer.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/lymph.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/mfeat-factors.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/mfeat-fourier.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/mfeat-karhunen.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/mfeat-morphological.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/mfeat-pixel.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/mfeat-zernike.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/molecular-biology_promoters.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/mushroom.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/nursery.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/optdigits.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/page-blocks.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/pendigits.arff",		       
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/postoperative-patient-data.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/primary-tumor.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/segment.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/shuttle-landing-control.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/sick.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/solar-flare_1.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/solar-flare_2.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/sonar.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/soybean.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/spambase.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/spect_test.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/spect_train.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/spectf_test.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/spectf_train.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/spectrometer.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/splice.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/tae.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/tic-tac-toe.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/trains.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/vehicle.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/vote.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/vowel.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/waveform-5000.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/wine.arff",
		"http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff/zoo.arff"
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
removes[["solar-flare_1.arff"]] = c("C-class_flares_production_by_this_region", "M-class_flares_production_by_this_region")
removes[["solar-flare_2.arff"]] = c("C-class_flares_production_by_this_region", "M-class_flares_production_by_this_region")
removes[["bridges_version1.arff"]] = "IDENTIF"
removes[["bridges_version2.arff"]] = "IDENTIF"
removes[["flags.arff"]] = "name"
removes[["kdd_synthetic_control.arff"]] = "index"
removes[["solar-flare_1.arff"]] = c("C-class_flares_production_by_this_region","M-class_flares_production_by_this_region")
removes[["solar-flare_2.arff"]] = c("C-class_flares_production_by_this_region","M-class_flares_production_by_this_region")
removes[["molecular-biology_promoters.arff"]] = "index"
removes[["spectrometer.arff"]] = "LRS-name"
removes[["splice.arff"]] = "Instance_name"


targets = list()
targets[["spect_train.arff"]] = "OVERALL_DIAGNOSIS"
targets[["spect_test.arff"]] = "OVERALL_DIAGNOSIS"
targets[["shuttle-landing-control.arff"]] = "Class"
targets[["lung-cancer.arff"]] = "class"
targets[["spectf_train.arff"]] = "OVERALL_DIAGNOSIS"
targets[["spectf_test.arff"]] = "OVERALL_DIAGNOSIS"
targets[["molecular-biology_promoters.arff.arff"]] = "class"
targets[["spectrometer.arff"]] = "LRS-class"
targets[["whine.arff"]] = "class"

for(i in 1:length(urls)){
	dn = ds.names[i]
	print(urls[i])
	if (dn %in% names(removes))
		remove = removes[[dn]]
	else
		remove = character(0)
	data <- read.arff(urls[i], remove=remove)
	
	if (dn %in% names(targets))
		dc = data.chars(data, dn, target=targets[[dn]])
	else
		dc = data.chars(data, dn)
	
	d.chars = rbind(d.chars, dc)
}

cs = c("ds", "n.obs", "n.inputs", "q.maxminclass", "n.narows", "n.nacols")
d.chars[d.chars$n.classes==2, cs]


