source("src/log.r")

logger.setup(level="debug", file="c:/log.txt")
warning("blubb")


stop("stop!!!")
logger.warn("dffd")


