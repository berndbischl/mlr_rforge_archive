source("src/log.r")

logger.define(level="debug", file="c:/log.txt")
warning("blubb")


stop("stop!!!")
logger.warn("dffd")


