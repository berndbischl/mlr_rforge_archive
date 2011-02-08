setGeneric(
  name = "plotFeatureDistrib",
  def = function(x.name, x, data, target) {
    if (missing(x))
      x = data[, x.name]
    standardGeneric("plotFeatureDistrib")
  }
)

setMethod(
  f = "plotFeatureDistrib",
  signature = signature(x.name="character", x="numeric", data="data.frame", target="character"),
  def = function(x.name, x, data, target) {
    a = aes_string(x=x.name, colour=target)
    ggplot(data, a) + 
      geom_density(size=3) + 
      geom_histogram(aes(y = ..density..), alpha=0.5)
})    

setMethod(
  f = "plotFeatureDistrib",
  signature = signature(x.name="character", x="integer", data="data.frame", target="character"),
  def = function(x.name, x, data, target) {
    a = aes_string(x=x.name, colour=target)
    ggplot(data, a) + 
      geom_density(size=3) + 
      geom_histogram(aes(y = ..density..), alpha=0.5)
})    


setMethod(
  f = "plotFeatureDistrib",
  signature = signature(x.name="character", x="logical", data="data.frame", target="character"),
  def = function(x.name, x, data, target) {
    a = aes_string(x=x.name)
    ggplot(data, a) +
      geom_histogram(aes(y=..density..)) +
      facet_wrap(as.formula(paste("~", target))) 
  })    


setMethod(
  f = "plotFeatureDistrib",
  signature = signature(x.name="character", x="factor", data="data.frame", target="character"),
  def = function(x.name, x, data, target) {
    a = aes_string(x=x.name)
    ggplot(data, a) +
      geom_histogram(aes(y=..density..)) +
      facet_wrap(as.formula(paste("~", target))) 
  })    


