

setGeneric(
  name = "plotFeatureDistrib",
  def = function(x, x.name, df, target) {
    standardGeneric("plotFeatureDistrib")
  }
)

setMethod(
  f = "plotFeatureDistrib",
  signature = signature(x="numeric", x.name="character", df="data.frame", target="character"),
  def = function(x, x.name, df, target) {
    a = aes_string(x=x.name, colour=target)
    ggplot(df, a) + 
      geom_density(size=3) + 
      geom_histogram(aes(y = ..density..), alpha=0.5)
})    

setMethod(
  f = "plotFeatureDistrib",
  signature = signature(x="integer", x.name="character", df="data.frame", target="character"),
  def = function(x, x.name, df, target) {
    a = aes_string(x=x.name, colour=target)
    ggplot(df, a) + 
      geom_density(size=3) + 
      geom_histogram(aes(y = ..density..), alpha=0.5)
})    


setMethod(
  f = "plotFeatureDistrib",
  signature = signature(x="logical", x.name="character", df="data.frame", target="character"),
  def = function(x, x.name, df, target) {
    a = aes_string(x=x.name)
    ggplot(df, a) +
      geom_histogram(aes(y=..density..)) +
      facet_wrap(as.formula(paste("~", target))) 
  })    


setMethod(
  f = "plotFeatureDistrib",
  signature = signature(x="factor", x.name="character", df="data.frame", target="character"),
  def = function(x, x.name, df, target) {
    a = aes_string(x=x.name)
    ggplot(df, a) +
      geom_histogram(aes(y=..density..)) +
      facet_wrap(as.formula(paste("~", target))) 
  })    


