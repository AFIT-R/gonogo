#' Title
#'
#' @param bl 
#' @param ul 
#'
#' @return
#' @export
#'
#' @examples
calcblim <-
function(bl,ul)
{
# Calculate limits on plot 1 based on just bounded (or combined) regions
mlim=slim=numeric(0);
num=length(bl);
if(length(bl[[1]][[1]]) != 1)
{
for(k in 1:num){
bk=bl[[k]]; mlim=range(c(mlim,bk[[1]])); slim=range(c(slim,bk[[2]]));
}
}
num=length(ul);
if(length(ul[[1]][[1]]) != 1){
for(k in 1:num){
bk=ul[[k]]; mlim=range(c(mlim,bk[[1]],bk[[3]])); slim=range(c(slim,bk[[2]],bk[[4]]));
}
}
a=list(mlim,slim);
names(a)=c("mlim","slim");
return(a)
}
