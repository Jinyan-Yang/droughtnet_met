path.nm <- 'download/ornl/'
fn.vec <- list.files(path.nm,'.csv')
# # the file has inconsistent veg type in the file names and meta file
# # I'll stick to what's used in the filenames
# find.gr.func <- function(input.nm){
#   path.nm <- 'download/ornl'
#   fn <- paste0(path.nm,'/',input.nm)
#   see <- readLines(fn)
#   veg.type <- gsub("primary_veg_type: ","",
#                    see[grep("primary_veg_type", see)])
#   veg.type <- gsub(' ','',veg.type)
#   if(veg.type == 'GR') return(fn)
# }
# 
# tem <- sapply(fn.vec,find.gr.func,simplify = "array")
# 
# library(rlist)
# gr.fn.vec <- unlist(list.clean(tem, fun = is.null, recursive = FALSE))
# 
# gr.site.mn.vec <- gsub('_meta.txt','',names(gr.fn.vec))
# 
# see <- read.csv(paste0(path.nm,'/',gr.site.mn.vec[1],'_GR_3000_1day.csv'),skip = 23,header = T)

gr.csv.fn.vec <- Filter(function(x) grepl("GR_1000_1day.csv", x), fn.vec)
gr.site.vec <- gsub('_GR_1000_1day.csv','',(gr.csv.fn.vec))
# 
temp <- list()
for (i in seq_along(gr.csv.fn.vec)){
  temp[[i]] <- read.csv(paste0(path.nm,gr.csv.fn.vec[i]),skip = 24,header = T)
  temp[[i]]$site <- gr.site.vec[i]
  temp[[i]]$date <- as.Date(as.character(temp[[i]]$date,'%Y-%m-%d'))
}

gr.gcc.df$site.factor <- as.factor(gr.gcc.df$site)
gr.gcc.df <- do.call(rbind,temp)
gr.gcc.df$gcc_mean

library(RColorBrewer)
n <- 69
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot(gcc_mean~date,data = gr.gcc.df,
     col = col_vector[69],pch=16,type='l')
