##### pepXML to TSV conversion R script
### HJK

#### You only need to execute this part once for the installation
source("https://bioconductor.org/biocLite.R")
biocLite("pepXMLTab")
install.packages("reshape2")
#### /You only need to execute this part once for the installation

#### Loading Library 
library(pepXMLTab)
library(reshape2)


#### Execute the actual parsing script
# This will read all pepXML files list from the working path
pepxml_filenames <- list.files(pattern = "\\.pepXML$")
datalist = list()
i<-1
# it could've been parallelized but i didn't bother
for(each_pepxml in pepxml_filenames){
  print(each_pepxml)
  datalist[[i]] <- pepXML2tab(each_pepxml)
  i <- i + 1
}
combined_pepxml_df <- do.call(rbind, datalist)
write.csv(combined_pepxml_df, "combined_pepxml_conversion_result.csv")
# DONE! check your working folder!

