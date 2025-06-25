################
## Exercise 2 ##
################

rm(list=ls());

folder.fastSimcoal2 <- "/home/difronzo/";
exe <- "fsc28"
args <- c("-i", paste(folder.fastSimcoal2,"DemographicModelSplitR.par", sep=""), "-x", "-s0", "-d", "-n", "1", "-q", "-G")

setwd(folder.fastSimcoal2);

# Exercise 2)
# Produce the par file for fastSimcoal2. Generate number_of_blocks 
# independent genomic regions, each of 1 Mb. Assume a mutation rate of 1.6*10^-7 and recombination rate of 10^-8.
# a) assuming a constant population size A. Sample 100 chromosomes

model.single.pop <- function(effective_population_size_1, number_of_blocks)
{
  lines <- c(
    "//Number of population samples (demes)",
    "1",
    "//Population effective sizes (number of genes)",
    effective_population_size_1,
    "//Sample sizes",
    "100",
    "//Growth rates\t: negative growth implies population expansion",
    "0",
    "//Number of migration matrices : 0 implies no migration between demes",
    "0",
    "//historical event: time, source, sink, migrants, new size, new growth rate, migr. matrix",
    "0 historical event",
    "//Number of independent loci [chromosome]",
    paste(number_of_blocks,"1",sep=" ")
  )
  
  # Repeating the block 22 times
  block <- c(
    "//Per chromosome: Number of linkage blocks",
    "1",
    "//per Block: data type, num loci, rec. rate and mut rate + optional parameters",
    "DNA 1000000 1.0E-8 1.855284327902964E-7 0.0"
  )
  
  # Append number_of_blocks blocks
  for (i in 1:number_of_blocks) {
    lines <- c(lines, block)
  }
  
  # Write to file
  writeLines(lines, "DemographicModelSplitR.par")
  # Execute
  system2(exe, args = args);
  
  data.t <- read.table(file=paste(folder.fastSimcoal2,"DemographicModelSplitR/DemographicModelSplitR_1_1.gen", sep=""), header = T);
  
  # First four columns are snp info
  # haplotype matrix. Rows are haplotypes, columns are positions
  H <- t(as.matrix(data.t[,5:ncol(data.t)]));
  rownames(H) <- rep("A",100);
  # return a list with the chromosomal positions and the haplotype matrix
  return(list(position = data.t[,1:2],haplotype_matrix = H));
}

ne <-1000
d <- model.single.pop(ne, 5)$haplotype_matrix;

