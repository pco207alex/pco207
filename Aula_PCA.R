library(tidyverse)
#library(doMC)
library(stringr)
library(microbenchmark)
library(readxl)
library(readr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(fun)

# Entrada de dados
if(0){
  
  m0 = read_csv("input.csv")
  
}

if(0){
  
  m.pca = PCA(m0[,1:13], scale.unit = T, ncp = 13, graph = F)
  
}


if(0){
  
  p0 <-fviz_eig(m.pca, addlabels = TRUE, ylim = c(0, 50))
  print(p0)
  
}

if(0){
  
  fviz_eig(m.pca)
  p1 = fviz_pca_var(m.pca, 
                    col.var = "black", 
                    axes = c(1,2),
                    select.var = list(contrib = 6), 
                    title = "1a PCA de var")
  print(p1)
  
  p2 = fviz_pca_ind(m.pca, 
                    col.var = "black", 
                    axes = c(1,2),
                    #select.ind = list(contrib = 6), 
                    title = "1a PCA de ind")
  print(p2)
  
  p3 = fviz_pca_biplot(m.pca,
                       axes = c(1,2),
                       title = "1a PCA biplot")
  print(p3)
  
  
  p4 = fviz_pca_ind(m.pca, 
                    col.ind = factor(m0$C14), 
                    axes = c(1,2),
                    #select.ind = list(contrib = 6), 
                    title = "1a PCA de ind")
  print(p4)
  
  
  p5 = fviz_pca_biplot(m.pca,
                       axes = c(1,2),
                       col.ind = factor(m0$C14), 
                       title = "1a PCA biplot")
  print(p5)
  
}






