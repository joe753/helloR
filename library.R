library(stringi)
library('ggplot2')
library('dplyr')
library('ggiraphExtra')
library(gridExtra)
library(devtools)
library(ggiraph)
library(kormaps2014)
library(plotly)
library(dygraphs)
library(xts)  
library(sqldf)


options(encoding="UTF-8")
data = read.csv('data/성적.csv')
data = dplyr::rename(data, stuno=학번, cls=반, gen=성별, math=수학, kor=국어, eng=영어, sci=과학, art=예체)


  kdata = load('data/kdata.rda')


