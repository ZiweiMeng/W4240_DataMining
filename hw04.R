###########################
#contains all the functions
###########################
#############################
# < Ziwei Meng >
# STAT W4240 
# Homework 04
# < Wednesday November 11 >
#
# The following code analyzes the federalist papers
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
#setwd("~/Documents/Rworkplace/W4240")
#clear workspace
#rm(list=ls())

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.
# Use the package installer and be sure to install all dependencies
#library(tm)

#################
# Problem 1a
#################

##########################################
# This code uses tm to preprocess the papers into a format useful for NB
preprocess.directory = function(dirname){
  
  # the directory must have all the relevant text files
  ds = DirSource(dirname)
  # Corpus will make a tm document corpus from this directory
  fp = Corpus( ds )
  # inspect to verify
  # inspect(fp[1])
  # another useful command
  # identical(fp[[1]], fp[["Federalist01.txt"]])
  # now let us iterate through and clean this up using tm functionality
  
  # make all words lower case
  fp = tm_map( fp, content_transformer(tolower))
  # remove all punctuation    
  fp = tm_map( fp, removePunctuation)
  # remove stopwords like the, a, and so on.	
  fp = tm_map( fp, removeWords, stopwords("english"))
  # remove stems like suffixes
  fp = tm_map( fp, stemDocument)
  # remove extra whitespace
  fp = tm_map( fp, stripWhitespace)	
  
  # now write the corpus out to the files for our future use.
  # MAKE SURE THE _CLEAN DIRECTORY EXISTS
  writeCorpus( fp , sprintf('%s_clean',dirname) )
}
#######################