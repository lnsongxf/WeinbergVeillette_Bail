# Makefile to run R script to replicate WeinbergVeillette_Bail
# First cleans CT data
# Next runs regressions for the paper
# =========================

OUTPUTS = ../output/clean_bail_data.RData \
../output/ols.tex \
../output/iv1.tex \
../output/iv2.tex


all: $(OUTPUTS)

../output/clean_bail_data.RData: clean_CTdata.R ../input/Accused_Pre-Trial_Inmates_in_Correctional_Facilities.csv
	RScript --vanilla $< \
		--input ../input/Accused_Pre-Trial_Inmates_in_Correctional_Facilities.csv \
		--output $@

../output/ols.tex: regress.R ../output/clean_bail_data.RData
	RScript --vanilla $< \
		--input ../output/clean_bail_data.RData \
		--output $@

../output/iv1.tex ../output/iv2.tex: ../output/iv2.tex
