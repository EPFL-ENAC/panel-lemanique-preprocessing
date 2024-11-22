.PHONY: preprocess_wave2 remove_data preprocess_data

preprocess_wave2:
	Rscript R/preprocess_wave2.R
	
preprocess_wave1:
	Rscript R/preprocess_wave1.R

preprocess_data: preprocess_wave2 preprocess_wave1

clean:
	rm -r data
