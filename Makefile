.PHONY: preprocess_wave2 remove_data preprocess_data

preprocess_wave2:
	Rscript R/preprocess_wave2.R

preprocess_data: preprocess_wave2

clean:
	rm -r data
