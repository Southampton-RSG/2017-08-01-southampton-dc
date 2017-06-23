
all: pages code-handout.R

skeleton-%.R: %.Rmd _site.yml
	Rscript -e "knitr::purl('$<', output='$@', documentation=0L)"

%.html: %.Rmd _site.yml
	Rscript -e "rmarkdown::render_site(input='$<')"

%.html: %.md
	Rscript -e "rmarkdown::render_site(input='$<')"

code-handout.R: skeleton-00-before-we-start.R skeleton-01-intro-to-R.R skeleton-02-starting-with-data.R skeleton-03-dplyr.R skeleton-04-visualization-ggplot2.R
	for f in $^; do cat $$f; echo "\n"; done > $@
	make clean-skeleton

pages: 01_R_and_Rstudio.html 02_intro_to_R.html 03_import_and_inspect.html 04_transforming_data.html 05_visualising_data.html 06_export_and_communication.html 07_ecology_data_overview.html index.html LICENSE.html CITATION.html
	make clean-md

clean-skeleton:
	-rm skeleton-*-*.R

clean-md:
	-rm *-*.md

clean-html:
	-rm *-*.html

clean: clean-skeleton clean-html clean-md

clean-data:
	-rm -rf data
