

ly_examples=$(addprefix $(outdir)/, $(addsuffix .ly.txt, $(examples)))
ps_examples=$(addprefix $(outdir)/, $(addsuffix .ps.gz, $(examples)))
gif_examples=$(addprefix $(outdir)/, $(addsuffix .gif, $(examples)))


html_subdirs=$(addprefix --subdirs ,$(SUBDIRS))


local-WWW: $(ly_examples) $(ps_examples) $(gif_examples)
	(cd $(outdir); $(PYTHON) ../$(depth)/bin/mutopia-index.py --prefix=../ --suffix=/$(outdir) $(html_subdirs) $(examples))

