

ly_examples=$(addprefix $(outdir)/, $(addsuffix .ly.txt, $(examples)))
ps_examples=$(addprefix $(outdir)/, $(addsuffix .ps.gz, $(examples)))
gif_examples=$(addprefix $(outdir)/, $(addsuffix .gif, $(examples)))



WWW:  $(ly_examples) $(ps_examples) $(gif_examples)
	(cd $(outdir); $(PYTHON) ../$(depth)/bin/mutopia-index.py $(examples))

