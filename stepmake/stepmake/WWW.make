# WWW.make

ly_examples=$(addprefix $(outdir)/, $(addsuffix .ly.txt, $(examples)))
fly_examples=$(addprefix $(outdir)/, $(addsuffix .fly.txt, $(flexamples)))

all_examples=$(flexamples) $(examples)

ps_examples=$(addprefix $(outdir)/, $(addsuffix .ps.gz, $(all_examples)))
gif_examples=$(addprefix $(outdir)/, $(addsuffix .gif, $(all_examples)))

html_subdirs=$(addprefix --subdirs ,$(SUBDIRS))

## UGH!  lily specific
local-WWW: $(ly_examples) $(fly_examples) $(ps_examples) $(gif_examples)
	(cd $(outdir); $(PYTHON) ../$(buildscripts)/mutopia-index.py --package=$(topdir) --prefix=../ --suffix=/$(outdir) $(html_subdirs) $(all_examples))
	$(PYTHON) $(step-bindir)/add-html-footer.py --package=$(topdir) --index=$(depth)/$(outdir)/index.html $(outdir)/index.html

