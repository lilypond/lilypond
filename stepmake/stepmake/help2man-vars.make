
HELP2MAN_GROFFS = $(addsuffix .1, $(addprefix $(outdir)/, $(HELP2MAN_EXECS)))

# triggers recompilations.  Ugh.
OUT_DIST_FILES +=  # $(wildcard $(outdir)/*.1) 
