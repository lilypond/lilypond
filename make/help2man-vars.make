
ifneq ($(strip $(DOCUMENTATION)),no)
HELP2MAN_GROFFS = $(addsuffix .1, $(addprefix $(outdir)/, $(HELP2MAN_EXECS)))
endif
