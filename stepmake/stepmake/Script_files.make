

PERL_SCRIPTS_IN := $(wildcard *.pl)
PERL_SCRIPTS = $(addprefix $(outdir)/, $(PERL_SCRIPTS_IN:.pl=))
SH_SCRIPTS_IN := $(wildcard *.sh)
SH_SCRIPTS = $(addprefix $(outdir)/, $(SH_SCRIPTS_IN:.sh=))
PYTHON_SCRIPTS_IN := $(wildcard *.py)
PYTHON_SCRIPTS = $(addprefix $(outdir)/, $(PYTHON_SCRIPTS_IN:.py=))
ALL_SCRIPTS_IN = $(SH_SCRIPTS_IN) $(PERL_SCRIPTS_IN) $(PYTHON_SCRIPTS_IN)
EXTRA_DIST_FILES += $(ALL_SCRIPTS_IN)
