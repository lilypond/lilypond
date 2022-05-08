
EXPECT_SCRIPTS_IN := $(call src-wildcard,*.expect)
PERL_SCRIPTS_IN := $(call src-wildcard,*.pl)
PYTHON_SCRIPTS_IN := $(call src-wildcard,*.py)
SCM_SCRIPTS_IN := $(call src-wildcard,*.scm)
SH_SCRIPTS_IN := $(call src-wildcard,*.sh)

EXPECT_SCRIPTS = $(addprefix $(outdir)/, $(EXPECT_SCRIPTS_IN:.expect=))
PERL_SCRIPTS = $(addprefix $(outdir)/, $(PERL_SCRIPTS_IN:.pl=))
PYTHON_SCRIPTS = $(addprefix $(outdir)/, $(PYTHON_SCRIPTS_IN:.py=))
SCM_SCRIPTS = $(addprefix $(outdir)/, $(SCM_SCRIPTS_IN:.scm=))
SH_SCRIPTS = $(addprefix $(outdir)/, $(SH_SCRIPTS_IN:.sh=))
