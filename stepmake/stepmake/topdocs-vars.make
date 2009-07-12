TO_TOP_FILES=$(addprefix $(outdir)/, $(addsuffix .txt, $(README_TOP_FILES)))

DOCUMENTATION_INCLUDES += -I $(top-src-dir)/Documentation/user
