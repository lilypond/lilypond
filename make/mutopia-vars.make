
# UGH UGH
include $(make-dir)/lilypond-vars.make

# LY_FILES is defined the LY make template.

SCORE_LY_FILES = $(shell cd $(src-dir) && fgrep -l score *.ly)

ILY_FILES = $(call src-wildcard,*.ily)
M4_FILES = $(call src-wildcard,*.m4)
LYM4_FILES = $(call src-wildcard,*.lym4)
EXTRA_DIST_FILES += $(M4_FILES) $(LYM4_FILES)

ly_examples=$(addprefix $(outdir)/, $(addsuffix .ly, $(examples)))
ly_examples+= $(ILY_FILES:%.ily=$(outdir)/%.ily)
all_examples=$(examples)
ps_examples=$(addprefix $(outdir)/, $(addsuffix .ps.gz, $(all_examples)))
pdf_examples=$(addprefix $(outdir)/, $(addsuffix .pdf, $(all_examples)))
gif_examples=$(addprefix $(outdir)/, $(addsuffix .gif, $(all_examples)))
png_examples=$(addprefix $(outdir)/, $(addsuffix .png, $(all_examples)))

html_subdirs=$(addprefix --subdirs ,$(SUBDIRS))


name-stem= $(notdir $(basename $<))

OUT_FILES = $(addprefix $(outdir)/,$(M4_FILES:%.m4=%)) \
 $(addprefix $(outdir)/,$(LYM4_FILES:%.lym4=%.ly))

score_ps = $(addprefix $(outdir)/, $(addsuffix .ps.gz, $($SCORE_LY_FILES)))
