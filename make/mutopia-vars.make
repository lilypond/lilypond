
# UGH UGH
include $(make-dir)/lilypond-vars.make

LY_FILES = $(wildcard *.ly)
SCORE_LY_FILES = $(shell fgrep -l score *.ly)


M4_FILES = $(wildcard *.m4)
LYM4_FILES = $(wildcard *.lym4)
EXTRA_DIST_FILES += $(LY_FILES) $(M4_FILES) $(LYM4_FILES)

ly_examples=$(addprefix $(outdir)/, $(addsuffix .ly.txt, $(examples)))

ps_examples=$(addprefix $(outdir)/, $(addsuffix .ps.gz, $(examples)))
pdf_examples=$(addprefix $(outdir)/, $(addsuffix .pdf, $(examples)))
dvi_examples=$(addprefix $(outdir)/, $(addsuffix .dvi, $(examples)))
gif_examples=$(addprefix $(outdir)/, $(addsuffix .gif, $(examples)))
png_examples=$(addprefix $(outdir)/, $(addsuffix .png, $(examples)))

html_subdirs=$(addprefix --subdirs ,$(SUBDIRS))


name-stem= $(notdir $(basename $<))

OUT_FILES = $(addprefix $(outdir)/,$(M4_FILES:%.m4=%)) \
 $(addprefix $(outdir)/,$(LYM4_FILES:%.lym4=%.ly))

score_ps = $(addprefix $(outdir)/, $(addsuffix .ps.gz, $($SCORE_LY_FILES)))
