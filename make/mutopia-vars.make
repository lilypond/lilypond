
# UGH UGH
include $(make-dir)/lilypond-vars.make

LY_FILES = $(wildcard *.ly)
FLY_FILES = $(wildcard *.fly)
SLY_FILES = $(wildcard *.sly)

M4_FILES = $(wildcard *.m4)
LYM4_FILES = $(wildcard *.lym4)
EXTRA_DIST_FILES += $(FLY_FILES) $(SLY_FILES) $(LY_FILES) $(M4_FILES) $(LYM4_FILES)

# WWW.make

ly_examples=$(addprefix $(outdir)/, $(addsuffix .ly.txt, $(examples)))
fly_examples=$(addprefix $(outdir)/, $(addsuffix .fly.txt, $(flexamples)))
sly_examples=$(addprefix $(outdir)/, $(addsuffix .sly.txt, $(slexamples)))

all_examples=$(flexamples) $(slexamples) $(examples)

ps_examples=$(addprefix $(outdir)/, $(addsuffix .ps.gz, $(all_examples)))
gif_examples=$(addprefix $(outdir)/, $(addsuffix .gif, $(all_examples)))
png_examples=$(addprefix $(outdir)/, $(addsuffix .png, $(all_examples)))

html_subdirs=$(addprefix --subdirs ,$(SUBDIRS))


name-stem= $(notdir $(basename $<))

OUT_FILES = $(addprefix $(outdir)/,$(M4_FILES:%.m4=%)) \
 $(addprefix $(outdir)/,$(LYM4_FILES:%.lym4=%.ly))
