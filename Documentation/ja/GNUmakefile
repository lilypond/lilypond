ISOLANG = ja
depth = ../..
STEPMAKE_TEMPLATES = texinfo
LOCALSTEPMAKE_TEMPLATES = lilypond ly doc-i18n-root

#
# By default, Japanese PDF documents are not built.
# It requires some fonts and packages.
# Unfortunately, most developers do not have them.
#
# For building Japanese PDF documents the following is required.
#
#   XeTeX 0.9998+ (TeX Live 2012+)
#     The modern TeX engine used for LilyPond's PDF documents.
#     It might be contained in texlive-xetex package.
#
#   IPAex fonts
#     Japanese fonts.
#     It might be contained in texlive-lang-japanese package.
#
#   zhspacing
#     Spacing for mixed CJK-English documents in XeTeX.
#     It might be contained in texlive-lang-chinese package.
#     (This package is for Chinese, but can also be used for Japanese.)
#
# If you want to build Japanese PDF documents,
# please install these packages and comment out `NO_PDF_FILES = 1'
# before `make doc'.
# Note: Even if Japanese PDF documents have been built,
# links from HTML documents to Japanese PDF will not be created.
#
NO_PDF_FILES = 1

include $(depth)/make/stepmake.make
