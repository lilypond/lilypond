/* Copyright (c) 1997-1998 by Juliusz Chroboczek */

#define NULL ((void *)0)

/* char *macEncoding[]={ */
/*   ".notdef", NULL, NULL, NULL, NULL, NULL, */
/*   NULL, NULL, NULL, NULL, NULL, NULL, */
/*   NULL, NULL, NULL, NULL, NULL, NULL, */
/*   NULL, NULL, NULL, NULL, NULL, NULL, */
/*   NULL, NULL, NULL, NULL, NULL, NULL, */
/*   NULL, NULL, "space", "exclam", "quotedbl", "numbersign", */
/*   "dollar", "percent", "ampersand", "quoteright", "parenleft", */
/*   "parenright", "asterisk", "plus", "comma", "hyphen", "period", */
/*   "slash", "zero", "one", "two", "three", "four", "five", "six", */
/*   "seven", "eight", "nine", "colon", "semicolon", "less", "equal", */
/*   "greater", "question", "at", "A", "B", "C", "D", "E", "F", "G", "H", */
/*   "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", */
/*   "W", "X", "Y", "Z", "bracketleft", "backslash", "bracketright", */
/*   "asciicircum", "underscore", "quoteleft", "a", "b", "c", "d", "e", */
/*   "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", */
/*   "t", "u", "v", "w", "x", "y", "z", "braceleft", "bar", "braceright", */
/*   "tilde", NULL, "Adieresis", "Aring", "Ccedilla", "Eacute", */
/*   "Ntilde", "Odieresis", "Udieresis", "aacute", "agrave", "acircumflex", */
/*   "adieresis", "atilde", "aring", "ccedilla", "eacute", "egrave", */
/*   "ecircumflex", "edieresis", "iacute", "igrave", "icircumflex", */
/*   "idieresis", "ntilde", "oacute", "ograve", "ocircumflex", "odieresis", */
/*   "otilde", "uacute", "ugrave", "ucircumflex", "udieresis", "dagger", */
/*   "degree", "cent", "sterling", "section", "bullet", "paragraph", */
/*   "germandbls", "registered", "copyright", "trademark", "acute", */
/*   "dieresis", "notequal", "AE", "Oslash", "infinity", "plusminus", */
/*   "lessequal", "greaterequal", "yen", "mu", "partialdiff", "Sigma", */
/*   "product", "pi", "integral", "ordfeminine", "ordmasculine", "Omega", */
/*   "ae", "oslash", "questiondown", "exclamdown", "logicalnot", "radical", */
/*   "florin", "approxequal", "Delta", "guillemotleft", "guillemotright", */
/*   "ellipsis", "space", "Agrave", "Atilde", "Otilde", "OE", "oe", */
/*   "endash", "emdash", "quotedblleft", "quotedblright", "quoteleft", */
/*   "quoteright", "divide", "lozenge", "ydieresis", "Ydieresis", */
/*   "fraction", "currency", "guilsinglleft", "guilsinglright", "fi", "fl", */
/*   "daggerdbl", "periodcentered", "quotesinglbase", "quotedblbase", */
/*   "perthousand", "Acircumflex", "Ecircumflex", "Aacute", "Edieresis", */
/*   "Egrave", "Iacute", "Icircumflex", "Idieresis", "Igrave", "Oacute", */
/*   "Ocircumflex", NULL, "Ograve", "Uacute", "Ucircumflex", "Ugrave", */
/*   "dotlessi", "circumflex", "tilde", "macron", "breve", "dotaccent", */
/*   "ring", "cedilla", "hungarumlaut", "ogonek", "caron"}; */

char *macGlyphEncoding[] = {
  ".notdef", ".null", "CR", "space", "exclam", "quotedbl", "numbersign",
  "dollar", "percent", "ampersand", "quotesingle", "parenleft",
  "parenright", "asterisk", "plus", "comma", "hyphen", "period",
  "slash", "zero", "one", "two", "three", "four", "five", "six",
  "seven", "eight", "nine", "colon", "semicolon", "less", "equal",
  "greater", "question", "at", "A", "B", "C", "D", "E", "F", "G", "H",
  "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
  "W", "X", "Y", "Z", "bracketleft", "backslash", "bracketright",
  "asciicircum", "underscore", "grave", "a", "b", "c", "d", "e", "f",
  "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
  "u", "v", "w", "x", "y", "z", "braceleft", "bar", "braceright",
  "asciitilde", "Adieresis", "Aring", "Ccedilla", "Eacute", "Ntilde",
  "Odieresis", "Udieresis", "aacute", "agrave", "acircumflex",
  "adieresis", "atilde", "aring", "ccedilla", "eacute", "egrave",
  "ecircumflex", "edieresis", "iacute", "igrave", "icircumflex",
  "idieresis", "ntilde", "oacute", "ograve", "ocircumflex", "odieresis",
  "otilde", "uacute", "ugrave", "ucircumflex", "udieresis", "dagger",
  "degree", "cent", "sterling", "section", "bullet", "paragraph",
  "germandbls", "registered", "copyright", "trademark", "acute",
  "dieresis", "notequal", "AE", "Oslash", "infinity", "plusinus",
  "lessequal", "greaterequal", "yen", "mu1", "partialdiff", "summation",
  "product", "pi", "integral", "ordfeminine", "ordmasculine", "Ohm",
  "ae", "oslash", "questiondown", "exclamdown", "logicalnot", "radical",
  "florin", "approxequal", "increment", "guillemotleft",
  "guillemotright", "ellipsis", "nbspace", "Agrave", "Atilde", "Otilde",
  "OE", "oe", "endash", "emdash", "quotedblleft", "quotedblright",
  "quoteleft", "quoteright", "divide", "lozenge", "ydieresis",
  "Ydieresis", "fraction", "currency", "guilsingleft", "guilsingright",
  "fi", "fl", "daggerdbl", "periodcentered", "quotesinglbase",
  "quotedblbase", "perthousand", "Acircumflex", "Ecircumflex", "Aacute",
  "Edieresis", "Egrave", "Iacute", "Icircumflex", "Idieresis", "Igrave",
  "Oacute", "Ocircumflex", "applelogo", "Ograve", "Uacute",
  "Ucircumflex", "Ugrave", "dotlessi", "circumflex", "tilde",
  "overscore", "breve", "dotaccent", "ring", "cedilla", "hungarumlaut",
  "ogonek", "caron", "Lslash", "lslash", "Scaron", "scaron", "Zcaron",
  "zcaron", "brokenbar", "Eth", "eth", "Yacute", "yacute", "Thorn",
  "thorn", "minus", "multiply", "onesuperior", "twosuperior",
  "threesuperior", "onehalf", "onequarter", "threequarters", "franc",
  "Gbreve", "gbreve", "Idot", "Scedilla", "scedilla", "Cacute",
  "cacute", "Ccaron", "ccaron", "dmacron"
};

char *adobeStandardEncoding[] = {
  ".notdef", NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  "space", "exclam", "quotedbl", "numbersign", "dollar", "percent",
    "ampersand", "quoteright",
  "parenleft", "parenright", "asterisk", "plus", "comma", "hyphen", "period",
    "slash",
  "zero", "one", "two", "three", "four", "five", "six", "seven",
  "eight", "nine", "colon", "semicolon", "less", "equal", "greater",
    "question",
  "at", "A", "B", "C", "D", "E", "F", "G",
  "H", "I", "J", "K", "L", "M", "N", "O",
  "P", "Q", "R", "S", "T", "U", "V", "W",
  "X", "Y", "Z", "bracketleft", "backslash", "bracketright", "asciicircum",
    "underscore",
  "quoteleft", "a", "b", "c", "d", "e", "f", "g",
  "h", "i", "j", "k", "l", "m", "n", "o",
  "p", "q", "r", "s", "t", "u", "v", "w",
  "x", "y", "z", "braceleft", "bar", "braceright", "asciitilde", NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, "exclamdown", "cent", "sterling", "fraction", "yen", "florin",
    "section",
  "currency", "quotesingle", "quotedblleft", "guillemotleft", "guilsinglleft",
    "guilsinglright", "fi", "fl",
  NULL, "endash", "dagger", "daggerdbl", "periodcentered", NULL, "paragraph",
    "bullet",
  "quotesinglbase", "quotedblbase", "quotedblright", "guillemotright",
    "ellipsis", "perthousand", NULL, "questiondown",
  NULL, "grave", "acute", "circumflex", "tilde", "macron", "breve",
    "dotaccent",
  "dieresis", NULL, "ring", "cedilla", NULL, "hungarumlaut", "ogonek",
    "caron",
  "emdash", NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, "AE", NULL, "ordfeminine", NULL, NULL, NULL, NULL,
  "Lslash", "Oslash", "OE", "ordmasculine", NULL, NULL, NULL, NULL,
  NULL, "ae", NULL, NULL, NULL, "dotlessi", NULL, NULL,
  "lslash", "oslash", "oe", "germandbls", NULL, NULL, NULL, NULL
};
