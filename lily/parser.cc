
/*  A Bison parser, made from parser.yy
 by  GNU Bison version 1.25
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	ALIAS	258
#define	BAR	259
#define	BEAMPLET	260
#define	MAEBTELP	261
#define	BREAK	262
#define	CADENZA	263
#define	CLEAR	264
#define	CLEF	265
#define	CONTAINS	266
#define	CONSISTS	267
#define	ACCEPTS	268
#define	CM_T	269
#define	DURATION	270
#define	ABSDYNAMIC	271
#define	END	272
#define	GROUPING	273
#define	TRANSLATOR	274
#define	HEADER	275
#define	IN_T	276
#define	LYRIC	277
#define	KEY	278
#define	MELODIC	279
#define	MIDI	280
#define	MELODIC_REQUEST	281
#define	METER	282
#define	MM_T	283
#define	MULTI	284
#define	NOTENAMES	285
#define	OCTAVE	286
#define	OUTPUT	287
#define	PAPER	288
#define	PARTIAL	289
#define	PLET	290
#define	TELP	291
#define	PT_T	292
#define	SCORE	293
#define	SCRIPT	294
#define	SHAPE	295
#define	SKIP	296
#define	SPANDYNAMIC	297
#define	STAFF	298
#define	START_T	299
#define	SYMBOLTABLES	300
#define	TABLE	301
#define	TRANSPOSE	302
#define	TEMPO	303
#define	TYPE	304
#define	TEXID	305
#define	TEXTSTYLE	306
#define	TITLE	307
#define	PROPERTY	308
#define	VERSION	309
#define	E_EXCLAMATION	310
#define	E_SMALLER	311
#define	E_BIGGER	312
#define	E_CHAR	313
#define	DIGIT	314
#define	NOTENAME_ID	315
#define	DURATION_IDENTIFIER	316
#define	IDENTIFIER	317
#define	MELODIC_REQUEST_IDENTIFIER	318
#define	MUSIC_IDENTIFIER	319
#define	VOICE_IDENTIFIER	320
#define	POST_REQUEST_IDENTIFIER	321
#define	SCRIPT_IDENTIFIER	322
#define	COMMAND_IDENTIFIER	323
#define	REAL_IDENTIFIER	324
#define	TRANS_IDENTIFIER	325
#define	INT_IDENTIFIER	326
#define	SCORE_IDENTIFIER	327
#define	MIDI_IDENTIFIER	328
#define	PAPER_IDENTIFIER	329
#define	REQUEST_IDENTIFIER	330
#define	REAL	331
#define	RESTNAME	332
#define	STRING	333
#define	UNSIGNED	334
#define	POST_QUOTES	335
#define	PRE_QUOTES	336

#line 1 "parser.yy"
 // -*-Fundamental-*-

/*
  parser.yy -- YACC -> C++ parser for mudela

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
           Jan Nieuwenhuizen <jan@digicash.com>
*/

#include <iostream.h>

// mmm
#define MUDELA_VERSION "0.1.9"

#include "scalar.hh"
#include "translation-property.hh"
#include "script-def.hh"
#include "symtable.hh"
#include "lookup.hh"
#include "misc.hh"
#include "my-lily-lexer.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "main.hh"
#include "keyword.hh"
#include "debug.hh"
#include "parseconstruct.hh"
#include "dimen.hh"
#include "identifier.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "my-lily-parser.hh"
#include "text-def.hh"
#include "translator-group.hh"
#include "score.hh"
#include "music-list.hh"
#include "header.hh"
#include "duration-convert.hh"
#include "change-translator.hh"

int const GUESS_PLET = 5;
int guess_plet_a[GUESS_PLET] =
{ 
  1,
  3,
  2,
  3,
  4
};

#ifndef NDEBUG
#define YYDEBUG 1
#endif

#define YYERROR_VERBOSE 1

#define YYPARSE_PARAM my_lily_parser_l
#define YYLEX_PARAM my_lily_parser_l
#define THIS ((My_lily_parser *) my_lily_parser_l)

#define yyerror THIS->parser_error


#line 68 "parser.yy"
typedef union {
    Array<Interval>* intarr;
    Array<Melodic_req*> *melreqvec;/* should clean up naming */
    Array<String> * strvec;
    Array<int> *intvec;
    Box *box;
    Chord * chord;
    Duration *duration;
    Identifier *id;
    Translator* trans;
    Music *music;
    Music_list *musiclist;
    Score *score;
    Header *header;
    Interval *interval;
    Lookup*lookup;
    Melodic_req * melreq;
    Musical_req* musreq;
    Music_output_def * outputdef;
    Midi_def* midi;
    Moment *moment;
    Note_req *notereq;
    Paper_def *paper;
    Real real;
    Request * request;
    General_script_def * script;
    Scalar *scalar;
    String *string;
    Atom * symbol;
    Symtable * symtable;
    Symtables * symtables;
    Text_def * textdef;
    Tempo_req *tempo;
    char c;
    const char *consstr;
    int i;
    int pair[2];
    int ii[10];
} YYSTYPE;
#line 107 "parser.yy"


int
yylex (YYSTYPE *s,  void * v_l)
{
	My_lily_parser	 *pars_l = (My_lily_parser*) v_l;
	My_lily_lexer * lex_l = pars_l->lexer_p_;

	lex_l->lexval_l = (void*) s;
	return lex_l->yylex ();
}


#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		353
#define	YYFLAG		-32768
#define	YYNTBASE	104

#define YYTRANSLATE(x) ((unsigned)(x) <= 336 ? yytranslate[x] : 191)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    92,     2,     2,     2,     2,     2,     2,    95,
    98,    86,   100,     2,   101,    87,    91,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    97,    82,    88,
    85,    89,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    96,     2,    93,    99,   103,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
   102,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    83,    90,    84,    94,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
    56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
    66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
    76,    77,    78,    79,    80,    81
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     1,     4,     7,    10,    13,    16,    19,    23,    28,
    29,    32,    37,    38,    44,    49,    50,    53,    54,    59,
    61,    63,    65,    67,    69,    71,    73,    75,    77,    79,
    81,    83,    88,    90,    94,   100,   105,   110,   111,   117,
   118,   120,   123,   126,   129,   132,   134,   136,   137,   142,
   147,   148,   150,   155,   158,   164,   170,   175,   181,   184,
   185,   189,   194,   195,   200,   205,   209,   212,   217,   221,
   222,   225,   227,   231,   237,   239,   241,   243,   245,   248,
   249,   253,   254,   258,   260,   262,   267,   274,   276,   278,
   282,   286,   287,   290,   294,   298,   300,   303,   305,   307,
   309,   311,   314,   316,   318,   321,   323,   328,   331,   333,
   336,   339,   342,   345,   348,   349,   352,   355,   357,   359,
   361,   363,   365,   367,   370,   373,   375,   378,   385,   391,
   396,   402,   406,   409,   412,   414,   417,   419,   421,   423,
   425,   427,   429,   433,   436,   439,   441,   443,   445,   447,
   449,   454,   461,   464,   466,   468,   470,   472,   474,   476,
   478,   480,   482,   484,   486,   488,   490,   492,   494,   496,
   498,   500,   501,   504,   507,   510,   511,   515,   518,   519,
   522,   524,   527,   528,   530,   532,   534,   536,   538,   541,
   545,   549,   551,   554,   557,   560,   563,   564,   567,   569,
   571,   573,   576,   578,   580,   582,   585,   587,   589,   591,
   593,   598,   599,   601,   605,   610,   615,   616,   620,   623,
   625,   628
};

static const short yyrhs[] = {    -1,
   104,   109,     0,   104,   116,     0,   104,   111,     0,   104,
     1,     0,   104,   105,     0,   104,   106,     0,    54,    78,
    82,     0,    30,    83,   107,    84,     0,     0,   107,     9,
     0,   107,    78,    85,   149,     0,     0,   108,    78,    85,
   110,    82,     0,    20,    83,   108,    84,     0,     0,   110,
    78,     0,     0,    78,   112,    85,   113,     0,   116,     0,
   121,     0,   124,     0,   158,     0,   129,     0,   184,     0,
   181,     0,   180,     0,   146,     0,   149,     0,   114,     0,
   150,     0,    19,    83,   115,    84,     0,    70,     0,    49,
    78,    82,     0,   115,    78,    85,   134,    82,     0,   115,
    12,    78,    82,     0,   115,    13,    78,    82,     0,     0,
    38,   117,    83,   118,    84,     0,     0,    72,     0,   118,
   109,     0,   118,   129,     0,   118,   119,     0,   118,     1,
     0,   121,     0,   124,     0,     0,   120,   180,    86,   180,
     0,    33,    83,   122,    84,     0,     0,    74,     0,   122,
    32,    78,    82,     0,   122,   184,     0,   122,    78,    85,
   182,    82,     0,   122,    78,    85,   181,    82,     0,   122,
    78,    85,   114,     0,   122,    40,    85,   123,    82,     0,
   122,     1,     0,     0,   123,   182,   182,     0,    25,    83,
   125,    84,     0,     0,   125,    78,    85,   114,     0,   125,
    32,    78,    82,     0,   125,   126,    82,     0,   125,     1,
     0,    48,   172,    85,   179,     0,    83,   128,    84,     0,
     0,   128,   129,     0,   138,     0,    49,    78,   129,     0,
    49,    78,    85,    78,   129,     0,   127,     0,   135,     0,
   137,     0,    64,     0,    64,    82,     0,     0,    24,   130,
   129,     0,     0,    22,   131,   129,     0,   133,     0,   132,
     0,    19,    78,    85,    78,     0,    53,    78,    87,    78,
    85,   134,     0,    78,     0,   180,     0,    88,   136,    89,
     0,    29,   179,   135,     0,     0,   136,   129,     0,    47,
   147,   129,     0,   167,   139,   144,     0,   140,     0,   168,
    82,     0,   176,     0,   177,     0,   141,     0,   142,     0,
   143,    82,     0,    90,     0,    68,     0,     4,    78,     0,
     7,     0,    27,   179,    91,   179,     0,    41,   170,     0,
   126,     0,     8,   179,     0,    34,   170,     0,    10,    78,
     0,    23,   178,     0,    18,   120,     0,     0,   144,   145,
     0,   144,   154,     0,   160,     0,   146,     0,    66,     0,
   151,     0,   175,     0,    60,     0,   147,    80,     0,    81,
   147,     0,   147,     0,   148,    92,     0,    26,    83,   180,
   180,   180,    84,     0,    15,    83,   180,   179,    84,     0,
    16,    83,   179,    84,     0,    42,    83,   180,   180,    84,
     0,   179,    91,   179,     0,    91,   179,     0,    93,   152,
     0,    36,     0,    36,   152,     0,    94,     0,    95,     0,
    93,     0,    56,     0,    57,     0,   153,     0,    96,    97,
   179,     0,    96,   152,     0,    35,   152,     0,    55,     0,
    98,     0,    96,     0,   155,     0,   156,     0,    39,    83,
   159,    84,     0,    78,   180,   180,   180,   180,   180,     0,
   166,   161,     0,   162,     0,   165,     0,   163,     0,    78,
     0,    59,     0,    99,     0,   100,     0,   101,     0,    90,
     0,   102,     0,    89,     0,    87,     0,    67,     0,   158,
     0,   164,     0,   103,     0,    99,     0,   101,     0,     0,
   167,   157,     0,    15,    78,     0,    15,   172,     0,     0,
    31,   169,   147,     0,    51,    78,     0,     0,   170,   174,
     0,    87,     0,   171,    87,     0,     0,   171,     0,   174,
     0,   172,     0,   179,     0,    61,     0,   174,    87,     0,
   174,    86,   179,     0,   174,    91,   179,     0,    97,     0,
    97,   179,     0,   148,   173,     0,    77,   173,     0,   162,
   173,     0,     0,   178,    60,     0,    79,     0,    59,     0,
   179,     0,   101,   179,     0,    71,     0,    76,     0,    69,
     0,   181,   183,     0,    14,     0,    21,     0,    28,     0,
    37,     0,    45,    83,   185,    84,     0,     0,    62,     0,
   185,    50,    78,     0,   185,    78,    85,   186,     0,    46,
    83,   187,    84,     0,     0,   187,    78,   188,     0,    78,
   189,     0,    78,     0,   190,   190,     0,   182,   182,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   258,   259,   263,   266,   267,   268,   269,   272,   285,   288,
   291,   294,   300,   304,   311,   318,   322,   331,   335,   342,
   347,   351,   355,   359,   363,   367,   371,   374,   377,   381,
   384,   391,   396,   401,   406,   411,   415,   424,   428,   442,
   445,   448,   451,   456,   459,   464,   468,   473,   475,   484,
   489,   493,   496,   500,   501,   504,   507,   511,   515,   520,
   524,   531,   537,   540,   544,   548,   552,   557,   570,   576,
   581,   586,   588,   593,   600,   601,   602,   603,   604,   605,
   608,   610,   613,   614,   615,   618,   631,   645,   647,   651,
   653,   659,   665,   670,   683,   688,   689,   692,   694,   697,
   707,   709,   712,   716,   721,   726,   731,   736,   744,   747,
   750,   754,   758,   764,   769,   778,   782,   791,   793,   796,
   800,   803,   815,   820,   823,   829,   835,   841,   850,   858,
   864,   872,   877,   885,   893,   900,   909,   913,   916,   919,
   922,   925,   929,   941,   949,   958,   962,   965,   968,   970,
   976,   980,   989,  1000,  1005,  1009,  1015,  1026,  1036,  1038,
  1039,  1040,  1041,  1042,  1043,  1048,  1050,  1051,  1057,  1059,
  1060,  1063,  1066,  1075,  1080,  1084,  1094,  1098,  1104,  1108,
  1113,  1115,  1118,  1122,  1126,  1132,  1138,  1148,  1151,  1154,
  1157,  1163,  1167,  1177,  1185,  1191,  1203,  1206,  1211,  1215,
  1219,  1223,  1226,  1233,  1237,  1246,  1251,  1252,  1253,  1254,
  1260,  1264,  1268,  1271,  1275,  1281,  1285,  1287,  1294,  1300,
  1307,  1315
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","ALIAS",
"BAR","BEAMPLET","MAEBTELP","BREAK","CADENZA","CLEAR","CLEF","CONTAINS","CONSISTS",
"ACCEPTS","CM_T","DURATION","ABSDYNAMIC","END","GROUPING","TRANSLATOR","HEADER",
"IN_T","LYRIC","KEY","MELODIC","MIDI","MELODIC_REQUEST","METER","MM_T","MULTI",
"NOTENAMES","OCTAVE","OUTPUT","PAPER","PARTIAL","PLET","TELP","PT_T","SCORE",
"SCRIPT","SHAPE","SKIP","SPANDYNAMIC","STAFF","START_T","SYMBOLTABLES","TABLE",
"TRANSPOSE","TEMPO","TYPE","TEXID","TEXTSTYLE","TITLE","PROPERTY","VERSION",
"E_EXCLAMATION","E_SMALLER","E_BIGGER","E_CHAR","DIGIT","NOTENAME_ID","DURATION_IDENTIFIER",
"IDENTIFIER","MELODIC_REQUEST_IDENTIFIER","MUSIC_IDENTIFIER","VOICE_IDENTIFIER",
"POST_REQUEST_IDENTIFIER","SCRIPT_IDENTIFIER","COMMAND_IDENTIFIER","REAL_IDENTIFIER",
"TRANS_IDENTIFIER","INT_IDENTIFIER","SCORE_IDENTIFIER","MIDI_IDENTIFIER","PAPER_IDENTIFIER",
"REQUEST_IDENTIFIER","REAL","RESTNAME","STRING","UNSIGNED","POST_QUOTES","PRE_QUOTES",
"';'","'{'","'}'","'='","'*'","'.'","'<'","'>'","'|'","'/'","'!'","']'","'~'",
"'('","'['","':'","')'","'^'","'+'","'-'","'o'","'_'","mudela","check_version",
"add_notenames","notenames_body","mudela_header_body","mudela_header","concat_strings",
"add_declaration","@1","identifier_init","translator_spec","translator_spec_body",
"score_block","@2","score_body","output_def","intastint_list","paper_block",
"paper_body","shape_array","midi_block","midi_body","tempo_request","Voice",
"Voice_body","Music","@3","@4","translator_change","property_def","scalar","Chord",
"Chord_body","transposed_music","full_element","simple_element","command_elt",
"command_req","abbrev_command_req","verbose_command_req","post_requests","structured_post_request",
"post_request","steno_melodic_req","steno_note_req","melodic_request","explicit_duration",
"dynamic_req","plet_fraction","close_plet_parens","close_request_parens","open_abbrev_parens",
"open_plet_parens","open_request_parens","script_definition","script_body","script_req",
"gen_script_def","text_def","finger","script_abbreviation","mudela_script","script_dir",
"pre_requests","voice_command","@5","duration_length","dots","entered_notemode_duration",
"notemode_duration","explicit_steno_duration","abbrev_type","music_elt","lyrics_elt",
"pitch_list","unsigned","int","real","dim","unit","symtables","symtables_body",
"symtable","symtable_body","symboldef","box","dinterval", NULL
};
#endif

static const short yyr1[] = {     0,
   104,   104,   104,   104,   104,   104,   104,   105,   106,   107,
   107,   107,   108,   108,   109,   110,   110,   112,   111,   113,
   113,   113,   113,   113,   113,   113,   113,   113,   113,   113,
   113,   114,   115,   115,   115,   115,   115,   117,   116,   118,
   118,   118,   118,   118,   118,   119,   119,   120,   120,   121,
   122,   122,   122,   122,   122,   122,   122,   122,   122,   123,
   123,   124,   125,   125,   125,   125,   125,   126,   127,   128,
   128,   129,   129,   129,   129,   129,   129,   129,   129,   130,
   129,   131,   129,   129,   129,   132,   133,   134,   134,   135,
   135,   136,   136,   137,   138,   138,   138,   139,   139,   140,
   141,   141,   142,   142,   143,   143,   143,   143,   143,   143,
   143,   143,   143,   143,   144,   144,   144,   145,   145,   146,
   146,   146,   147,   147,   147,   148,   148,   149,   150,   151,
   151,   152,   152,   153,   153,   153,   154,   154,   154,   154,
   154,   154,   155,   156,   156,   157,   157,   157,   157,   157,
   158,   159,   160,   161,   161,   161,   162,   163,   164,   164,
   164,   164,   164,   164,   164,   165,   165,   165,   166,   166,
   166,   167,   167,   168,   168,   169,   168,   168,   170,   170,
   171,   171,   172,   172,   172,   173,   174,   174,   174,   174,
   174,   175,   175,   176,   176,   177,   178,   178,   179,   179,
   180,   180,   180,   181,   181,   182,   183,   183,   183,   183,
   184,   185,   185,   185,   185,   186,   187,   187,   188,   188,
   189,   190
};

static const short yyr2[] = {     0,
     0,     2,     2,     2,     2,     2,     2,     3,     4,     0,
     2,     4,     0,     5,     4,     0,     2,     0,     4,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     4,     1,     3,     5,     4,     4,     0,     5,     0,
     1,     2,     2,     2,     2,     1,     1,     0,     4,     4,
     0,     1,     4,     2,     5,     5,     4,     5,     2,     0,
     3,     4,     0,     4,     4,     3,     2,     4,     3,     0,
     2,     1,     3,     5,     1,     1,     1,     1,     2,     0,
     3,     0,     3,     1,     1,     4,     6,     1,     1,     3,
     3,     0,     2,     3,     3,     1,     2,     1,     1,     1,
     1,     2,     1,     1,     2,     1,     4,     2,     1,     2,
     2,     2,     2,     2,     0,     2,     2,     1,     1,     1,
     1,     1,     1,     2,     2,     1,     2,     6,     5,     4,
     5,     3,     2,     2,     1,     2,     1,     1,     1,     1,
     1,     1,     3,     2,     2,     1,     1,     1,     1,     1,
     4,     6,     2,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     0,     2,     2,     2,     0,     3,     2,     0,     2,
     1,     2,     0,     1,     1,     1,     1,     1,     2,     3,
     3,     1,     2,     2,     2,     2,     0,     2,     1,     1,
     1,     2,     1,     1,     1,     2,     1,     1,     1,     1,
     4,     0,     1,     3,     4,     4,     0,     3,     2,     1,
     2,     2
};

static const short yydefact[] = {     1,
     0,     5,     0,     0,    38,     0,    18,     6,     7,     2,
     4,     3,    13,    10,     0,     0,     0,     0,     0,    40,
     8,   172,     0,    15,    11,     0,     9,    41,     0,     0,
   106,     0,     0,   183,     0,    48,     0,    82,   197,    80,
     0,     0,     0,     0,   176,     0,   179,     0,   179,     0,
     0,     0,   183,     0,     0,     0,   200,    78,   120,   104,
   205,   203,   204,   199,    70,    92,   103,   192,     0,    19,
    30,    20,    21,    22,   109,    75,    24,    85,    84,    76,
    77,    72,    96,   100,   101,     0,    28,    29,    31,   121,
    23,     0,     0,   122,   201,    27,    26,    25,    16,     0,
    45,   183,     0,    39,    42,    44,    46,    47,    43,   105,
   110,   112,   188,   174,     0,   181,   184,   175,   185,   187,
     0,   114,     0,     0,   172,   113,   172,    63,     0,     0,
     0,     0,    51,   111,     0,   108,     0,   212,   123,     0,
   172,     0,   172,   178,     0,    79,   172,   172,   193,   202,
   102,     0,   146,   183,   157,   148,   147,   115,   126,   183,
   149,   150,   173,   183,    98,    99,    97,     0,    12,     0,
   182,     0,   189,     0,     0,     0,     0,     0,    33,     0,
    83,   198,    81,     0,     0,     0,    91,   177,    52,     0,
   180,     0,     0,     0,   213,     0,   125,   124,    94,     0,
     0,    73,     0,    69,    71,    90,    93,     0,   145,     0,
   186,   195,     0,   144,    95,   127,   194,   196,    17,    14,
     0,   190,   191,   130,     0,    86,     0,     0,     0,     0,
    32,    67,     0,     0,    62,     0,     0,   107,    59,     0,
     0,     0,    50,    54,     0,   151,     0,     0,     0,   211,
    68,   172,     0,   133,     0,   143,   135,   140,   141,   139,
   137,   138,   170,   171,   169,   116,   119,   142,   117,   118,
     0,   129,    49,    34,     0,     0,     0,     0,     0,    66,
     0,     0,    60,     0,     0,   131,   214,     0,    74,     0,
   132,   136,   134,   158,   166,   165,   164,   162,   159,   160,
   161,   163,   167,   153,   154,   156,   168,   155,    36,    37,
    88,     0,    89,    65,     0,    64,   128,    53,     0,    57,
     0,     0,     0,     0,   215,    87,    35,    58,     0,     0,
   207,   208,   209,   210,    56,   206,    55,     0,   217,    61,
   152,     0,     0,   216,   220,   218,     0,   219,     0,   222,
   221,     0,     0
};

static const short yydefgoto[] = {     1,
     8,     9,    19,    18,    10,   168,    11,    17,    70,    71,
   180,    12,    15,    29,   106,   122,    73,   190,   319,    74,
   184,    75,    76,   147,    77,   127,   125,    78,    79,   312,
    80,   148,    81,    82,   158,    83,    84,    85,    86,   215,
   266,    87,   141,   160,    88,    89,    90,   209,   268,   269,
   161,   162,   163,    91,   193,   270,   304,   164,   306,   307,
   308,   271,    92,    93,   132,   134,   117,   211,   212,   119,
    94,   165,   166,   126,    95,   313,   329,   347,   336,    98,
   196,   325,   342,   346,   348,   349
};

static const short yypact[] = {-32768,
    19,-32768,   -66,   -47,-32768,   -18,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   -14,    -6,    28,    50,    -4,    12,
-32768,   243,    46,-32768,-32768,    53,-32768,-32768,   328,    62,
-32768,    51,    71,   134,    68,-32768,   -52,-32768,-32768,-32768,
    91,    92,    51,    51,-32768,    97,-32768,   100,-32768,   114,
   118,   -46,   141,   126,   127,   128,-32768,    54,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,    51,    51,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   132,-32768,-32768,-32768,-32768,
-32768,   113,   136,-32768,-32768,-32768,-32768,-32768,-32768,   170,
-32768,   120,   138,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   107,-32768,   123,-32768,   101,-32768,
    51,   107,   137,     7,   608,   159,   608,-32768,   107,   142,
   -23,   -46,   151,   -36,   149,   -36,   107,   168,-32768,   -46,
   413,   147,   461,-32768,   148,-32768,   509,   557,-32768,-32768,
-32768,    73,-32768,   141,-32768,   -27,-32768,-32768,   154,    58,
-32768,-32768,-32768,   141,-32768,-32768,-32768,    20,-32768,    51,
-32768,    51,-32768,    51,   152,   153,   160,   162,-32768,    34,
-32768,-32768,-32768,    10,   107,    51,-32768,   154,-32768,     8,
   101,   107,   157,   107,-32768,   -17,   154,-32768,-32768,    51,
   164,-32768,   165,-32768,-32768,-32768,-32768,    51,-32768,   146,
-32768,-32768,    51,-32768,   -12,-32768,-32768,-32768,-32768,-32768,
   161,-32768,-32768,-32768,   107,-32768,   166,   171,   174,   169,
-32768,-32768,   177,   172,-32768,   178,   107,-32768,-32768,   193,
   188,   190,-32768,-32768,   107,-32768,   194,   201,   195,-32768,
-32768,   608,   198,-32768,    51,-32768,    73,-32768,-32768,    73,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
    57,-32768,-32768,-32768,   204,   205,    83,   207,   225,-32768,
   209,   213,-32768,    -1,   107,-32768,-32768,   200,-32768,    83,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   215,-32768,-32768,   173,-32768,-32768,-32768,   -10,-32768,
    13,   216,   107,   217,-32768,-32768,-32768,-32768,   139,   -48,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,   107,-32768,-32768,
-32768,    87,   221,-32768,   -48,-32768,   -48,-32768,   -48,-32768,
-32768,   303,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,   275,-32768,-32768,-32768,-32768,  -170,
-32768,   283,-32768,-32768,-32768,-32768,   277,-32768,-32768,   279,
-32768,   129,-32768,-32768,   -21,-32768,-32768,-32768,-32768,    27,
   179,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   103,   -77,-32768,   220,-32768,-32768,  -149,-32768,-32768,
-32768,-32768,-32768,    44,-32768,-32768,-32768,    52,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   272,-32768,   -24,    25,   -35,
-32768,-32768,-32768,-32768,   -31,   -22,   -20,  -268,-32768,   135,
-32768,-32768,-32768,-32768,-32768,   -25
};


#define	YYLAST		698


static const short yytable[] = {    96,
   111,    97,   120,    35,    25,    44,   214,   109,   239,   118,
   232,   130,   131,   139,   159,   322,    13,   315,   352,     2,
    61,   120,    57,   257,   113,   123,   331,    63,   142,    50,
   124,    57,   248,   332,   140,    14,   149,   150,     3,   240,
   333,   233,    64,   258,   259,   228,   229,   241,     4,   334,
   330,    64,    51,    59,   188,   178,     5,    53,    61,    16,
   249,   340,   197,   208,    66,    63,   250,    61,    20,   213,
   120,   328,     6,    26,    63,    21,   179,   118,   350,    27,
   260,   261,   262,    28,    68,   242,   263,   234,   264,   175,
   265,   243,   170,   235,   335,    48,     7,   219,   191,   176,
   191,   220,   120,   181,   120,   183,   185,   292,   316,    57,
   293,   230,    22,   320,   194,   294,    57,   231,   113,   199,
   210,   202,   120,   295,   210,   205,   207,    23,   120,    64,
    99,    57,   120,    24,   155,   146,    64,   100,   221,   110,
   222,    57,   223,   296,   116,   297,   298,   152,   112,   216,
   121,    64,   331,    62,   238,   299,   300,   301,   302,   332,
   311,    64,   237,   208,   343,    57,   333,   153,   251,   245,
   344,   247,   139,   128,   129,   334,   254,    62,    57,   133,
   113,   256,   135,    69,   217,    64,   172,   173,   218,   154,
   155,   174,    57,   140,   113,    42,   137,   114,    64,    57,
   138,   113,   273,   143,   144,   145,   116,    69,   156,   171,
   157,   114,    64,   151,   281,   123,   115,   167,   182,    64,
   116,   177,   285,   291,   189,   210,   192,   116,   210,   195,
   289,   200,   186,   198,   203,   224,   255,   226,   225,   227,
   246,   252,   253,   315,   272,   324,    30,   274,   275,    31,
    32,   276,    33,   277,   278,   124,   279,    34,    35,   280,
    36,    37,   323,   321,    38,    39,    40,    41,    42,    43,
   282,    44,   283,    45,   284,    46,    47,   286,   287,   288,
     5,    48,   290,    49,    50,   309,   310,    51,   314,    52,
    53,    54,   317,    55,   318,    56,   327,   337,   345,   339,
   338,    57,   353,   105,    72,   107,    58,   108,    59,   187,
    60,    61,   236,    62,   303,   341,   326,   267,    63,   169,
   136,    64,   305,   351,   244,    65,     0,     0,   101,     0,
    66,    30,    67,     0,    31,    32,     0,    33,     0,    68,
     0,     0,   102,    69,     0,    36,   103,     3,     0,    38,
    39,    40,    41,     0,    43,     0,    44,     0,    45,     0,
    46,    47,  -172,     0,     0,     0,     0,     0,    49,     0,
     0,     0,     0,     0,    52,    53,    54,     0,    55,     0,
    56,     0,  -172,     0,     0,     0,     0,  -172,     0,     0,
     0,    58,     0,     0,     0,    60,     0,     0,     0,     0,
     0,     0,     0,     0,  -172,  -172,     0,     0,  -172,     0,
    65,   104,     0,     0,     0,    66,    30,    67,     0,    31,
    32,     0,    33,  -172,     0,  -172,     0,   102,     0,     0,
    36,   103,     0,     0,    38,    39,    40,     0,     0,    43,
     0,    44,     0,    45,     0,     0,    47,     0,     0,     0,
     0,     0,     0,    49,     0,     0,     0,     0,     0,    52,
    53,    54,     0,    55,    30,    56,     0,    31,    32,     0,
    33,     0,     0,     0,     0,   102,    58,     0,    36,   103,
    60,     0,    38,    39,    40,     0,     0,    43,     0,    44,
     0,    45,   198,     0,    47,    65,     0,     0,     0,     0,
    66,    49,    67,     0,     0,     0,     0,    52,    53,    54,
     0,    55,    30,    56,     0,    31,    32,     0,    33,     0,
     0,     0,     0,   102,    58,     0,    36,   103,    60,     0,
    38,    39,    40,     0,     0,    43,     0,    44,     0,    45,
     0,     0,    47,    65,     0,   201,     0,     0,    66,    49,
    67,     0,     0,     0,     0,    52,    53,    54,     0,    55,
    30,    56,     0,    31,    32,     0,    33,     0,     0,     0,
     0,   102,    58,     0,    36,   103,    60,     0,    38,    39,
    40,     0,     0,    43,     0,    44,     0,    45,     0,     0,
    47,    65,   204,     0,     0,     0,    66,    49,    67,     0,
     0,     0,     0,    52,    53,    54,     0,    55,     0,    56,
     0,    30,     0,     0,    31,    32,     0,    33,     0,     0,
    58,     0,   102,     0,    60,    36,   103,     0,     0,    38,
    39,    40,     0,     0,    43,     0,    44,     0,    45,    65,
     0,    47,     0,     0,    66,   206,    67,     0,    49,     0,
     0,     0,     0,     0,    52,    53,    54,     0,    55,     0,
    56,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    58,     0,     0,     0,    60,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    65,     0,     0,     0,     0,    66,     0,    67
};

static const short yycheck[] = {    22,
    32,    22,    34,    16,     9,    29,   156,    29,     1,    34,
     1,    43,    44,    60,    92,   284,    83,    19,     0,     1,
    69,    53,    59,    36,    61,    78,    14,    76,    53,    42,
    83,    59,    50,    21,    81,    83,    68,    69,    20,    32,
    28,    32,    79,    56,    57,    12,    13,    40,    30,    37,
   319,    79,    45,    66,   132,    49,    38,    48,    69,    78,
    78,   330,   140,    91,    88,    76,    84,    69,    83,    97,
   102,    82,    54,    78,    76,    82,    70,   102,   347,    84,
    93,    94,    95,    72,    97,    78,    99,    78,   101,   121,
   103,    84,   115,    84,    82,    39,    78,    78,   134,   122,
   136,    82,   134,   125,   136,   127,   129,   257,   279,    59,
   260,    78,    85,   284,   137,    59,    59,    84,    61,   141,
   152,   143,   154,    67,   156,   147,   148,    78,   160,    79,
    85,    59,   164,    84,    78,    82,    79,    85,   170,    78,
   172,    59,   174,    87,    87,    89,    90,    35,    78,    92,
    83,    79,    14,    71,   186,    99,   100,   101,   102,    21,
    78,    79,   185,    91,    78,    59,    28,    55,   200,   192,
    84,   194,    60,    83,    83,    37,   208,    71,    59,    83,
    61,   213,    83,   101,   160,    79,    86,    87,   164,    77,
    78,    91,    59,    81,    61,    26,    83,    78,    79,    59,
    83,    61,   225,    78,    78,    78,    87,   101,    96,    87,
    98,    78,    79,    82,   237,    78,    83,    82,    60,    79,
    87,    85,   245,   255,    74,   257,    78,    87,   260,    62,
   252,    85,    91,    80,    87,    84,    91,    78,    86,    78,
    84,    78,    78,    19,    84,    46,     4,    82,    78,     7,
     8,    78,    10,    85,    78,    83,    85,    15,    16,    82,
    18,    19,   285,   284,    22,    23,    24,    25,    26,    27,
    78,    29,    85,    31,    85,    33,    34,    84,    78,    85,
    38,    39,    85,    41,    42,    82,    82,    45,    82,    47,
    48,    49,    84,    51,    82,    53,    82,    82,    78,    83,
   323,    59,     0,    29,    22,    29,    64,    29,    66,   131,
    68,    69,   184,    71,   271,   338,   290,   215,    76,   100,
    49,    79,   271,   349,   190,    83,    -1,    -1,     1,    -1,
    88,     4,    90,    -1,     7,     8,    -1,    10,    -1,    97,
    -1,    -1,    15,   101,    -1,    18,    19,    20,    -1,    22,
    23,    24,    25,    -1,    27,    -1,    29,    -1,    31,    -1,
    33,    34,    35,    -1,    -1,    -1,    -1,    -1,    41,    -1,
    -1,    -1,    -1,    -1,    47,    48,    49,    -1,    51,    -1,
    53,    -1,    55,    -1,    -1,    -1,    -1,    60,    -1,    -1,
    -1,    64,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    81,    -1,
    83,    84,    -1,    -1,    -1,    88,     4,    90,    -1,     7,
     8,    -1,    10,    96,    -1,    98,    -1,    15,    -1,    -1,
    18,    19,    -1,    -1,    22,    23,    24,    -1,    -1,    27,
    -1,    29,    -1,    31,    -1,    -1,    34,    -1,    -1,    -1,
    -1,    -1,    -1,    41,    -1,    -1,    -1,    -1,    -1,    47,
    48,    49,    -1,    51,     4,    53,    -1,     7,     8,    -1,
    10,    -1,    -1,    -1,    -1,    15,    64,    -1,    18,    19,
    68,    -1,    22,    23,    24,    -1,    -1,    27,    -1,    29,
    -1,    31,    80,    -1,    34,    83,    -1,    -1,    -1,    -1,
    88,    41,    90,    -1,    -1,    -1,    -1,    47,    48,    49,
    -1,    51,     4,    53,    -1,     7,     8,    -1,    10,    -1,
    -1,    -1,    -1,    15,    64,    -1,    18,    19,    68,    -1,
    22,    23,    24,    -1,    -1,    27,    -1,    29,    -1,    31,
    -1,    -1,    34,    83,    -1,    85,    -1,    -1,    88,    41,
    90,    -1,    -1,    -1,    -1,    47,    48,    49,    -1,    51,
     4,    53,    -1,     7,     8,    -1,    10,    -1,    -1,    -1,
    -1,    15,    64,    -1,    18,    19,    68,    -1,    22,    23,
    24,    -1,    -1,    27,    -1,    29,    -1,    31,    -1,    -1,
    34,    83,    84,    -1,    -1,    -1,    88,    41,    90,    -1,
    -1,    -1,    -1,    47,    48,    49,    -1,    51,    -1,    53,
    -1,     4,    -1,    -1,     7,     8,    -1,    10,    -1,    -1,
    64,    -1,    15,    -1,    68,    18,    19,    -1,    -1,    22,
    23,    24,    -1,    -1,    27,    -1,    29,    -1,    31,    83,
    -1,    34,    -1,    -1,    88,    89,    90,    -1,    41,    -1,
    -1,    -1,    -1,    -1,    47,    48,    49,    -1,    51,    -1,
    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    64,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    83,    -1,    -1,    -1,    -1,    88,    -1,    90
};
#define YYPURE 1

/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 196 "/usr/lib/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 2:
#line 259 "parser.yy"
{
		delete THIS->default_header_p_ ;
		THIS->default_header_p_ = yyvsp[0].header;
	;
    break;}
case 3:
#line 263 "parser.yy"
{
		add_score (yyvsp[0].score);
	;
    break;}
case 4:
#line 266 "parser.yy"
{ ;
    break;}
case 6:
#line 268 "parser.yy"
{ ;
    break;}
case 7:
#line 269 "parser.yy"
{ ;
    break;}
case 8:
#line 273 "parser.yy"
{
		if (String (*yyvsp[-1].string) != MUDELA_VERSION) {
			if (THIS->ignore_version_b_) {
				THIS->here_input ().error ("Incorrect mudela version");
			} else {
				THIS->fatal_error_i_ = 1;
				THIS->parser_error ("Incorrect mudela version");
			}
		}
	;
    break;}
case 10:
#line 289 "parser.yy"
{
	;
    break;}
case 11:
#line 291 "parser.yy"
{
		THIS->clear_notenames ();
	;
    break;}
case 12:
#line 294 "parser.yy"
{
		THIS->add_notename (*yyvsp[-2].string, yyvsp[0].melreq);
		delete yyvsp[-2].string;
	;
    break;}
case 13:
#line 301 "parser.yy"
{
		yyval.header = new Header;
	;
    break;}
case 14:
#line 304 "parser.yy"
{
		(*yyval.header)[*yyvsp[-3].string] = *yyvsp[-1].string;
		delete yyvsp[-3].string;
		delete yyvsp[-1].string;
	;
    break;}
case 15:
#line 312 "parser.yy"
{
		yyval.header = yyvsp[-1].header;
	;
    break;}
case 16:
#line 319 "parser.yy"
{
		yyval.string = new String;
	;
    break;}
case 17:
#line 322 "parser.yy"
{
		*yyval.string += *yyvsp[0].string;
	;
    break;}
case 18:
#line 332 "parser.yy"
{
		THIS->remember_spot ();
	;
    break;}
case 19:
#line 335 "parser.yy"
{
	    THIS->lexer_p_->set_identifier (*yyvsp[-3].string, yyvsp[0].id);
	    yyvsp[0].id->init_b_ = THIS->init_parse_b_;
	    yyvsp[0].id->set_spot (THIS->pop_spot ());
	;
    break;}
case 20:
#line 343 "parser.yy"
{
		yyval.id = new Score_id (yyvsp[0].score, SCORE_IDENTIFIER);

	;
    break;}
case 21:
#line 347 "parser.yy"
{
		yyval.id = new Paper_def_id (yyvsp[0].paper, PAPER_IDENTIFIER);

	;
    break;}
case 22:
#line 351 "parser.yy"
{
		yyval.id = new Midi_def_id (yyvsp[0].midi, MIDI_IDENTIFIER);

	;
    break;}
case 23:
#line 355 "parser.yy"
{
		yyval.id = new Script_id (yyvsp[0].script, SCRIPT_IDENTIFIER);

	;
    break;}
case 24:
#line 359 "parser.yy"
{
		yyval.id = new Music_id (yyvsp[0].music, MUSIC_IDENTIFIER);

	;
    break;}
case 25:
#line 363 "parser.yy"
{
		yyval.id = new Lookup_id (yyvsp[0].lookup, IDENTIFIER);

	;
    break;}
case 26:
#line 367 "parser.yy"
{
		yyval.id = new Real_id (new Real (yyvsp[0].real), REAL_IDENTIFIER);

	;
    break;}
case 27:
#line 371 "parser.yy"
{
		yyval.id = new Int_id (new int (yyvsp[0].i), INT_IDENTIFIER);
	;
    break;}
case 28:
#line 374 "parser.yy"
{
		yyval.id = new Request_id (yyvsp[0].request, POST_REQUEST_IDENTIFIER);
	;
    break;}
case 29:
#line 377 "parser.yy"
{
		yyval.id = new Request_id (yyvsp[0].melreq, MELODIC_REQUEST_IDENTIFIER);

	;
    break;}
case 30:
#line 381 "parser.yy"
{
		yyval.id = new Translator_id (yyvsp[0].trans, TRANS_IDENTIFIER);
	;
    break;}
case 31:
#line 384 "parser.yy"
{
		yyval.id = new Duration_id (yyvsp[0].duration, DURATION_IDENTIFIER);
	;
    break;}
case 32:
#line 393 "parser.yy"
{ yyval.trans = yyvsp[-1].trans; ;
    break;}
case 33:
#line 397 "parser.yy"
{
		yyval.trans = yyvsp[0].id->translator ();
		yyval.trans-> set_spot (THIS->here_input ());
	;
    break;}
case 34:
#line 401 "parser.yy"
{
		yyval.trans = get_translator_l (*yyvsp[-1].string)->clone ();
		yyval.trans->set_spot (THIS->here_input ());
		delete yyvsp[-1].string;
	;
    break;}
case 35:
#line 406 "parser.yy"
{
		yyval.trans-> set_property (*yyvsp[-3].string, *yyvsp[-1].scalar);
		delete yyvsp[-3].string;
		delete yyvsp[-1].scalar;
	;
    break;}
case 36:
#line 411 "parser.yy"
{
		yyval.trans->group_l ()->consists_str_arr_.push (*yyvsp[-1].string);
		delete yyvsp[-1].string;
	;
    break;}
case 37:
#line 415 "parser.yy"
{
		yyval.trans->group_l ()->accepts_str_arr_.push (*yyvsp[-1].string);
		delete yyvsp[-1].string;
	;
    break;}
case 38:
#line 425 "parser.yy"
{ THIS->remember_spot ();
		THIS->error_level_i_ =0;
	;
    break;}
case 39:
#line 428 "parser.yy"
{
		yyval.score = yyvsp[-1].score;
		yyval.score->set_spot (THIS->pop_spot ());
		if (!yyval.score->def_p_arr_.size ())
			yyval.score->add (THIS->default_paper ());

		/* handle error levels. */
		yyval.score->errorlevel_i_ = THIS->error_level_i_;
		THIS->error_level_i_ = 0;
		if (!yyval.score->header_p_ && THIS->default_header_p_)
			yyval.score->header_p_ = new Header (*THIS->default_header_p_);
	;
    break;}
case 40:
#line 442 "parser.yy"
{
		yyval.score = new Score;
	;
    break;}
case 41:
#line 445 "parser.yy"
{
		yyval.score = yyvsp[0].id->score ();
	;
    break;}
case 42:
#line 448 "parser.yy"
{
		yyval.score->header_p_ = yyvsp[0].header;
	;
    break;}
case 43:
#line 451 "parser.yy"
{
		if (yyval.score->music_p_)
			yyvsp[0].music->warning ("More than one music block");	
		yyval.score->music_p_ = yyvsp[0].music;
	;
    break;}
case 44:
#line 456 "parser.yy"
{
		yyval.score->add (yyvsp[0].outputdef);
	;
    break;}
case 45:
#line 459 "parser.yy"
{

	;
    break;}
case 46:
#line 465 "parser.yy"
{
		yyval.outputdef = yyvsp[0].paper;
	;
    break;}
case 47:
#line 468 "parser.yy"
{
		yyval.outputdef= yyvsp[0].midi;
	;
    break;}
case 48:
#line 474 "parser.yy"
{ yyval.intvec =new Array<int>; ;
    break;}
case 49:
#line 475 "parser.yy"
{
		yyval.intvec->push (yyvsp[-2].i); yyval.intvec->push (yyvsp[0].i);
	;
    break;}
case 50:
#line 486 "parser.yy"
{ yyval.paper = yyvsp[-1].paper; ;
    break;}
case 51:
#line 490 "parser.yy"
{
		yyval.paper = THIS->default_paper ();
	;
    break;}
case 52:
#line 493 "parser.yy"
{
		yyval.paper = yyvsp[0].id->paperdef ();
	;
    break;}
case 53:
#line 496 "parser.yy"
{ 
		yyval.paper->outfile_str_ = *yyvsp[-1].string;
		delete yyvsp[-1].string;
	;
    break;}
case 54:
#line 500 "parser.yy"
{ yyval.paper->set (yyvsp[0].lookup); ;
    break;}
case 55:
#line 501 "parser.yy"
{
		yyval.paper->set_var (*yyvsp[-3].string, yyvsp[-1].real);
	;
    break;}
case 56:
#line 504 "parser.yy"
{
		yyval.paper->set_var (*yyvsp[-3].string, yyvsp[-1].real);
	;
    break;}
case 57:
#line 507 "parser.yy"
{
		yyval.paper-> assign_translator (*yyvsp[-2].string, yyvsp[0].trans);
		delete yyvsp[-2].string;
	;
    break;}
case 58:
#line 511 "parser.yy"
{
		yyval.paper->shape_int_a_ = *yyvsp[-1].intarr;
		delete yyvsp[-1].intarr;
	;
    break;}
case 59:
#line 515 "parser.yy"
{

	;
    break;}
case 60:
#line 521 "parser.yy"
{
		yyval.intarr = new Array<Interval>;
	;
    break;}
case 61:
#line 524 "parser.yy"
{
		yyval.intarr->push(Interval(yyvsp[-1].real, yyvsp[-1].real + yyvsp[0].real));
	;
    break;}
case 62:
#line 534 "parser.yy"
{ yyval.midi = yyvsp[-1].midi; ;
    break;}
case 63:
#line 537 "parser.yy"
{
		yyval.midi = THIS->default_midi ();
	;
    break;}
case 64:
#line 540 "parser.yy"
{
		yyval.midi-> assign_translator (*yyvsp[-2].string, yyvsp[0].trans);
		delete yyvsp[-2].string;
	;
    break;}
case 65:
#line 544 "parser.yy"
{
		yyval.midi->outfile_str_ = *yyvsp[-1].string;
		delete yyvsp[-1].string;
	;
    break;}
case 66:
#line 548 "parser.yy"
{
		yyval.midi->set_tempo (yyvsp[-1].tempo->dur_.length (), yyvsp[-1].tempo->metronome_i_);
		delete yyvsp[-1].tempo;
	;
    break;}
case 67:
#line 552 "parser.yy"
{

	;
    break;}
case 68:
#line 558 "parser.yy"
{
		yyval.tempo = new Tempo_req;
		yyval.tempo->dur_ = *yyvsp[-2].duration;
		delete yyvsp[-2].duration;
		yyval.tempo-> metronome_i_ = yyvsp[0].i;
	;
    break;}
case 69:
#line 571 "parser.yy"
{
		yyval.musiclist = yyvsp[-1].musiclist;
	;
    break;}
case 70:
#line 577 "parser.yy"
{
		yyval.musiclist = new Voice;
		yyval.musiclist->set_spot (THIS->here_input ());
	;
    break;}
case 71:
#line 581 "parser.yy"
{
		yyval.musiclist->add (yyvsp[0].music);
	;
    break;}
case 72:
#line 587 "parser.yy"
{ yyval.music = yyvsp[0].music; ;
    break;}
case 73:
#line 588 "parser.yy"
{
		yyval.music = yyvsp[0].music;
		yyval.music->translator_type_str_ = *yyvsp[-1].string;
		delete yyvsp[-1].string;
	;
    break;}
case 74:
#line 593 "parser.yy"
{
		yyval.music = yyvsp[0].music;
		yyval.music->translator_type_str_ = *yyvsp[-3].string;
		yyval.music->translator_id_str_ = *yyvsp[-1].string;
		delete yyvsp[-3].string;
		delete yyvsp[-1].string;
	;
    break;}
case 75:
#line 600 "parser.yy"
{ yyval.music = yyvsp[0].musiclist; ;
    break;}
case 76:
#line 601 "parser.yy"
{ yyval.music = yyvsp[0].chord; ;
    break;}
case 77:
#line 602 "parser.yy"
{ yyval.music = yyvsp[0].music; ;
    break;}
case 78:
#line 603 "parser.yy"
{ yyval.music = yyvsp[0].id->music (); ;
    break;}
case 79:
#line 604 "parser.yy"
{ yyval.music = yyvsp[-1].id->music (); ;
    break;}
case 80:
#line 606 "parser.yy"
{ THIS->lexer_p_->push_note_state (); ;
    break;}
case 81:
#line 608 "parser.yy"
{ yyval.music=yyvsp[0].music; THIS->lexer_p_->pop_state (); ;
    break;}
case 82:
#line 611 "parser.yy"
{ THIS->lexer_p_->push_lyric_state (); ;
    break;}
case 83:
#line 613 "parser.yy"
{ yyval.music = yyvsp[0].music; THIS->lexer_p_->pop_state (); ;
    break;}
case 86:
#line 619 "parser.yy"
{
		Change_translator * t = new Change_translator;
		t-> change_to_type_str_ = *yyvsp[-2].string;
		t-> change_to_id_str_ = *yyvsp[0].string;

		yyval.music = t;
		yyval.music->set_spot (THIS->here_input ());
		delete yyvsp[-2].string;
		delete yyvsp[0].string;
	;
    break;}
case 87:
#line 632 "parser.yy"
{
		Translation_property *t = new Translation_property;
		t-> translator_type_str_ = *yyvsp[-4].string;
		t-> var_str_ = *yyvsp[-2].string;
		t-> value_ = *yyvsp[0].scalar;
		yyval.music = t;
		yyval.music->set_spot (THIS->here_input ());
		delete yyvsp[-4].string;
		delete yyvsp[-2].string;
		delete yyvsp[0].scalar;
	;
    break;}
case 88:
#line 646 "parser.yy"
{ yyval.scalar = new Scalar (*yyvsp[0].string); delete yyvsp[0].string; ;
    break;}
case 89:
#line 647 "parser.yy"
{ yyval.scalar = new Scalar (yyvsp[0].i); ;
    break;}
case 90:
#line 652 "parser.yy"
{ yyval.chord  = yyvsp[-1].chord; ;
    break;}
case 91:
#line 653 "parser.yy"
{
		yyval.chord = yyvsp[0].chord;
		yyval.chord->multi_level_i_=yyvsp[-1].i;
	;
    break;}
case 92:
#line 660 "parser.yy"
{
		yyval.chord = new Chord;
		yyval.chord-> multi_level_i_ = 1;
		yyval.chord->set_spot (THIS->here_input ());
	;
    break;}
case 93:
#line 665 "parser.yy"
{
		yyval.chord->add (yyvsp[0].music);
	;
    break;}
case 94:
#line 671 "parser.yy"
{
		yyval.music = yyvsp[0].music;
		yyval.music -> transpose (yyvsp[-1].melreq);

		delete yyvsp[-1].melreq;
	;
    break;}
case 95:
#line 684 "parser.yy"
{
	 	THIS->add_requests ((Chord*)yyvsp[-1].music);//ugh
 		yyval.music = yyvsp[-1].music;
	;
    break;}
case 97:
#line 689 "parser.yy"
{ yyval.music = 0; ;
    break;}
case 100:
#line 698 "parser.yy"
{
		yyval.music = new Request_chord;
		yyval.music-> set_spot (THIS->here_input ());
		yyvsp[0].request-> set_spot (THIS->here_input ());
		((Chord*)yyval.music) ->add (yyvsp[0].request);//ugh

	;
    break;}
case 102:
#line 709 "parser.yy"
{ yyval.request = yyvsp[-1].request; ;
    break;}
case 103:
#line 713 "parser.yy"
{
		yyval.request = new Barcheck_req;
	;
    break;}
case 104:
#line 716 "parser.yy"
{
		yyval.request = yyvsp[0].id->request ();
	;
    break;}
case 105:
#line 722 "parser.yy"
{
		yyval.request = new Bar_req (*yyvsp[0].string);
		delete yyvsp[0].string;
	;
    break;}
case 106:
#line 726 "parser.yy"
{
		Break_force_req * f = new Break_force_req;
		f-> set_spot (THIS->here_input ());
		yyval.request = f;
	;
    break;}
case 107:
#line 731 "parser.yy"
{
		Meter_change_req *m = new Meter_change_req;
		m->set (yyvsp[-2].i,yyvsp[0].i);
		yyval.request = m;
	;
    break;}
case 108:
#line 736 "parser.yy"
{
		Skip_req * skip_p = new Skip_req;
		skip_p->duration_.set_plet (yyvsp[0].moment->num (),
			yyvsp[0].moment->den ());

		delete yyvsp[0].moment;
		yyval.request = skip_p;
	;
    break;}
case 109:
#line 744 "parser.yy"
{
		yyval.request = yyvsp[0].tempo;
	;
    break;}
case 110:
#line 747 "parser.yy"
{
		yyval.request = new Cadenza_req (yyvsp[0].i);
	;
    break;}
case 111:
#line 750 "parser.yy"
{
		yyval.request = new Partial_measure_req (*yyvsp[0].moment);
		delete yyvsp[0].moment;
	;
    break;}
case 112:
#line 754 "parser.yy"
{
		yyval.request = new Clef_change_req (*yyvsp[0].string);
		delete yyvsp[0].string;
	;
    break;}
case 113:
#line 758 "parser.yy"
{
		Key_change_req *key_p= new Key_change_req;
		key_p->melodic_p_arr_ = *yyvsp[0].melreqvec;
		yyval.request = key_p;
		delete yyvsp[0].melreqvec;
	;
    break;}
case 114:
#line 764 "parser.yy"
{
		yyval.request = get_grouping_req (*yyvsp[0].intvec); delete yyvsp[0].intvec;
	;
    break;}
case 115:
#line 770 "parser.yy"
{
		/* something silly happened.  Junk this stuff*/
		if (!THIS->post_reqs.empty ())
		{
			warning ("Junking post-requests");
			THIS->post_reqs.clear ();
		}
	;
    break;}
case 116:
#line 778 "parser.yy"
{
		yyvsp[0].request->set_spot (THIS->here_input ());
		THIS->post_reqs.push (yyvsp[0].request);
	;
    break;}
case 117:
#line 782 "parser.yy"
{
		Array<Request*>& r = *THIS->get_parens_request (yyvsp[0].i);
		for (int i = 0; i < r.size (); i++ )
			r[i]->set_spot (THIS->here_input ());
		THIS->post_reqs.concat (r);
		delete &r;
	;
    break;}
case 120:
#line 797 "parser.yy"
{
		yyval.request = (Request*)yyvsp[0].id->request ();
	;
    break;}
case 121:
#line 800 "parser.yy"
{
		yyval.request = yyvsp[0].request;
	;
    break;}
case 122:
#line 803 "parser.yy"
{
		Abbreviation_req* a = new Abbreviation_req;
		a->type_i_ = yyvsp[0].i;
		yyval.request = a;
	;
    break;}
case 123:
#line 816 "parser.yy"
{
		yyval.melreq = yyvsp[0].melreq->clone ()->musical ()->melodic ();
		yyval.melreq->octave_i_ += THIS->default_octave_i_;
	;
    break;}
case 124:
#line 820 "parser.yy"
{
		yyval.melreq-> octave_i_ += yyvsp[0].i;
	;
    break;}
case 125:
#line 823 "parser.yy"
{
		yyval.melreq = yyvsp[0].melreq;
		yyvsp[0].melreq-> octave_i_ -= yyvsp[-1].i;
	;
    break;}
case 126:
#line 830 "parser.yy"
{
		yyval.notereq = new Note_req;
		* (Melodic_req *) yyval.notereq = *yyvsp[0].melreq;
		delete yyvsp[0].melreq;
	;
    break;}
case 127:
#line 835 "parser.yy"
{
		yyval.notereq->forceacc_b_ = ! yyval.notereq->forceacc_b_;
	;
    break;}
case 128:
#line 842 "parser.yy"
{/* ugh */
		yyval.melreq = new Melodic_req;
		yyval.melreq->octave_i_ = yyvsp[-3].i;
		yyval.melreq->notename_i_ = yyvsp[-2].i;
		yyval.melreq->accidental_i_ = yyvsp[-1].i;
	;
    break;}
case 129:
#line 851 "parser.yy"
{
		yyval.duration = new Duration;
		yyval.duration-> durlog_i_ = yyvsp[-2].i;
		yyval.duration-> dots_i_ = yyvsp[-1].i;
	;
    break;}
case 130:
#line 859 "parser.yy"
{
		Absolute_dynamic_req *ad_p = new Absolute_dynamic_req;
		ad_p ->loudness_ = (Dynamic_req::Loudness)yyvsp[-1].i;
		yyval.request =ad_p;
	;
    break;}
case 131:
#line 864 "parser.yy"
{
		Span_dynamic_req * sp_p = new Span_dynamic_req;
		sp_p->spantype = yyvsp[-1].i;
		sp_p-> dynamic_dir_  = yyvsp[-2].i;
		yyval.request = sp_p;
	;
    break;}
case 132:
#line 873 "parser.yy"
{
		yyval.pair[0] = yyvsp[-2].i;
		yyval.pair[1] = yyvsp[0].i;
	;
    break;}
case 133:
#line 878 "parser.yy"
{
		int num = yyvsp[0].i >? 1;
		yyval.pair[0] = guess_plet_a[(num <? GUESS_PLET) - 1];
		yyval.pair[1] = num;
	;
    break;}
case 134:
#line 886 "parser.yy"
{
		yyval.i = MAEBTELP;
		THIS->plet_.type_i_ = yyvsp[0].pair[1];
		THIS->plet_.iso_i_ = yyvsp[0].pair[0];
		THIS->default_duration_.plet_.type_i_ = yyvsp[0].pair[1];
		THIS->default_duration_.plet_.iso_i_ = yyvsp[0].pair[0];
	;
    break;}
case 135:
#line 893 "parser.yy"
{
		yyval.i = TELP;
		THIS->plet_.type_i_ = 1;
		THIS->plet_.iso_i_ = 1;
		THIS->default_duration_.plet_.iso_i_ = 1;
		THIS->default_duration_.plet_.type_i_ = 1;
	;
    break;}
case 136:
#line 900 "parser.yy"
{
		yyval.i = TELP;
		THIS->plet_.type_i_ = yyvsp[0].pair[1];
		THIS->plet_.iso_i_ = yyvsp[0].pair[0];
		THIS->default_duration_.plet_.type_i_ = yyvsp[0].pair[1];
		THIS->default_duration_.plet_.iso_i_ = yyvsp[0].pair[0];
	;
    break;}
case 137:
#line 910 "parser.yy"
{
		yyval.i = '~';
	;
    break;}
case 138:
#line 913 "parser.yy"
{
		yyval.i='(';
	;
    break;}
case 139:
#line 916 "parser.yy"
{
		yyval.i = ']';
	;
    break;}
case 140:
#line 919 "parser.yy"
{
		yyval.i = '<';
	;
    break;}
case 141:
#line 922 "parser.yy"
{
		yyval.i = '>';
	;
    break;}
case 142:
#line 925 "parser.yy"
{
	;
    break;}
case 143:
#line 930 "parser.yy"
{
		yyval.c = '[';
		if (!Duration::duration_type_b (yyvsp[0].i))
			THIS->parser_error ("1:Not a duration");
		else if (yyvsp[0].i < 8)
			THIS->parser_error ("Can't abbreviate");
		else
			THIS->set_abbrev_beam (yyvsp[0].i);
	;
    break;}
case 144:
#line 942 "parser.yy"
{
		yyval.i = BEAMPLET;
		THIS->plet_.type_i_ = yyvsp[0].pair[1];
		THIS->plet_.iso_i_ = yyvsp[0].pair[0];
		THIS->default_duration_.plet_.type_i_ = yyvsp[0].pair[1];
		THIS->default_duration_.plet_.iso_i_ = yyvsp[0].pair[0];
	;
    break;}
case 145:
#line 949 "parser.yy"
{
		yyval.i = PLET;
		THIS->plet_.type_i_ = yyvsp[0].pair[1];
		THIS->plet_.iso_i_ = yyvsp[0].pair[0];
		THIS->default_duration_.plet_.type_i_ = yyvsp[0].pair[1];
		THIS->default_duration_.plet_.iso_i_ = yyvsp[0].pair[0];
	;
    break;}
case 146:
#line 959 "parser.yy"
{
		yyval.i = '!';
	;
    break;}
case 147:
#line 962 "parser.yy"
{
		yyval.i=')';
	;
    break;}
case 148:
#line 965 "parser.yy"
{
		yyval.i='[';
	;
    break;}
case 149:
#line 968 "parser.yy"
{
	;
    break;}
case 150:
#line 970 "parser.yy"
{
	;
    break;}
case 151:
#line 977 "parser.yy"
{ yyval.script = yyvsp[-1].script; ;
    break;}
case 152:
#line 981 "parser.yy"
{
		Script_def *s = new Script_def;
		s->set_from_input (*yyvsp[-5].string,yyvsp[-4].i, yyvsp[-3].i,yyvsp[-2].i,yyvsp[-1].i, yyvsp[0].i);
		yyval.script  = s;
		delete yyvsp[-5].string;
	;
    break;}
case 153:
#line 990 "parser.yy"
{
		Musical_script_req *m = new Musical_script_req;
		yyval.request = m;
		m->scriptdef_p_ = yyvsp[0].script;
		m->set_spot (THIS->here_input ());
		if (!m->dir_)
		  m->dir_  = yyvsp[-1].i;
	;
    break;}
case 154:
#line 1001 "parser.yy"
{ 
		yyval.script = yyvsp[0].textdef;
		((Text_def*) yyval.script)->align_i_ = CENTER; /* UGH */
	;
    break;}
case 155:
#line 1005 "parser.yy"
{ 
		yyval.script = yyvsp[0].script;
		yyval.script-> set_spot (THIS->here_input ());
	;
    break;}
case 156:
#line 1009 "parser.yy"
{
		yyval.script = yyvsp[0].textdef;
		((Text_def*)yyval.script)->align_i_ = CENTER;
	;
    break;}
case 157:
#line 1016 "parser.yy"
{
		Text_def *t  = new Text_def;
		yyval.textdef = t;
		t->text_str_ = *yyvsp[0].string;
		delete yyvsp[0].string;
		t->style_str_ = THIS->textstyle_str_;
		yyval.textdef->set_spot (THIS->here_input ());
	;
    break;}
case 158:
#line 1027 "parser.yy"
{
		Text_def* t  = new Text_def;
		yyval.textdef = t;
		t->text_str_ = String (yyvsp[0].i);
		t->style_str_ = "finger";
		yyval.textdef->set_spot (THIS->here_input ());
	;
    break;}
case 159:
#line 1037 "parser.yy"
{ yyval.string = get_scriptdef ('^'); ;
    break;}
case 160:
#line 1038 "parser.yy"
{ yyval.string = get_scriptdef ('+'); ;
    break;}
case 161:
#line 1039 "parser.yy"
{ yyval.string = get_scriptdef ('-'); ;
    break;}
case 162:
#line 1040 "parser.yy"
{ yyval.string = get_scriptdef ('|'); ;
    break;}
case 163:
#line 1041 "parser.yy"
{ yyval.string = get_scriptdef ('o'); ;
    break;}
case 164:
#line 1042 "parser.yy"
{ yyval.string = get_scriptdef ('>'); ;
    break;}
case 165:
#line 1043 "parser.yy"
{
		yyval.string = get_scriptdef ('.');
	;
    break;}
case 166:
#line 1049 "parser.yy"
{ yyval.script = yyvsp[0].id->script (); ;
    break;}
case 167:
#line 1050 "parser.yy"
{ yyval.script = yyvsp[0].script; ;
    break;}
case 168:
#line 1051 "parser.yy"
{
		yyval.script = THIS->lexer_p_->lookup_identifier (*yyvsp[0].string)->script ();
		delete yyvsp[0].string;
	;
    break;}
case 169:
#line 1058 "parser.yy"
{ yyval.i = -1; ;
    break;}
case 170:
#line 1059 "parser.yy"
{ yyval.i = 1; ;
    break;}
case 171:
#line 1060 "parser.yy"
{ yyval.i = 0; ;
    break;}
case 172:
#line 1064 "parser.yy"
{
	;
    break;}
case 173:
#line 1066 "parser.yy"
{
		Array<Request*>& r = *THIS->get_parens_request (yyvsp[0].i);
		for (int i = 0; i < r.size (); i++ )
			r[i]->set_spot (THIS->here_input ());
		THIS->pre_reqs.concat (r);
		delete &r;
	;
    break;}
case 174:
#line 1076 "parser.yy"
{
		THIS->set_duration_mode (*yyvsp[0].string);
		delete yyvsp[0].string;
	;
    break;}
case 175:
#line 1080 "parser.yy"
{
		THIS->set_default_duration (yyvsp[0].duration);
		delete yyvsp[0].duration;
	;
    break;}
case 176:
#line 1084 "parser.yy"
{
		/*
			This is weird, but default_octave_i_
			is used in steno_note_req too

			c' -> default_octave_i_ == 1
		*/
		/* why can't we have \oct 0 iso \oct{c'}*/
		THIS->default_octave_i_ = 1; ;
    break;}
case 177:
#line 1094 "parser.yy"
{
		THIS->default_octave_i_ = yyvsp[0].melreq->octave_i_;
		delete yyvsp[0].melreq;
	;
    break;}
case 178:
#line 1098 "parser.yy"
{
		THIS->textstyle_str_ = *yyvsp[0].string;
		delete yyvsp[0].string;
	;
    break;}
case 179:
#line 1105 "parser.yy"
{
		yyval.moment = new Moment (0,1);
	;
    break;}
case 180:
#line 1108 "parser.yy"
{
		*yyval.moment += yyvsp[0].duration->length ();
	;
    break;}
case 181:
#line 1114 "parser.yy"
{ yyval.i = 1; ;
    break;}
case 182:
#line 1115 "parser.yy"
{ yyval.i ++; ;
    break;}
case 183:
#line 1119 "parser.yy"
{
		yyval.duration = new Duration (THIS->default_duration_);
	;
    break;}
case 184:
#line 1122 "parser.yy"
{
		yyval.duration = new Duration (THIS->default_duration_);
		yyval.duration->dots_i_  = yyvsp[0].i;
	;
    break;}
case 185:
#line 1126 "parser.yy"
{
		THIS->set_last_duration (yyvsp[0].duration);
		yyval.duration = yyvsp[0].duration;
	;
    break;}
case 186:
#line 1133 "parser.yy"
{
		yyval.duration = yyvsp[0].duration;
	;
    break;}
case 187:
#line 1139 "parser.yy"
{
		yyval.duration = new Duration;
		if (!Duration::duration_type_b (yyvsp[0].i))
			THIS->parser_error ("2:Not a duration");
		else {
			yyval.duration->durlog_i_ = Duration_convert::i2_type (yyvsp[0].i);
			yyval.duration->set_plet (THIS->default_duration_);
		     }
	;
    break;}
case 188:
#line 1148 "parser.yy"
{
		yyval.duration = yyvsp[0].id->duration ();
	;
    break;}
case 189:
#line 1151 "parser.yy"
{
		yyval.duration->dots_i_ ++;
	;
    break;}
case 190:
#line 1154 "parser.yy"
{
		yyval.duration->plet_.iso_i_ = yyvsp[0].i;
	;
    break;}
case 191:
#line 1157 "parser.yy"
{
		yyval.duration->plet_.type_i_ = yyvsp[0].i;
	;
    break;}
case 192:
#line 1164 "parser.yy"
{
		yyval.i =0;
	;
    break;}
case 193:
#line 1167 "parser.yy"
{
		if (!Duration::duration_type_b (yyvsp[0].i))
			THIS->parser_error ("3:Not a duration");
		else if (yyvsp[0].i < 8)
			THIS->parser_error ("Can't abbreviate");
		yyval.i = yyvsp[0].i;
	;
    break;}
case 194:
#line 1178 "parser.yy"
{
		if (!THIS->lexer_p_->note_state_b ())
			THIS->parser_error ("have to be in Note mode for notes");
		yyvsp[-1].notereq->set_duration (*yyvsp[0].duration);
		int durlog_i = yyvsp[0].duration->durlog_i_;
		yyval.music = THIS->get_note_element (yyvsp[-1].notereq, yyvsp[0].duration);
	;
    break;}
case 195:
#line 1185 "parser.yy"
{
		yyval.music = THIS->get_rest_element (*yyvsp[-1].string, yyvsp[0].duration);
		delete yyvsp[-1].string;
	;
    break;}
case 196:
#line 1192 "parser.yy"
{
	/* this sux! text-def should be feature of lyric-engraver. */
		if (!THIS->lexer_p_->lyric_state_b ())
			THIS->parser_error ("Have to be in Lyric mode for lyrics");
		yyval.music = THIS->get_word_element (yyvsp[-1].textdef, yyvsp[0].duration);

	;
    break;}
case 197:
#line 1203 "parser.yy"
{
		yyval.melreqvec = new Array<Melodic_req*>;
	;
    break;}
case 198:
#line 1206 "parser.yy"
{
		yyval.melreqvec->push (yyvsp[0].melreq->clone ()->musical ()->melodic ());
	;
    break;}
case 199:
#line 1212 "parser.yy"
{
		yyval.i = yyvsp[0].i;
	;
    break;}
case 200:
#line 1215 "parser.yy"
{
		yyval.i = yyvsp[0].i;
	;
    break;}
case 201:
#line 1220 "parser.yy"
{
		yyval.i = yyvsp[0].i;
	;
    break;}
case 202:
#line 1223 "parser.yy"
{
		yyval.i = -yyvsp[0].i;
	;
    break;}
case 203:
#line 1226 "parser.yy"
{
		int *i_p = yyvsp[0].id->intid ();
		yyval.i = *i_p;
		delete i_p;
	;
    break;}
case 204:
#line 1234 "parser.yy"
{
		yyval.real = yyvsp[0].real;
	;
    break;}
case 205:
#line 1237 "parser.yy"
{
		Real *r_p = yyvsp[0].id->real ();
		yyval.real = * r_p;
		delete r_p;
	;
    break;}
case 206:
#line 1247 "parser.yy"
{ yyval.real = yyvsp[-1].real*yyvsp[0].real; ;
    break;}
case 207:
#line 1251 "parser.yy"
{ yyval.real = 1 CM; ;
    break;}
case 208:
#line 1252 "parser.yy"
{ yyval.real = 1 INCH; ;
    break;}
case 209:
#line 1253 "parser.yy"
{ yyval.real = 1 MM; ;
    break;}
case 210:
#line 1254 "parser.yy"
{ yyval.real = 1 PT; ;
    break;}
case 211:
#line 1261 "parser.yy"
{ yyval.lookup = yyvsp[-1].lookup; ;
    break;}
case 212:
#line 1265 "parser.yy"
{
		yyval.lookup = new Lookup;
	;
    break;}
case 213:
#line 1268 "parser.yy"
{
		yyval.lookup = yyvsp[0].id->lookup ();
	;
    break;}
case 214:
#line 1271 "parser.yy"
{
		yyval.lookup->texsetting = *yyvsp[0].string;
		delete yyvsp[0].string;
	;
    break;}
case 215:
#line 1275 "parser.yy"
{
		yyval.lookup->add (*yyvsp[-2].string, yyvsp[0].symtable);
		delete yyvsp[-2].string;
	;
    break;}
case 216:
#line 1282 "parser.yy"
{ yyval.symtable = yyvsp[-1].symtable; ;
    break;}
case 217:
#line 1286 "parser.yy"
{ yyval.symtable = new Symtable; ;
    break;}
case 218:
#line 1287 "parser.yy"
{
		yyval.symtable->add (*yyvsp[-1].string, *yyvsp[0].symbol);
		delete yyvsp[-1].string;
		delete yyvsp[0].symbol;
	;
    break;}
case 219:
#line 1295 "parser.yy"
{
		yyval.symbol = new Atom (*yyvsp[-1].string, *yyvsp[0].box);
		delete yyvsp[-1].string;
		delete yyvsp[0].box;
	;
    break;}
case 220:
#line 1300 "parser.yy"
{
		Box b (Interval (0,0), Interval (0,0));
		yyval.symbol = new Atom (*yyvsp[0].string, b);
		delete yyvsp[0].string;
	;
    break;}
case 221:
#line 1308 "parser.yy"
{
		yyval.box = new Box (*yyvsp[-1].interval, *yyvsp[0].interval);
		delete yyvsp[-1].interval;
		delete yyvsp[0].interval;
	;
    break;}
case 222:
#line 1315 "parser.yy"
{
		yyval.interval = new Interval (yyvsp[-1].real, yyvsp[0].real);
	;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 498 "/usr/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 1320 "parser.yy"


void
My_lily_parser::set_yydebug (bool b)
{
#ifdef YYDEBUG
	yydebug = b;
#endif
}
void
My_lily_parser::do_yyparse ()
{
	yyparse ((void*)this);
}

Paper_def*
My_lily_parser::default_paper ()
{
	Identifier *id = lexer_p_->lookup_identifier ("default_paper");
	return id ? id->paperdef () : new Paper_def ;
}

Midi_def*
My_lily_parser::default_midi ()
{
	Identifier *id = lexer_p_->lookup_identifier ("default_midi");
	return id ? id->mididef () : new Midi_def ;
}

