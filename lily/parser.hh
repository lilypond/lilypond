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

