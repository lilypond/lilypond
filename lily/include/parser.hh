typedef union {
    Request * request;
    Real real;
    Identifier *id;    
   Voice *voice;    
    Voice_element *el;	
    String *string;
    const char *consstr;
    Paper_def *paper;
    Midi_def* midi;
    Input_music *music;
    Music_general_chord *chord;
    Music_voice *mvoice; 
    int i;
    char c;
    int ii[10];
	Moment *moment;

    Array<String> * strvec;
    Array<int> *intvec;
    Array<Melodic_req*> *melreqvec;
    Input_staff *staff;    
    Input_score *score;
    Symtables * symtables;
    Symtable * symtable;
    Symbol * symbol;
    Lookup*lookup;
    Interval *interval;
    Box *box;
    Notename_tab *notename_tab;
    Script_def * script;
    Text_def * textdef;
} YYSTYPE;
#define	VOICE	258
#define	STAFF	259
#define	SCORE	260
#define	TITLE	261
#define	BAR	262
#define	OUTPUT	263
#define	MULTIVOICE	264
#define	CM_T	265
#define	IN_T	266
#define	PT_T	267
#define	MM_T	268
#define	PAPER	269
#define	WIDTH	270
#define	METER	271
#define	UNITSPACE	272
#define	SKIP	273
#define	COMMANDS	274
#define	COMMAND	275
#define	GEOMETRIC	276
#define	START_T	277
#define	DURATIONCOMMAND	278
#define	OCTAVECOMMAND	279
#define	KEY	280
#define	CLEF	281
#define	TABLE	282
#define	VOICES	283
#define	STEM	284
#define	PARTIAL	285
#define	MUSIC	286
#define	GROUPING	287
#define	CADENZA	288
#define	END	289
#define	SYMBOLTABLES	290
#define	TEXID	291
#define	NOTENAMES	292
#define	SCRIPT	293
#define	TEXTSTYLE	294
#define	PLET	295
#define	GOTO	296
#define	MIDI	297
#define	TEMPO	298
#define	IDENTIFIER	299
#define	PITCHMOD	300
#define	DURATION	301
#define	RESTNAME	302
#define	NOTENAME	303
#define	REAL	304
#define	STRING	305
#define	DOTS	306
#define	INT	307


extern YYSTYPE yylval;
