%{ // -*-Fundamental-*-

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

// needed for bison.simple's malloc() and free()
#include <malloc.h>

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

%}


%union {
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
}
%{

int
yylex (YYSTYPE *s,  void * v_l)
{
	My_lily_parser	 *pars_l = (My_lily_parser*) v_l;
	My_lily_lexer * lex_l = pars_l->lexer_p_;

	lex_l->lexval_l = (void*) s;
	return lex_l->yylex ();
}


%}

%pure_parser

/* tokens which are not keywords */

%token ALIAS
%token BAR
%token BEAMPLET
%token MAEBTELP
%token BREAK
%token CADENZA
%token CLEAR
%token CLEF
%token CONTAINS
%token CONSISTS
%token ACCEPTS
%token CM_T
%token DURATION
%token ABSDYNAMIC
%token END
%token GROUPING
%token TRANSLATOR
%token HEADER
%token IN_T
%token LYRIC
%token KEY
%token MELODIC
%token MIDI
%token MELODIC_REQUEST
%token METER
%token MM_T
%token MULTI
%token NOTENAMES
%token OCTAVE
%token OUTPUT
%token PAPER
%token PARTIAL
%token PLET
%token TELP
%token PT_T
%token SCORE
%token SCRIPT
%token SHAPE
%token SKIP
%token SPANDYNAMIC
%token STAFF
%token START_T
%token SYMBOLTABLES
%token TABLE
%token TRANSPOSE
%token TEMPO
%token TYPE
%token TEXID
%token TEXTSTYLE
%token TITLE
%token PROPERTY
%token VERSION

/* escaped */
%token E_EXCLAMATION E_SMALLER E_BIGGER E_CHAR

%type <i>	dots
%token <i>	DIGIT
%token <melreq>	NOTENAME_ID
%token <id>	DURATION_IDENTIFIER
%token <id>	IDENTIFIER
%token <id>	MELODIC_REQUEST_IDENTIFIER
%token <id>	MUSIC_IDENTIFIER
%token <id>	VOICE_IDENTIFIER
%token <id>	POST_REQUEST_IDENTIFIER
%token <id>	SCRIPT_IDENTIFIER
%token <id>	COMMAND_IDENTIFIER
%token <id>	REAL_IDENTIFIER
%token <id>	TRANS_IDENTIFIER
%token <id>	INT_IDENTIFIER
%token <id>	SCORE_IDENTIFIER
%token <id>	MIDI_IDENTIFIER
%token <id>	PAPER_IDENTIFIER
%token <id>	REQUEST_IDENTIFIER
%token <real>	REAL
%token <string>	DURATION RESTNAME
%token <string>	STRING
%token <i>	UNSIGNED
%token <i> 	POST_QUOTES
%token <i> 	PRE_QUOTES

%type <outputdef> output_def
%type <header> 	mudela_header mudela_header_body
%type <box>	box
%type <i>	open_request_parens close_request_parens
%type <c>	open_abbrev_parens
%type <i>	open_plet_parens close_plet_parens
%type <music>	simple_element music_elt full_element lyrics_elt command_elt
%type <i>	abbrev_type
%type <i>	int unsigned
%type <i>	script_dir
%type <id>	identifier_init
%type <duration> explicit_steno_duration notemode_duration
%type <duration> entered_notemode_duration explicit_duration
%type <interval>	dinterval
%type <intvec>	intastint_list
%type <lookup>	symtables symtables_body
%type <melreq>	melodic_request steno_melodic_req
%type <notereq>	steno_note_req
%type <melreqvec>	pitch_list
%type <midi>	midi_block midi_body
%type <moment>	duration_length

%type <scalar>  scalar
%type <music>	Music transposed_music
%type <music>	property_def translator_change
%type <musiclist> Voice Voice_body
%type <chord>	Chord Chord_body
%type <paper>	paper_block paper_body
%type <real>	dim real
%type <real>	unit
%type <request> abbrev_command_req
%type <request>	post_request structured_post_request
%type <pair>	plet_fraction
%type <request> command_req verbose_command_req
%type <request>	script_req  dynamic_req
%type <score>	score_block score_body
%type <intarr>	shape_array
%type <script>	script_definition script_body mudela_script gen_script_def
%type <textdef> text_def finger
%type <string>	script_abbreviation
%type <symbol>	symboldef
%type <symtable>	symtable symtable_body
%type <trans>	translator_spec translator_spec_body
%type <tempo> 	tempo_request
%type <string>	concat_strings

%expect 1


%%

mudela:	/* empty */
	| mudela mudela_header {
		delete THIS->default_header_p_ ;
		THIS->default_header_p_ = $2;
	}
	| mudela score_block {
		add_score ($2);
	}
	| mudela add_declaration { }
	| mudela error
	| mudela check_version { }
	| mudela add_notenames { }
	;

check_version:
	VERSION STRING ';'		{
		if (String (*$2) != MUDELA_VERSION) {
			if (THIS->ignore_version_b_) {
				THIS->here_input ().error ("Incorrect mudela version");
			} else {
				THIS->fatal_error_i_ = 1;
				THIS->parser_error ("Incorrect mudela version");
			}
		}
	}
	;

add_notenames:
	NOTENAMES '{' notenames_body '}'
	;
notenames_body:
	/**/	{
	}
	| notenames_body CLEAR	{
		THIS->clear_notenames ();
	}
	| notenames_body STRING '=' melodic_request {
		THIS->add_notename (*$2, $4);
		delete $2;
	}
	;

mudela_header_body:
		{
		$$ = new Header;
	}
	| mudela_header_body STRING '=' concat_strings ';' {
		(*$$)[*$2] = *$4;
		delete $2;
		delete $4;
	}
	;

mudela_header:
	HEADER '{' mudela_header_body '}'	{
		$$ = $3;
	}
	;


concat_strings:
		{
		$$ = new String;
	}
	| concat_strings STRING	{
		*$$ += *$2;
	}


/*
	DECLARATIONS
*/

add_declaration:
	STRING {
		THIS->remember_spot ();
	}
	/* cont */ '=' identifier_init {
	    THIS->lexer_p_->set_identifier (*$1, $4);
	    $4->init_b_ = THIS->init_parse_b_;
	    $4->set_spot (THIS->pop_spot ());
	}
	;

identifier_init:
	score_block {
		$$ = new Score_id ($1, SCORE_IDENTIFIER);

	}
	| paper_block {
		$$ = new Paper_def_id ($1, PAPER_IDENTIFIER);

	}
	| midi_block {
		$$ = new Midi_def_id ($1, MIDI_IDENTIFIER);

	}
	| script_definition {
		$$ = new Script_id ($1, SCRIPT_IDENTIFIER);

	}
	| Music  {
		$$ = new Music_id ($1, MUSIC_IDENTIFIER);

	}
	| symtables {
		$$ = new Lookup_id ($1, IDENTIFIER);

	}
	| real	{
		$$ = new Real_id (new Real ($1), REAL_IDENTIFIER);

	}
	| int	{
		$$ = new Int_id (new int ($1), INT_IDENTIFIER);
	}
	| post_request {
		$$ = new Request_id ($1, POST_REQUEST_IDENTIFIER);
	}
	| melodic_request {
		$$ = new Request_id ($1, MELODIC_REQUEST_IDENTIFIER);

	}
	| translator_spec {
		$$ = new Translator_id ($1, TRANS_IDENTIFIER);
	}
	| explicit_duration {
		$$ = new Duration_id ($1, DURATION_IDENTIFIER);
	}
	;



translator_spec:
	TRANSLATOR '{' translator_spec_body '}'
		{ $$ = $3; }
	;

translator_spec_body:
	TRANS_IDENTIFIER	{
		$$ = $1->translator ();
		$$-> set_spot (THIS->here_input ());
	}
	| TYPE STRING ';'	{
		$$ = get_translator_l (*$2)->clone ();
		$$->set_spot (THIS->here_input ());
		delete $2;
	}
	| translator_spec_body STRING '=' scalar ';'	{
		$$-> set_property (*$2, *$4);
		delete $2;
		delete $4;
	}
	| translator_spec_body CONSISTS STRING ';'	{
		$$->group_l ()->consists_str_arr_.push (*$3);
		delete $3;
	}
	| translator_spec_body ACCEPTS STRING ';' {
		$$->group_l ()->accepts_str_arr_.push (*$3);
		delete $3;
	}
	;

/*
	SCORE
*/
score_block:
	SCORE { THIS->remember_spot ();
		THIS->error_level_i_ =0;
	}
	/*cont*/ '{' score_body '}' 	{
		$$ = $4;
		$$->set_spot (THIS->pop_spot ());
		if (!$$->def_p_arr_.size ())
			$$->add (THIS->default_paper ());

		/* handle error levels. */
		$$->errorlevel_i_ = THIS->error_level_i_;
		THIS->error_level_i_ = 0;
		if (!$$->header_p_ && THIS->default_header_p_)
			$$->header_p_ = new Header (*THIS->default_header_p_);
	}
	;

score_body:		{
		$$ = new Score;
	}
	| SCORE_IDENTIFIER {
		$$ = $1->score ();
	}
	| score_body mudela_header 	{
		$$->header_p_ = $2;
	}
	| score_body Music	{
		if ($$->music_p_)
			$2->warning ("More than one music block");	
		$$->music_p_ = $2;
	}
	| score_body output_def {
		$$->add ($2);
	}
	| score_body error {

	}
	;

output_def:
	paper_block {
		$$ = $1;
	}
	|  midi_block		{
		$$= $1;
	}
	;

intastint_list:
	/* */	{ $$ =new Array<int>; }
	| intastint_list int '*' int	{
		$$->push ($2); $$->push ($4);
	}
	;


/*
	PAPER
*/
paper_block:
	PAPER
	'{' paper_body '}' 	{ $$ = $3; }
	;

paper_body:
	/* empty */		 	{
		$$ = THIS->default_paper ();
	}
	| PAPER_IDENTIFIER	{
		$$ = $1->paperdef ();
	}
	| paper_body OUTPUT STRING ';'	{ 
		$$->outfile_str_ = *$3;
		delete $3;
	}
	| paper_body symtables		{ $$->set ($2); }
	| paper_body STRING '=' dim ';'		{
		$$->set_var (*$2, $4);
	}
	| paper_body STRING '=' real ';' {
		$$->set_var (*$2, $4);
	}
	| paper_body STRING '=' translator_spec	{
		$$-> assign_translator (*$2, $4);
		delete $2;
	}
	| paper_body SHAPE '=' shape_array ';' {
		$$->shape_int_a_ = *$4;
		delete $4;
	}
	| paper_body error {

	}
	;

shape_array:
	/* empty */ {
		$$ = new Array<Interval>;
	}
	| shape_array dim dim {
		$$->push(Interval($2, $2 + $3));
	};

/*
	MIDI
*/
midi_block:
	MIDI

	'{' midi_body '}' 	{ $$ = $3; }
	;

midi_body: /* empty */ 		{
		$$ = THIS->default_midi ();
	}
	| midi_body STRING '=' translator_spec	{
		$$-> assign_translator (*$2, $4);
		delete $2;
	}
	| midi_body OUTPUT STRING ';'	{
		$$->outfile_str_ = *$3;
		delete $3;
	}
	| midi_body tempo_request ';' {
		$$->set_tempo ($2->dur_.length (), $2->metronome_i_);
		delete $2;
	}
	| midi_body error {

	}
	;

tempo_request:
	TEMPO entered_notemode_duration '=' unsigned	{
		$$ = new Tempo_req;
		$$->dur_ = *$2;
		delete $2;
		$$-> metronome_i_ = $4;
	}
	;

/*
	MUSIC
*/

Voice:
	'{' Voice_body '}'	{
		$$ = $2;
	}
	;

Voice_body:
	/**/		{
		$$ = new Voice;
		$$->set_spot (THIS->here_input ());
	}
	| Voice_body Music		{
		$$->add ($2);
	}
	;

Music:
	full_element		{ $$ = $1; }
	| TYPE STRING Music	{
		$$ = $3;
		$$->translator_type_str_ = *$2;
		delete $2;
	}
	| TYPE STRING '=' STRING Music {
		$$ = $5;
		$$->translator_type_str_ = *$2;
		$$->translator_id_str_ = *$4;
		delete $2;
		delete $4;
	}
	| Voice		{ $$ = $1; }
	| Chord			{ $$ = $1; }
	| transposed_music	{ $$ = $1; }
	| MUSIC_IDENTIFIER 	{ $$ = $1->music (); }
	| MUSIC_IDENTIFIER ';'	{ $$ = $1->music (); }
	| MELODIC
		{ THIS->lexer_p_->push_note_state (); }
	Music
		{ $$=$3; THIS->lexer_p_->pop_state (); }

	| LYRIC
		{ THIS->lexer_p_->push_lyric_state (); }
	Music
		{ $$ = $3; THIS->lexer_p_->pop_state (); }
	| property_def
	| translator_change
	;

translator_change:
	TRANSLATOR STRING '=' STRING  {
		Change_translator * t = new Change_translator;
		t-> change_to_type_str_ = *$2;
		t-> change_to_id_str_ = *$4;

		$$ = t;
		$$->set_spot (THIS->here_input ());
		delete $2;
		delete $4;
	}
	;

property_def:
	PROPERTY STRING '.' STRING '=' scalar	{
		Translation_property *t = new Translation_property;
		t-> translator_type_str_ = *$2;
		t-> var_str_ = *$4;
		t-> value_ = *$6;
		$$ = t;
		$$->set_spot (THIS->here_input ());
		delete $2;
		delete $4;
		delete $6;
	}
	;

scalar:
	STRING		{ $$ = new Scalar (*$1); delete $1; }
	| int		{ $$ = new Scalar ($1); }
	;


Chord:
	'<' Chord_body '>'	{ $$  = $2; }
	| MULTI unsigned Chord {
		$$ = $3;
		$$->multi_level_i_=$2;
	}
	;

Chord_body:
	/**/	{
		$$ = new Chord;
		$$-> multi_level_i_ = 1;
		$$->set_spot (THIS->here_input ());
	}
	| Chord_body Music {
		$$->add ($2);
	}
	;

transposed_music:
	TRANSPOSE steno_melodic_req Music {
		$$ = $3;
		$$ -> transpose ($2);

		delete $2;
	}
	;


/*
	VOICE ELEMENTS
*/
full_element:
	pre_requests simple_element post_requests	{
	 	THIS->add_requests ((Chord*)$2);//ugh
 		$$ = $2;
	}
	| command_elt
	| voice_command ';'	{ $$ = 0; }
	;

simple_element:
	music_elt
 	| lyrics_elt
	;

command_elt:
	command_req {
		$$ = new Request_chord;
		$$-> set_spot (THIS->here_input ());
		$1-> set_spot (THIS->here_input ());
		((Chord*)$$) ->add ($1);//ugh

	}
	;

command_req:
	abbrev_command_req
	| verbose_command_req ';'	{ $$ = $1; }
	;

abbrev_command_req:
	'|'				{
		$$ = new Barcheck_req;
	}
	| COMMAND_IDENTIFIER	{
		$$ = $1->request ();
	}
	;

verbose_command_req:
	BAR STRING 			{
		$$ = new Bar_req (*$2);
		delete $2;
	}
	| BREAK	{
		Break_force_req * f = new Break_force_req;
		f-> set_spot (THIS->here_input ());
		$$ = f;
	}
	| METER unsigned '/' unsigned 	{
		Meter_change_req *m = new Meter_change_req;
		m->set ($2,$4);
		$$ = m;
	}
	| SKIP duration_length {
		Skip_req * skip_p = new Skip_req;
		skip_p->duration_.set_plet ($2->num (),
			$2->den ());

		delete $2;
		$$ = skip_p;
	}
	| tempo_request {
		$$ = $1;
	}
	| CADENZA unsigned	{
		$$ = new Cadenza_req ($2);
	}
	| PARTIAL duration_length 	{
		$$ = new Partial_measure_req (*$2);
		delete $2;
	}
	| CLEF STRING {
		$$ = new Clef_change_req (*$2);
		delete $2;
	}
	| KEY pitch_list 	{
		Key_change_req *key_p= new Key_change_req;
		key_p->melodic_p_arr_ = *$2;
		$$ = key_p;
		delete $2;
	}
	| GROUPING intastint_list {
		$$ = get_grouping_req (*$2); delete $2;
	}
	;

post_requests:
	{
		/* something silly happened.  Junk this stuff*/
		if (!THIS->post_reqs.empty ())
		{
			warning ("Junking post-requests");
			THIS->post_reqs.clear ();
		}
	}
	| post_requests structured_post_request {
		$2->set_spot (THIS->here_input ());
		THIS->post_reqs.push ($2);
	}
	| post_requests close_request_parens	{
		Array<Request*>& r = *THIS->get_parens_request ($2);
		for (int i = 0; i < r.size (); i++ )
			r[i]->set_spot (THIS->here_input ());
		THIS->post_reqs.concat (r);
		delete &r;
	}
	;

structured_post_request:
	script_req
	| post_request
	;

post_request:
	POST_REQUEST_IDENTIFIER	{
		$$ = (Request*)$1->request ();
	}
	| dynamic_req {
		$$ = $1;
	}
	| abbrev_type	{
		Abbreviation_req* a = new Abbreviation_req;
		a->type_i_ = $1;
		$$ = a;
	}
	;



/*
	URG!!
*/
steno_melodic_req:
	NOTENAME_ID	{
		$$ = $1->clone ()->musical ()->melodic ();
		$$->octave_i_ += THIS->default_octave_i_;
	}
	| steno_melodic_req POST_QUOTES 	{
		$$-> octave_i_ += $2;
	}
	| PRE_QUOTES steno_melodic_req	 {
		$$ = $2;
		$2-> octave_i_ -= $1;
	}
	;

steno_note_req:
	steno_melodic_req	{
		$$ = new Note_req;
		* (Melodic_req *) $$ = *$1;
		delete $1;
	}
	| steno_note_req   '!' 		{
		$$->forceacc_b_ = ! $$->forceacc_b_;
	}
	/* have to duration here. */
	;

melodic_request:
	MELODIC_REQUEST '{' int int int '}'	{/* ugh */
		$$ = new Melodic_req;
		$$->octave_i_ = $3;
		$$->notename_i_ = $4;
		$$->accidental_i_ = $5;
	}
	;

explicit_duration:
	DURATION '{' int unsigned '}'	{
		$$ = new Duration;
		$$-> durlog_i_ = $3;
		$$-> dots_i_ = $4;
	}
	;

dynamic_req:
	ABSDYNAMIC '{' unsigned '}'	{
		Absolute_dynamic_req *ad_p = new Absolute_dynamic_req;
		ad_p ->loudness_ = (Dynamic_req::Loudness)$3;
		$$ =ad_p;
	}
	| SPANDYNAMIC '{' int int '}' {
		Span_dynamic_req * sp_p = new Span_dynamic_req;
		sp_p->spantype = (Span_req::Spantype)$4;
		sp_p-> dynamic_dir_  = (Direction)$3;
		$$ = sp_p;
	}
	;

plet_fraction:
	unsigned '/' unsigned {
		$$[0] = $1;
		$$[1] = $3;
	}
	|
	'/' unsigned {
		int num = $2 >? 1;
		$$[0] = guess_plet_a[(num <? GUESS_PLET) - 1];
		$$[1] = num;
	}
	;

close_plet_parens:
	']' plet_fraction {
		$$ = MAEBTELP;
		THIS->plet_.type_i_ = $2[1];
		THIS->plet_.iso_i_ = $2[0];
		THIS->default_duration_.plet_.type_i_ = $2[1];
		THIS->default_duration_.plet_.iso_i_ = $2[0];
	}
	| TELP {
		$$ = TELP;
		THIS->plet_.type_i_ = 1;
		THIS->plet_.iso_i_ = 1;
		THIS->default_duration_.plet_.iso_i_ = 1;
		THIS->default_duration_.plet_.type_i_ = 1;
	}
	| TELP plet_fraction {
		$$ = TELP;
		THIS->plet_.type_i_ = $2[1];
		THIS->plet_.iso_i_ = $2[0];
		THIS->default_duration_.plet_.type_i_ = $2[1];
		THIS->default_duration_.plet_.iso_i_ = $2[0];
	}
	;

close_request_parens:
	'~'	{
		$$ = '~';
	}
	| '('	{
		$$='(';
	}
	| ']'	{
		$$ = ']';
	}
	| E_SMALLER {
		$$ = '<';
	}
	| E_BIGGER {
		$$ = '>';
	}
	| close_plet_parens {
	}
	;

open_abbrev_parens:
	'[' ':' unsigned {
		$$ = '[';
		if (!Duration::duration_type_b ($3))
			THIS->parser_error ("1:Not a duration");
		else if ($3 < 8)
			THIS->parser_error ("Can't abbreviate");
		else
			THIS->set_abbrev_beam ($3);
	}
	;

open_plet_parens:
	'[' plet_fraction {
		$$ = BEAMPLET;
		THIS->plet_.type_i_ = $2[1];
		THIS->plet_.iso_i_ = $2[0];
		THIS->default_duration_.plet_.type_i_ = $2[1];
		THIS->default_duration_.plet_.iso_i_ = $2[0];
	}
	| PLET plet_fraction {
		$$ = PLET;
		THIS->plet_.type_i_ = $2[1];
		THIS->plet_.iso_i_ = $2[0];
		THIS->default_duration_.plet_.type_i_ = $2[1];
		THIS->default_duration_.plet_.iso_i_ = $2[0];
	}
	;

open_request_parens:
	E_EXCLAMATION 	{
		$$ = '!';
	}
	| ')'	{
		$$=')';
	}
	| '['	{
		$$='[';
	}
	| open_abbrev_parens {
	}
	| open_plet_parens {
	}
	;



script_definition:
	SCRIPT '{' script_body '}' 	{ $$ = $3; }
	;

script_body:
	STRING int int int int int		{
		Script_def *s = new Script_def;
		s->set_from_input (*$1,$2, $3,$4,$5, $6);
		$$  = s;
		delete $1;
	}
	;

script_req:
	script_dir gen_script_def	{
		Musical_script_req *m = new Musical_script_req;
		$$ = m;
		m->scriptdef_p_ = $2;
		m->set_spot (THIS->here_input ());
		if (!m->dir_)
		  m->dir_  = (Direction)$1;
	}
	;

gen_script_def:
	text_def	{ 
		$$ = $1;
		((Text_def*) $$)->align_i_ = CENTER; /* UGH */
	}
	| mudela_script	{ 
		$$ = $1;
		$$-> set_spot (THIS->here_input ());
	}
	| finger {
		$$ = $1;
		((Text_def*)$$)->align_i_ = CENTER;
	}
	;

text_def:
	STRING {
		Text_def *t  = new Text_def;
		$$ = t;
		t->text_str_ = *$1;
		delete $1;
		t->style_str_ = THIS->textstyle_str_;
		$$->set_spot (THIS->here_input ());
	}
	;

finger:
	 DIGIT {
		Text_def* t  = new Text_def;
		$$ = t;
		t->text_str_ = String ($1);
		t->style_str_ = "finger";
		$$->set_spot (THIS->here_input ());
	}
	;

script_abbreviation:
	'^'		{ $$ = get_scriptdef ('^'); }
	| '+'		{ $$ = get_scriptdef ('+'); }
	| '-'		{ $$ = get_scriptdef ('-'); }
 	| '|'		{ $$ = get_scriptdef ('|'); }
	| 'o'		{ $$ = get_scriptdef ('o'); }
	| '>'		{ $$ = get_scriptdef ('>'); }
	| '.' 		{
		$$ = get_scriptdef ('.');
	}
	;

mudela_script:
	SCRIPT_IDENTIFIER		{ $$ = $1->script (); }
	| script_definition		{ $$ = $1; }
	| script_abbreviation		{
		$$ = THIS->lexer_p_->lookup_identifier (*$1)->script ();
		delete $1;
	}
	;

script_dir:
	'_'	{ $$ = -1; }
	| '^'	{ $$ = 1; }
	| '-'	{ $$ = 0; }
	;

pre_requests:
	{
	}
	| pre_requests open_request_parens {
		Array<Request*>& r = *THIS->get_parens_request ($2);
		for (int i = 0; i < r.size (); i++ )
			r[i]->set_spot (THIS->here_input ());
		THIS->pre_reqs.concat (r);
		delete &r;
	}
	;

voice_command:
	DURATION STRING {
		THIS->set_duration_mode (*$2);
		delete $2;
	}
	| DURATION entered_notemode_duration {
		THIS->set_default_duration ($2);
		delete $2;
	}
	| OCTAVE {
		/*
			This is weird, but default_octave_i_
			is used in steno_note_req too

			c' -> default_octave_i_ == 1
		*/
		/* why can't we have \oct 0 iso \oct{c'}*/
		THIS->default_octave_i_ = 1; }
/* cont */
	steno_melodic_req {
		THIS->default_octave_i_ = $3->octave_i_;
		delete $3;
	}
	| TEXTSTYLE STRING 	{
		THIS->textstyle_str_ = *$2;
		delete $2;
	}
	;

duration_length:
	{
		$$ = new Moment (0,1);
	}
	| duration_length explicit_steno_duration		{
		*$$ += $2->length ();
	}
	;

dots:
	'.'		{ $$ = 1; }
	| dots '.'	{ $$ ++; }
	;

entered_notemode_duration:
	/* */		{
		$$ = new Duration (THIS->default_duration_);
	}
	| dots		{
		$$ = new Duration (THIS->default_duration_);
		$$->dots_i_  = $1;
	}
	| explicit_steno_duration	{
		THIS->set_last_duration ($1);
		$$ = $1;
	}
	;

notemode_duration:
	entered_notemode_duration {
		$$ = $1;
	}
	;

explicit_steno_duration:
	unsigned		{
		$$ = new Duration;
		if (!Duration::duration_type_b ($1))
			THIS->parser_error ("2:Not a duration");
		else {
			$$->durlog_i_ = Duration_convert::i2_type ($1);
			$$->set_plet (THIS->default_duration_);
		     }
	}
	| DURATION_IDENTIFIER	{
		$$ = $1->duration ();
	}
	| explicit_steno_duration '.' 	{
		$$->dots_i_ ++;
	}
	| explicit_steno_duration '*' unsigned  {
		$$->plet_.iso_i_ = $3;
	}
	| explicit_steno_duration '/' unsigned {
		$$->plet_.type_i_ = $3;
	}
	;


abbrev_type: 
	':'	{
		$$ =0;
	}
	| ':' unsigned {
		if (!Duration::duration_type_b ($2))
			THIS->parser_error ("3:Not a duration");
		else if ($2 < 8)
			THIS->parser_error ("Can't abbreviate");
		$$ = $2;
	}

	;

music_elt:
	steno_note_req notemode_duration  {
		if (!THIS->lexer_p_->note_state_b ())
			THIS->parser_error ("have to be in Note mode for notes");
		$1->set_duration (*$2);
		int durlog_i = $2->durlog_i_;
		$$ = THIS->get_note_element ($1, $2);
	}
	| RESTNAME notemode_duration		{
		$$ = THIS->get_rest_element (*$1, $2);
		delete $1;
	}
	;

lyrics_elt:
	text_def notemode_duration 			{
	/* this sux! text-def should be feature of lyric-engraver. */
		if (!THIS->lexer_p_->lyric_state_b ())
			THIS->parser_error ("Have to be in Lyric mode for lyrics");
		$$ = THIS->get_word_element ($1, $2);

	};

/*
	UTILITIES
 */
pitch_list:			{
		$$ = new Array<Melodic_req*>;
	}
	| pitch_list NOTENAME_ID	{
		$$->push ($2->clone ()->musical ()->melodic ());
	}
	;

unsigned:
	UNSIGNED	{
		$$ = $1;
	}
	| DIGIT {
		$$ = $1;
	};

int:
	unsigned {
		$$ = $1;
	}
	| '-' unsigned {
		$$ = -$2;
	}
	| INT_IDENTIFIER	{
		int *i_p = $1->intid ();
		$$ = *i_p;
		delete i_p;
	}
	;

real:
	REAL		{
		$$ = $1;
	}
	| REAL_IDENTIFIER		{
		Real *r_p = $1->real ();
		$$ = * r_p;
		delete r_p;
	}
	;



dim:
	real unit	{ $$ = $1*$2; }
	;


unit:	CM_T		{ $$ = 1 CM; }
	|IN_T		{ $$ = 1 INCH; }
	|MM_T		{ $$ = 1 MM; }
	|PT_T		{ $$ = 1 PT; }
	;

/*
	symbol tables
*/
symtables:
	SYMBOLTABLES '{' symtables_body '}'	{ $$ = $3; }
	;

symtables_body:
	 		{
		$$ = new Lookup;
	}
	| IDENTIFIER		{
		$$ = $1->lookup ();
	}
	| symtables_body TEXID STRING 		{
		$$->texsetting = *$3;
		delete $3;
	}
	| symtables_body STRING '=' symtable		{
		$$->add (*$2, $4);
		delete $2;
	}
	;

symtable:
	TABLE '{' symtable_body '}' { $$ = $3; }
	;

symtable_body:
				{ $$ = new Symtable; }
	| symtable_body	STRING	symboldef {
		$$->add (*$2, *$3);
		delete $2;
		delete $3;
	}
	;

symboldef:
	STRING 	box		{
		$$ = new Atom (*$1, *$2);
		delete $1;
		delete $2;
	}
	| STRING {
		Box b (Interval (0,0), Interval (0,0));
		$$ = new Atom (*$1, b);
		delete $1;
	}
	;

box:
	dinterval dinterval 	{
		$$ = new Box (*$1, *$2);
		delete $1;
		delete $2;
	}
	;

dinterval: dim	dim		{
		$$ = new Interval ($1, $2);
	}
	;

%%

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

