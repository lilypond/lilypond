%{ // -*-Fundamental-*-

/*
  parser.y -- lily parser

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
           Jan Nieuwenhuizen <jan@digicash.com>
*/

#include <iostream.h>

// mmm
#define MUDELA_VERSION "0.1.1"

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
#include "input-translator.hh"
#include "score.hh"
#include "music-list.hh"

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
    Array<Melodic_req*> *melreqvec;/* should clean up naming */
    Array<String> * strvec;
    Array<int> *intvec;
    Box *box;
    Chord * chord;
    Duration *duration;
    Identifier *id;    
    Input_translator* itrans;
    Music *music;
    Music_list *musiclist;
    Score *score;
    Interval *interval;
    Lookup*lookup;
    Melodic_req * melreq;
    Midi_def* midi;
    Moment *moment;
    Note_req *notereq;
    Paper_def *paper;
    Real real;
    Request * request;
    General_script_def * script;
    String *string;
    Symbol * symbol;
    Symtable * symtable;
    Symtables * symtables;
    Text_def * textdef;
    Tempo_req *tempo;
    char c;
    const char *consstr;
    int i;
    int ii[10];
}
%{

int 
yylex(YYSTYPE *s,  void * v_l)
{
	My_lily_parser	 *pars_l = (My_lily_parser*) v_l;
	My_lily_lexer * lex_l = pars_l->lexer_p_;
	
	lex_l->lexval_l = (void*) s;
	return lex_l->yylex();
}


%}

%pure_parser

/* tokens which are not keywords */

%token ALIAS
%token BAR
%token CADENZA
%token CLEAR
%token CLEF
%token CONTAINS
%token CONSISTS
%token ACCEPTS
%token CM_T
%token DURATIONCOMMAND
%token ABSDYNAMIC
%token END
%token GROUPING
%token GROUP
%token REQUESTTRANSLATOR
%token HSHIFT
%token IN_T
%token ID
%token LYRIC
%token KEY
%token MELODIC
%token MIDI
%token MELODIC_REQUEST
%token METER
%token MM_T
%token MULTI
%token NOTENAMES
%token OCTAVECOMMAND
%token OUTPUT
%token PAPER
%token PARTIAL
%token PLET
%token PT_T
%token SCORE
%token SCRIPT
%token SKIP
%token SPANDYNAMIC
%token STAFF
%token START_T
%token STEM
%token SYMBOLTABLES
%token TABLE
%token TRANSPOSE
%token TEMPO
%token TEXID
%token TEXTSTYLE
%token TITLE
%token VERSION

/* escaped */
%token E_EXCLAMATION E_SMALLER E_BIGGER E_CHAR

%type <i>	dots
%token <i>	INT
%token <melreq>	NOTENAME_ID
%token <id>	IDENTIFIER
%token <id>	MELODIC_REQUEST_IDENTIFIER 
%token <id>	MUSIC_IDENTIFIER
%token <id>	VOICE_IDENTIFIER
%token <id>	POST_REQUEST_IDENTIFIER
%token <id>	SCRIPT_IDENTIFIER
%token <id>	COMMAND_IDENTIFIER
%token <id>	REAL_IDENTIFIER
%token <id>	INPUT_TRANS_IDENTIFIER
%token <id>	INT_IDENTIFIER
%token <id>	SCORE_IDENTIFIER
%token <id>	MIDI_IDENTIFIER
%token <id>	PAPER_IDENTIFIER
%token <id>	REQUEST_IDENTIFIER
%token <real>	REAL 
%token <string>	DURATION RESTNAME
%token <string>	STRING
%token <i> 	POST_QUOTES 
%token <i> 	PRE_QUOTES


%type <box>	box
%type <c>	open_request_parens close_request_parens
%type <c>	open_plet_parens close_plet_parens
%type <music>	simple_element music_elt full_element lyrics_elt command_elt
%type <i>	int
%type <i>	script_dir
%type <id>	declaration
%type <duration>	explicit_duration notemode_duration entered_notemode_duration
%type <interval>	dinterval
%type <intvec>	intastint_list
%type <lookup>	symtables symtables_body
%type <melreq>	melodic_request steno_melodic_req
%type <notereq>	steno_note_req
%type <melreqvec>	pitch_list 
%type <midi>	midi_block midi_body
%type <moment>	duration_length

%type <music>	Music transposed_music
%type <musiclist> Voice Voice_body 
%type <chord>	Chord Chord_body
%type <paper>	paper_block paper_body
%type <real>	dim real
%type <real>	unit
%type <request>	post_request pre_request command_req verbose_command_req abbrev_command_req
%type <request>	script_req  dynamic_req 
%type <score>	score_block score_body
%type <script>	script_definition script_body mudela_script gen_script_def
%type <textdef> text_def
%type <string>	declarable_identifier
%type <string>	script_abbreviation
%type <id>	old_identifier
%type <symbol>	symboldef
%type <symtable>	symtable symtable_body
%type <itrans>	input_translator_spec input_translator_spec_body
%type <tempo> 	tempo_request

%left PRIORITY

%%

mudela:	/* empty */
	| mudela score_block {
		add_score($2);		
	}
	| mudela add_declaration { }
	| mudela error
	| mudela check_version { } 
	| mudela add_notenames { }
	;

check_version:
	VERSION STRING ';'		{
		if ( String( *$2 ) != MUDELA_VERSION) {
			if (THIS->ignore_version_b_) {
				THIS->here_input().error("Incorrect mudela version");
			} else {
				THIS->fatal_error_i_ = 1;
				THIS->parser_error("Incorrect mudela version");
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
		THIS->clear_notenames();
	}
	| notenames_body STRING '=' melodic_request {
		THIS->add_notename(*$2, $4);
		delete $2;
	}
	;
/*
	DECLARATIONS
*/
add_declaration: declaration	{
		THIS->lexer_p_->add_identifier($1);
		$1->init_b_ = THIS->init_parse_b_;
		$1->set_spot(THIS->pop_spot());
	}
	;

declarable_identifier:
	STRING {
		THIS->remember_spot();
	    $$ = $1;
	}
	| old_identifier { 
		THIS->remember_spot();
		$$ = new String($1->name_str_); 
		THIS->here_input().warning("redeclaration of `" + *$$ + "'");
	}
	;


old_identifier:
	IDENTIFIER
	|	INPUT_TRANS_IDENTIFIER
	|	MELODIC_REQUEST_IDENTIFIER 
	|	POST_REQUEST_IDENTIFIER
	|	SCRIPT_IDENTIFIER
	|	REAL_IDENTIFIER
	|	SCORE_IDENTIFIER
	|	REQUEST_IDENTIFIER
	;

declaration:
	declarable_identifier '=' score_block {
		$$ = new Score_id(*$1, $3, SCORE_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' paper_block {
		$$ = new Paper_def_id(*$1, $3, PAPER_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' midi_block {
		$$ = new Midi_def_id(*$1, $3, MIDI_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' script_definition {
		$$ = new Script_id(*$1, $3, SCRIPT_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' Music  {
		$$ = new Music_id(*$1, $3, MUSIC_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' symtables {
		$$ = new Lookup_id(*$1, $3, IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' real	{
		$$ = new Real_id(*$1, new Real($3), REAL_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' int	{
		$$ = new Int_id(*$1, new int($3), INT_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' post_request {
		$$ = new Request_id(*$1, $3, POST_REQUEST_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' melodic_request {
		$$ = new Request_id(*$1, $3, MELODIC_REQUEST_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' input_translator_spec {
		$$ = new Input_translator_id ( *$1, $3, INPUT_TRANS_IDENTIFIER);
		delete $1;
	}
	;



input_translator_spec:
	REQUESTTRANSLATOR '{' input_translator_spec_body '}'
		{ $$ = $3; }
	;

input_translator_spec_body:
	INPUT_TRANS_IDENTIFIER	{
		$$ = $1->input_translator(true);
		$$-> set_spot( THIS->here_input() );
	}
	| STRING STRING	{ 
		$$ = new Input_translator; 
		$$->base_str_ = *$1;
		$$->type_str_ =*$2;
		$$->set_spot ( THIS->here_input() );
		delete $1;
		delete $2;
	}
	| input_translator_spec_body ID STRING ';' {
		$$-> default_id_str_ = *$3;
		delete $3;
	}
	| input_translator_spec_body ALIAS STRING ';' {
		$$-> alias_str_arr_.push(*$3);
		delete $3;
	}
	| input_translator_spec_body CONSISTS STRING ';'	{
		$$-> consists_str_arr_.push(*$3);
		delete $3;
	}
	| input_translator_spec_body CONTAINS input_translator_spec {
		$$->add($3);
	}
	;

/*
	SCORE
*/
score_block:
	SCORE { THIS->remember_spot(); }
	/*cont*/ '{' score_body '}' 	{
		$$ = $4;
		$$->set_spot(THIS->pop_spot());
		if (!$$->paper_p_ && ! $$->midi_p_)
			$$->paper_p_ = THIS->default_paper();

		/* handle error levels. */
		$$->errorlevel_i_ = THIS->error_level_i_;
		THIS->error_level_i_ = 0;
	}
	;

score_body:		{ 
		$$ = new Score; 
	}
	| SCORE_IDENTIFIER {
		$$ = $1->score(true);
	}
	| score_body Music	{
		$$->music_p_ = $2;
	}
	| score_body paper_block		{
		$$->paper_p_ = $2;	
	}
	| score_body midi_block		{ 
		$$->midi_p_ = $2;
	}
	| score_body error {

	}
	;

intastint_list:
	/* */	{ $$ =new Array<int>; }
	| intastint_list int '*' int	{
		$$->push($2); $$->push($4);
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
		$$ = THIS->default_paper(); // paper / video / engrave
	}
	| paper_body OUTPUT STRING ';'	{ $$->outfile_str_ = *$3;
		delete $3;
	}
	| paper_body symtables		{ $$->set($2); }
	| paper_body STRING '=' dim ';'		{ 
		$$->set_var(*$2, $4);
	}
	| paper_body STRING '=' real ';' {
		$$->set_var(*$2, $4);
	}
	| paper_body input_translator_spec	{
		$$->set( $2 );
	}
	| paper_body error {

	}
	;

/*
	MIDI
*/
midi_block:
	MIDI

	'{' midi_body '}' 	{ $$ = $3; }
	;

midi_body: /* empty */ 		{
		$$ = THIS->default_midi();
	}
	| midi_body OUTPUT STRING ';'	{ 
		$$->outfile_str_ = *$3; 
		delete $3; 
	}
	| midi_body tempo_request ';' {
		$$->set_tempo( $2->dur_.length(), $2->metronome_i_ );
		delete $2;
	}
	| midi_body input_translator_spec	{
		$$->set( $2 );
	}
	| midi_body error {

	}
	;

tempo_request:
	TEMPO entered_notemode_duration '=' int 	{
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
	}
	| Voice_body ID STRING STRING ';'	{
		$$ = new Voice;
		$$->type_str_ = *$3;	
		$$->id_str_ = *$4;
		delete $3;
		delete $4;
	}
	| Voice_body Music		{
		$$->add($2);
	}
	;

Music:
	full_element		{ $$ = $1; }
	| Voice		{ $$ = $1; }
	| Chord			{ $$ = $1; }
	| transposed_music	{ $$ = $1; }
	| MUSIC_IDENTIFIER 	{ $$ = $1->music(true); }
	| MELODIC 
		{ THIS->lexer_p_->push_note_state(); } 
	Music
		{ $$=$3; THIS->lexer_p_->pop_state(); }

	| LYRIC 
		{ THIS->lexer_p_->push_lyric_state(); } 
	Music
		{ $$ = $3; THIS->lexer_p_->pop_state(); }
	; 

Chord:
	'<' Chord_body '>'	{ $$  = $2; }
	;

Chord_body:
	/**/	{
		$$ = new Chord;
		$$-> multi_level_i_ = 1;
	}
	| Chord_body MULTI INT ';' {
		$$->multi_level_i_=$3;
	}
	| Chord_body ID STRING STRING ';'	{
		$$->type_str_ = *$3;	
		$$->id_str_ = *$4;
		delete $4; 
		delete $3;
	}
	| Chord_body Music {
		$$->add($2);
	}
	;

transposed_music:
	TRANSPOSE steno_melodic_req Music {
		$$ = $3;
		$$ -> transpose($2);
		delete $2;
	}
	;


/*
	VOICE ELEMENTS
*/
full_element:
	pre_requests simple_element post_requests	{
	 	THIS->add_requests((Chord*)$2);//ugh
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
		$$-> set_spot( THIS->here_input());
		$1-> set_spot( THIS->here_input());
		((Chord*)$$) ->add($1);//ugh

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
		$$ = $1->request(true);
	}
	;

verbose_command_req:
	BAR STRING 			{
		$$ = new Bar_req(*$2);
		delete $2;
	}
	| METER int '/' int 	{
		Meter_change_req *m = new Meter_change_req;
		m->set($2,$4);
		// sorry hw, i need meter at output of track,
		// but don-t know where to get it... statics should go.
		// HW : default: 4/4, meterchange reqs may change it.
		
		Midi_def::num_i_s = $2;
		Midi_def::den_i_s = $4;
		$$ = m;
	}
	| SKIP duration_length {
		Skip_req * skip_p = new Skip_req;
		skip_p->duration_.set_plet($2->numerator().as_long(), 
			$2->denominator().as_long());
		
		delete $2;
		$$ = skip_p;
	}
	| tempo_request {
		$$ = $1;
	}
	| CADENZA int	{
		$$ = new Cadenza_req($2);
	}
	| PARTIAL duration_length 	{
		$$ = new Partial_measure_req(*$2);
		delete $2;
	}
	| STEM int 	{
		$$ = get_stemdir_req($2);
	}
	| HSHIFT int	{
		$$ = get_hshift_req($2);
	}
	| CLEF STRING {
		$$ = new Clef_change_req(*$2);
		delete $2;
	}
	| KEY pitch_list 	{	
		Key_change_req *key_p= new Key_change_req;
		key_p->melodic_p_arr_ = *$2;
		$$ = key_p;
		delete $2;
	}
	| GROUPING intastint_list {
		$$ = get_grouping_req(*$2); delete $2;
	}
	;

post_requests:
	{
		assert(THIS->post_reqs.empty());
	}
	| post_requests post_request {
		$2->set_spot( THIS->here_input());
		THIS->post_reqs.push($2);
	}
	;


post_request:
	POST_REQUEST_IDENTIFIER	{
		$$ = (Request*)$1->request(true);
	}
	|close_request_parens	{ 
		$$ = THIS->get_parens_request($1); 
	}
	| script_req
	| dynamic_req
	;



/*
	URG!!
*/
steno_melodic_req:
	NOTENAME_ID	{
		$$ = $1->clone()->musical()->melodic();
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

dynamic_req:
	ABSDYNAMIC '{' int '}'	{
		Absolute_dynamic_req *ad_p = new Absolute_dynamic_req;
		ad_p ->loudness_ = (Dynamic_req::Loudness)$3;
		$$ =ad_p;
	}
	| SPANDYNAMIC '{' int int '}' {
		Span_dynamic_req * sp_p = new Span_dynamic_req;
		sp_p->spantype = $4;
		sp_p-> dynamic_dir_i_  = $3;
		$$ = sp_p;
	}
	;

close_plet_parens:
	']' INT '/' INT {
		$$ = ']';
		THIS->plet_.type_i_ = $4;
		THIS->plet_.iso_i_ = $2;
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
	| close_plet_parens {
		$$ = ']';
	}
	| E_SMALLER {
		$$ = '<';
	}
	| E_BIGGER {
		$$ = '>';
	}
	;

open_plet_parens:
	'[' INT '/' INT {
		$$ = '[';
		THIS->plet_.type_i_ = $4;
		THIS->plet_.iso_i_ = $2;
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
	| open_plet_parens {
	}
	;



script_definition:
	SCRIPT '{' script_body '}' 	{ $$ = $3; }
	;

script_body:
	STRING int int int int int		{
		Script_def *s = new Script_def;
		s->set_from_input(*$1,$2, $3,$4,$5, $6);
		$$  = s;
		delete $1;
	}	
	;

script_req:
	script_dir gen_script_def		{ 
		Musical_script_req *m = new Musical_script_req;
		$$ = m;	
		m-> scriptdef_p_ = $2;
		m-> set_spot ( THIS->here_input() );
		m-> dir_i_  = $1;
	}
	;

gen_script_def:
	text_def	{ $$ = $1; }
	| mudela_script	{ $$ = $1; 
		$$-> set_spot( THIS->here_input() );
	}
	;

text_def:
	STRING { 
		Text_def *t  = new Text_def;
		$$ = t;
		t->text_str_ = *$1; 
		delete $1;
		t->style_str_ = THIS->textstyle_str_;
		$$->set_spot( THIS->here_input() );
	}
	;

script_abbreviation:
	'^'		{ $$ = get_scriptdef('^'); }
	| '+'		{ $$ = get_scriptdef('+'); }
	| '-'		{ $$ = get_scriptdef('-'); }
 	| '|'		{ $$ = get_scriptdef('|'); }
	| 'o'		{ $$ = get_scriptdef('o'); }
	| '>'		{ $$ = get_scriptdef('>'); }
	| '.' 		{
		$$ = get_scriptdef('.');
	}
	;
	
mudela_script:
	SCRIPT_IDENTIFIER		{ $$ = $1->script(true); }
	| script_definition		{ $$ = $1; }
	| script_abbreviation		{ 
		$$ = THIS->lexer_p_->lookup_identifier(*$1)->script(true);
		delete $1;
	}
	;

script_dir:
	'_'	{ $$ = -1; }
	|'^'	{ $$ = 1; }
	|'-'	{ $$ = 0; }
	;

pre_requests:
	| pre_requests pre_request {
		THIS->pre_reqs.push($2);
		$2->set_spot( THIS->here_input());
	}
	;

pre_request: 
	open_request_parens	{ 
		$$ = THIS->get_parens_request($1); 
	}
	;

voice_command:
	PLET	 INT '/' INT {
		THIS->default_duration_.set_plet($2,$4);
	}
	| DURATIONCOMMAND STRING {
		THIS->set_duration_mode(*$2);
		delete $2;
	}
	| DURATIONCOMMAND entered_notemode_duration {
		THIS->set_default_duration($2);
		delete $2;
	}
	| OCTAVECOMMAND { 
		/*
			This is weird, but default_octave_i_
			is used in steno_note_req too

			c' -> default_octave_i_ == 1
		*/
		/* why can't we have \oct{0} iso \oct{c'}*/
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
		$$ = new Moment(0,1);
	}
	| duration_length explicit_duration		{	
		*$$ += $2->length();
	}
	;

dots:
	'.'		{ $$ = 1; }
	| dots '.'	{ $$ ++; }
	;

entered_notemode_duration:
	/* */		{ 
		$$ = new Duration(THIS->default_duration_);
	}
	| dots		{
		$$ = new Duration(THIS->default_duration_);		
		$$->dots_i_  = $1;
	}
	| explicit_duration	{
		THIS->set_last_duration($1);
		$$ = $1;
	}
	;

notemode_duration:
	entered_notemode_duration {
		$$ = $1;
		$$->plet_.type_i_ *= THIS->plet_.type_i_;
		$$->plet_.iso_i_ *= THIS->plet_.iso_i_;
	}
	;

explicit_duration:
	int		{
		$$ = new Duration;
		if ( !Duration::duration_type_b($1) )
			THIS->parser_error("Not a duration");
		else {
			$$->type_i_ = $1;
		     }
	}
	| explicit_duration '.' 	{
		$$->dots_i_ ++;
	}
	| explicit_duration '*' int  {
		$$->plet_.iso_i_ *= $3; 
	}
	| explicit_duration '/' int {
		$$->plet_.type_i_ *= $3; 
	}
	;


music_elt:
	steno_note_req notemode_duration 		{
		if (!THIS->lexer_p_->note_state_b())
			THIS->parser_error("have to be in Note mode for notes");
		$1->set_duration (*$2);
		$$ = THIS->get_note_element($1, $2);
	}
	| RESTNAME notemode_duration		{
		$$ = THIS->get_rest_element(*$1, $2);
		delete $1;
	}
	;

lyrics_elt:
	text_def notemode_duration 			{
	/* this sux! text-def should be feature of lyric-engraver. */
		if (!THIS->lexer_p_->lyric_state_b())
			THIS->parser_error("Have to be in Lyric mode for lyrics");
		$$ = THIS->get_word_element($1, $2);

	};

/*
	UTILITIES
 */
pitch_list:			{
		$$ = new Array<Melodic_req*>;
	}
	| pitch_list NOTENAME_ID	{
		$$->push($2->clone()->musical()->melodic());
	}
	;

int:
	INT			{
		$$ = $1;
	}
	| INT_IDENTIFIER	{
		$$ = * $1->intid(0);
	}
	;


real:
	REAL		{
		$$ = $1;
	}
	| REAL_IDENTIFIER		{
		$$ = * $1->real(0);		
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
		$$ = $1->lookup(true);
	}
	| symtables_body TEXID STRING 		{
		$$->texsetting = *$3;
		delete $3;
	}
	| symtables_body STRING '=' symtable		{
		$$->add(*$2, $4);
		delete $2;
	}
	;

symtable:
	TABLE '{' symtable_body '}' { $$ = $3; }
	;

symtable_body:
				{ $$ = new Symtable; }
	| symtable_body	STRING	symboldef {
		$$->add(*$2, *$3);
		delete $2;
		delete $3;
	}
	;

symboldef:
	STRING 	box		{
		$$ = new Symbol(*$1, *$2);
		delete $1;
		delete $2;
	}
	| STRING {
		Box b(Interval(0,0), Interval(0,0));
		$$ = new Symbol(*$1, b);
		delete $1;
	}
	;

box:
	dinterval dinterval 	{
		$$ = new Box(*$1, *$2);
		delete $1;
		delete $2;
	}
	;

dinterval: dim	dim		{
		$$ = new Interval($1, $2);	
	}
	;

%%

void 
My_lily_parser::set_yydebug(bool b )
{
#ifdef YYDEBUG
	yydebug = b;
#endif
}
void
My_lily_parser::do_yyparse()
{
	yyparse((void*)this);
}

Paper_def*
My_lily_parser::default_paper()
{
	Identifier *id = lexer_p_->lookup_identifier( "default_paper" );
	return id ? id->paperdef(true) : new Paper_def ;
}

Midi_def*
My_lily_parser::default_midi()
{
	Identifier *id = lexer_p_->lookup_identifier( "default_midi" );
	return id ? id->mididef(true) : new Midi_def ;
}

