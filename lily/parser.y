%{ // -*-Fundamental-*-
#include <iostream.h>


#include "script-def.hh"
#include "symtable.hh"
#include "lookup.hh"
#include "misc.hh"
#include "my-lily-lexer.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "main.hh"
#include "input-score.hh"
#include "input-staff.hh"
#include "input-music.hh"
#include "keyword.hh"
#include "debug.hh"
#include "parseconstruct.hh"
#include "dimen.hh"
#include "identifier.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "voice-element.hh"
#include "my-lily-parser.hh"
#include "text-def.hh"
#include "input-register.hh"

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
    Duration *duration;
    Identifier *id;    
    Input_register * iregs;
    Input_music *music;
    Input_score *score;
    Input_staff *staff;    
    Interval *interval;
    Lookup*lookup;
    Melodic_req * melreq;
    Midi_def* midi;
    Moment *moment;
    Music_general_chord *chord;
    Music_voice *mvoice;
    Note_req *notereq;
    Paper_def *paper;
    Real real;
    Request * request;
    Script_def * script;
    String *string;
    Symbol * symbol;
    Symtable * symtable;
    Symtables * symtables;
    Text_def * textdef;
    Voice *voice;    
    Voice_element *el;	
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
%token CONCAT

%token BAR
%token CADENZA
%token CLEF
%token CM_T
%token DURATIONCOMMAND
%token ABSDYNAMIC
%token END
%token GEOMETRIC
%token GROUPING
%token GROUP
%token INPUT_REGS
%token IN_T
%token LYRIC
%token KEY
%token MELODIC
%token MELODIC_REQUEST
%token METER
%token MIDI
%token MM_T
%token MULTIVOICE
%token NOTE
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
%token UNITSPACE
%token WIDTH

%token <i>	DOTS
%token <i>	INT
%token <id>	REGS_IDENTIFIER
%token <id>	IDENTIFIER
%token <id>	MELODIC_REQUEST_IDENTIFIER 
%token <id>	CHORD_IDENTIFIER
%token <id>	VOICE_IDENTIFIER
%token <id>	POST_REQUEST_IDENTIFIER
%token <id>	SCRIPT_IDENTIFIER
%token <id>	STAFF_IDENTIFIER
%token <id>	REAL_IDENTIFIER
%token <id>	SCORE_IDENTIFIER
%token <id>	REQUEST_IDENTIFIER
%token <real>	REAL 
%token <string>	DURATION RESTNAME
%token <string>	STRING
%token <i> 	POST_QUOTES 
%token <i> 	PRE_QUOTES


%type <box>	box
%type <c>	open_request_parens close_request_parens close_plet_parens
%type <chord>	music_chord music_chord_body  init_music_chord
%type <el>	voice_elt full_element lyrics_elt command_elt
%type <i>	int
%type <i>	script_dir
%type <id>	declaration
%type <duration>	explicit_duration notemode_duration
%type <interval>	dinterval
%type <intvec>	intastint_list
%type <lookup>	symtables symtables_body
%type <melreq>	melodic_request steno_melodic_req
%type <notereq>	steno_note_req
%type <melreqvec>	pitch_list 
%type <midi>	midi_block midi_body
%type <moment>	duration_length
%type <music>	init_music
%type <mvoice>	 transposed_music_voice init_lyrics_voice
%type <mvoice>	music_voice_body music_voice  init_music_voice 
%type <paper>	paper_block paper_body
%type <real>	dim real
%type <real>	unit
%type <request>	post_request pre_request command_req 
%type <request>	pure_post_request pure_post_request_choice
%type <request>	script_req textscript_req dynamic_req 
%type <score>	score_block score_body
%type <script>	script_definition script_body mudela_script
%type <staff>	staff_block staff_init staff_body
%type <string>	declarable_identifier
%type <string>	script_abbreviation
%type <id>	old_identifier
%type <symbol>	symboldef
%type <symtable>	symtable symtable_body
%type <textdef>	mudela_text
%type <iregs>	input_regs input_regs_body

%left PRIORITY

%expect 1	/* have to fix this. */

%%

mudela:	/* empty */
	| mudela score_block {
		add_score($2);		
	}
	| mudela add_declaration { }
	| mudela error
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
		$$ = new String($1->name); 
		THIS->here_input().warning("redeclaration of `" + *$$ + "'");
	}
	;


old_identifier:
	IDENTIFIER
	|	MELODIC_REQUEST_IDENTIFIER 
	|	CHORD_IDENTIFIER
	|	VOICE_IDENTIFIER
	|	POST_REQUEST_IDENTIFIER
	|	SCRIPT_IDENTIFIER
	|	STAFF_IDENTIFIER
	|	REAL_IDENTIFIER
	|	SCORE_IDENTIFIER
	|	REQUEST_IDENTIFIER
	|	REGS_IDENTIFIER
	;

declaration:
	declarable_identifier '=' score_block {
		$$ = new Score_id(*$1, $3, SCORE_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' staff_block  {
		$$ = new Staff_id(*$1, $3, STAFF_IDENTIFIER);
		delete $1; 
	}
	| declarable_identifier '=' init_music_voice {
		$$ = new M_voice_id(*$1, $3, VOICE_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' init_lyrics_voice {
		$$ = new M_voice_id(*$1, $3, VOICE_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' script_definition {
		$$ = new Script_id(*$1, $3, SCRIPT_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' init_music_chord  {
		$$ = new M_chord_id(*$1, $3, CHORD_IDENTIFIER);
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
	
	| declarable_identifier '=' pure_post_request {
		$$ = new Request_id(*$1, $3, POST_REQUEST_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' melodic_request {
		$$ = new Request_id(*$1, $3, MELODIC_REQUEST_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' input_regs	{
		$$ = new Input_regs_id(*$1, $3, REGS_IDENTIFIER);
		delete $1;
	}
	;


input_regs:
	INPUT_REGS
		{ THIS->remember_spot(); }
	'{' input_regs_body '}'
	{
		$$ = $4;
		$$->set_spot(THIS->pop_spot());
	}
	;

input_regs_body:
	REGS_IDENTIFIER 	{
		$$ = $1->iregs(true);
	}
	|STRING	{ 
		$$ = new Input_register;
		$$->name_str_ = *$1;
	}
	| input_regs_body input_regs {
		$$->add($2);
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
		$$ = new Input_score; 
	}
	| SCORE_IDENTIFIER {
		$$ = $1->score(true);
	}
	| score_body staff_block	{ $$->add($2); }
	| score_body paper_block		{ $$->set($2);	}
	| score_body midi_block		{ $$->set($2);	}
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
		$$ = THIS->default_paper();

	}
	| paper_body WIDTH dim		{ $$->linewidth = $3;}
	| paper_body OUTPUT STRING	{ $$->outfile = *$3;
		delete $3;
	}
	| paper_body symtables		{ $$->set($2); }
	| paper_body UNITSPACE dim	{ $$->whole_width = $3; }
	| paper_body GEOMETRIC REAL	{ $$->geometric_ = $3; }
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

midi_body: { 
		$$ = new Midi_def; 
	}
	| midi_body OUTPUT STRING	{ 
		$$->outfile_str_ = *$3; 
		delete $3; 
	}
	| midi_body TEMPO notemode_duration ':' int {
		$$->set_tempo( $3->length(), $5 );
	}
	| midi_body error {

	}
	;

/*
	STAFFs
*/
staff_block:
	STAFF 	{ THIS->remember_spot(); }
/*cont*/	'{' staff_body '}' 	{
		$$ = $4; 
		$$-> set_spot(THIS->pop_spot());
	}
	| { THIS->remember_spot(); }
/*cont*/	STAFF_IDENTIFIER	{ 
		$$ = $2->staff(true); 
		$$-> set_spot(THIS->pop_spot());
	}
	;



staff_init:
	REGS_IDENTIFIER {
		$$ = new Input_staff;
		$$->ireg_p_ = $1->iregs(true);
	}
	| input_regs	{
		$$ = new Input_staff;
		$$->ireg_p_ = $1;
	}
	;

staff_body:
	staff_init
	| staff_body init_music	{
		$$ = $1;
		$2->set_default_group( "staff_music" + String($$->music_.size()));
		$$->add($2);
	}
	| staff_body error {
	}
	;

/*
	let the lexer switch mode.
*/
init_music:
	init_music_voice	{ $$ = $1; }
	| init_music_chord	{ $$ = $1; }
	| init_lyrics_voice	{ $$ = $1; }
	| VOICE_IDENTIFIER	{ 
		$$ = $1->mvoice(true);
	}
	;

init_lyrics_voice:
	LYRIC { THIS->lexer_p_->push_lyric_state(); } 
	music_voice { $$ = $3; THIS->lexer_p_->pop_state(); }
	;

init_music_voice:
	MELODIC { THIS->lexer_p_->push_note_state(); } 
	/* cont*/ music_voice
		{ $$=$3; THIS->lexer_p_->pop_state(); }
	;
init_music_chord:
	{ THIS->lexer_p_->push_note_state(); } 
	/* cont*/ music_chord
		  { $$=$2; THIS->lexer_p_->pop_state(); }
	;
/*
	MUSIC
*/



transposed_music_voice:
	steno_melodic_req music_voice { 
		$$ = $2;
		$$->transpose(*$1);
		delete $1;
	}
	;

music_voice:  '{' music_voice_body '}'	{ $$ = $2; }
	| TRANSPOSE '{' transposed_music_voice '}' {
		$$ = $3;
	}
	;

music_voice_body:
	VOICE_IDENTIFIER {
		$$ = $1->mvoice(true);
	}
	| /* */ 	{
		$$ = new Music_voice;
	}
	| music_voice_body full_element {
		$$->add_elt($2);
	}
	| music_voice_body voice_command {
	}
	| music_voice_body music_chord	{
		$$->add($2);
	}
	| music_voice_body CONCAT music_voice	{
		$$->add($3);/* niet echt */
	}
	| music_voice_body error {
	}
	| music_voice_body '>' {
		THIS->fatal_error_i_ = 1;
		THIS->parser_error("Confused by errors: bailing out");
	}
	;

music_chord:  '<' music_chord_body '>'	{ $$ = $2; }
	;

music_chord_body:
	CHORD_IDENTIFIER {
		$$=$1->mchord(true);
	}
	| /* */	{
		$$ = new Voice_group_chord;
	}
	| MULTIVOICE {
		$$ = new Multi_voice_chord;
	}
	| music_chord_body music_voice {
		$$->add($2);
	}
	| music_chord_body full_element {
		$$ ->add_elt($2);
	}
	| music_chord_body '}' {
		THIS->fatal_error_i_ = 1;
		THIS->parser_error("Confused by errors: bailing out");
	}
	| music_chord_body error {
	}
	;

/*
	VOICE ELEMENTS
*/
full_element:	pre_requests voice_elt post_requests {
		THIS->add_requests($2);
		$$ = $2;
	}
 	| pre_requests lyrics_elt post_requests {
	 	THIS->add_requests($2);
 		$$ = $2;
        }
	| command_elt
	;	

command_elt:
/* empty */ 	{
		$$ = new Voice_element;
		$$-> set_spot( THIS->here_input());
	}
/* cont: */
	command_req	{
		$2-> set_spot( THIS->here_input());
		$$->add($2);

	}
	;

command_req:
	 '|'				{ 
		$$ = new Barcheck_req;
	}
	| BAR STRING 			{
		$$ = new Bar_req(*$2);
		delete $2;
	}
	| METER '{' int '/' int '}'	{
		Meter_change_req *m = new Meter_change_req;
		m->set($3,$5);
		// sorry hw, i need meter at output of track,
		// but don-t know where to get it... statics should go.
		// HW : default: 4/4, meterchange reqs may change it.
		
		Midi_def::num_i_s = $3;
		Midi_def::den_i_s = $5;
		$$ = m;
	}
	| SKIP '{' duration_length '}' {
		Skip_req * skip_p = new Skip_req;
		skip_p->duration_ = *$3;
		delete $3;
		$$ = skip_p;
	}
	| CADENZA '{' int '}'	{
		$$ = new Cadenza_req($3);
	}
	| PARTIAL '{' duration_length '}'	{
		$$ = new Partial_measure_req(*$3);
		delete $3;
	}
	| STEM '{' int '}'		{
		$$ = get_stemdir_req($3);
	}
	| CLEF STRING {
		$$ = new Clef_change_req(*$2);
		delete $2;
	}
	| KEY '{' pitch_list '}' 	{	
		Key_change_req *key_p= new Key_change_req;
		key_p->melodic_p_arr_ = *$3;
		$$ = key_p;
		delete $3;
	}
	| GROUPING '{' intastint_list '}' {
		$$ = get_grouping_req(*$3); delete $3;
	}
	| GROUP STRING		{
		$$ = new Group_change_req;
		$$ -> command()->groupchange()->newgroup_str_ = *$2;
		delete $2;
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
	| post_requests close_plet_parens INT '/' INT { 
		THIS->post_reqs.push( THIS->get_parens_request($2) ); 
		THIS->post_reqs.push( get_plet_request( $2, $3, $5 ) ); 
	}
	;

post_request:
	pure_post_request
	| POST_REQUEST_IDENTIFIER	{
		$$ = $1->request(false)->clone();
	}
	;

pure_post_request: 
	pure_post_request_choice	{
		$$ = $1;
		$$->set_spot( THIS->here_input());
	}
	;
pure_post_request_choice:
	close_request_parens	{ 
		$$ = THIS->get_parens_request($1); 
	}
	| script_req
	| textscript_req
	| dynamic_req
	;



/*
	URG!!
*/
steno_melodic_req:
	MELODIC_REQUEST_IDENTIFIER	{
		$$ = $1->request(false)->clone()->melodic();
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
	|SPANDYNAMIC '{' int int '}' {
		Span_dynamic_req * sp_p = new Span_dynamic_req;
		sp_p->spantype = $4;
		sp_p-> dynamic_dir_i_  = $3;
		$$ = sp_p;
	}
	;

close_plet_parens:
	']' {
		$$ = ']';
	}
	;

close_request_parens:
	'('	{ 
		$$='(';
	}
	| ']'	{ 
		$$ = ']';
	}
	;
  
open_request_parens:
	')'	{ 
		$$=')';
	}
	| '['	{
		$$='[';
	}
	;

script_definition:
	SCRIPT '{' script_body '}' 	{ $$ = $3; }
	;

script_body:
	STRING int int int 		{
		$$ = new Script_def(*$1,$2, $3,$4);
		delete $1;
	}	
	;

textscript_req:
	script_dir mudela_text		{ $$ = get_text_req($1,$2); }
	;

mudela_text:
	STRING			{ 
		$$ = new Text_def;
		$$->text_str_ = *$1; 
		delete $1;
		$$->style_str_ = THIS->textstyle_str_;
	}
	;

script_req:
	script_dir mudela_script	{ 
		$$ = get_script_req($1, $2);
	}
	;
script_abbreviation:
	'^'		{ $$ = get_scriptdef('^'); }
	| '+'		{ $$ = get_scriptdef('+'); }
	| '-'		{ $$ = get_scriptdef('-'); }
 	| '|'		{ $$ = get_scriptdef('|'); }
	| 'o'		{ $$ = get_scriptdef('o'); }
	| '>'		{ $$ = get_scriptdef('>'); }
	| DOTS 		{
		if ( $1 > 1 ) 
		    THIS->here_input().warning( "too many staccato dots"  );
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
	PLET	'{' INT '/' INT '}'		{
		THIS->default_duration_.set_plet($3,$5);
	}
	| DURATIONCOMMAND '{' STRING '}'	{
		THIS->set_duration_mode(*$3);
		delete $3;
	}
	| DURATIONCOMMAND '{' notemode_duration '}'	{
		THIS->default_duration_ = *$3;
		delete $3;
	}
	| OCTAVECOMMAND { THIS->default_octave_i_ = 2; }
/* cont */
	'{' steno_melodic_req '}'	{
		THIS->default_octave_i_ = $4->octave_i_;
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

notemode_duration:
	/* */		{ 
		$$ = new Duration(THIS->default_duration_);
	}
	| DOTS		{
		$$ = new Duration(THIS->default_duration_);
		$$->dots_i_ = $1;
	}
	| explicit_duration	{
		THIS->set_last_duration($1);
		$$ = $1;
	}
	;

explicit_duration:
	int		{
		$$ = new Duration;
		$$->type_i_ = $1;
	}
	| explicit_duration DOTS 	{
		$$->dots_i_ = $2;
	}
	| explicit_duration '*' int  {
		$$->plet_.iso_i_ *= $3; 
	}
	| explicit_duration '/' int {
		$$->plet_.type_i_ *= $3; 
	}
	;


voice_elt:
	steno_note_req notemode_duration 		{
		if (!THIS->lexer_p_->note_state_b())
			THIS->parser_error("have to be in Note mode for notes");
		$$ = THIS->get_note_element($1, $2);
	}
	| RESTNAME notemode_duration		{
		$$ = THIS->get_rest_element(*$1, $2);
		delete $1;
	}
	;

lyrics_elt:
	mudela_text notemode_duration 			{
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
	| pitch_list MELODIC_REQUEST_IDENTIFIER	{
		$$->push($2->request(false)->clone()->melodic());
	}
	;

int:
	real			{
		$$ = int($1);
		if ( distance($1,Real(int($$)) ) > 1e-8)
			yyerror( "integer expected" );
	}
	;

real:
	INT			{
		$$ = Real($1);
	}
	| REAL		{
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
		$$ = new Lookup(*$1->lookup(true));
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
		Box b;
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
    return new Paper_def(
	lexer_p_->lookup_identifier("default_table")->lookup(true));
}

