%{

#include "mi2mu-proto.hh"
#include "proto.hh"
#include "plist.hh"
#include "warn.hh"
#include "mi2mu-global.hh"
//#include "midi-parser.hh"
#include "my-midi-parser.hh"
#include "my-midi-lexer.hh"
#include "duration-convert.hh"
#include "string-convert.hh"
#include "mudela-item.hh"
#include "mudela-score.hh"
#include "mudela-staff.hh"

#ifndef NDEBUG
#define YYDEBUG 1
#endif

%}

%union {
    Byte byte;
    char c;
    int i;
    String* str_p;
    Mudela_item* mudela_item_p;	// Voice_element* ? jup, just about :-)
    Mudela_score* mudela_score_p; // Input_score* ?
    Mudela_staff* mudela_staff_p; // Input_music* ?
}

%token HEADER TRACK
%token SYSEX_EVENT1 SYSEX_EVENT2
%token META_EVENT
%token SEQUENCE
%token END_OF_TRACK TEMPO SMPTE_OFFSET TIME KEY SSME

%token<i> I8 U8 INT16 INT32 INT7_8UNSET INT7_8SET VARINT
%token<i> RUNNING_STATUS DATA_ENTRY ALL_NOTES_OFF
%token<i> NOTE_OFF NOTE_ON 
%token<i> POLYPHONIC_AFTERTOUCH CONTROLMODE_CHANGE PROGRAM_CHANGE 
%token<i> CHANNEL_AFTERTOUCH PITCHWHEEL_RANGE
%token<i> YYTEXT YYCOPYRIGHT YYTRACK_NAME YYINSTRUMENT_NAME YYLYRIC YYMARKER YYCUE_POINT
%token<str_p> DATA

%type <i> varint
%type <mudela_score_p> header mudela_score
%type <mudela_staff_p> track
%type <mudela_item_p> item
%type <mudela_item_p> the_item meta_item the_meta_item text_item mudela_item sysex_item
%type <mudela_item_p> running_status data_entry all_notes_off
%type <mudela_item_p> note_off note_on
%type <mudela_item_p> polyphonic_aftertouch controlmode_change program_change
%type <mudela_item_p> channel_aftertouch pitchwheel_range

%%

midi:	/* empty */
	| midi mudela_score {
		midi_parser_l_g->add_score ($2);		
	}
	;

mudela_score:
	header {
	}
	| mudela_score track {
		$$->add_staff ($2);
		// ugh
		$2->set_tempo (midi_parser_l_g->mudela_tempo_p_->useconds_per_4_i());
		$2->set_meter (midi_parser_l_g->mudela_meter_p_->num_i(), 
			midi_parser_l_g->mudela_meter_p_->den_i(), 
			midi_parser_l_g->mudela_meter_p_->clocks_1_i(), 
			8);
		if  (midi_parser_l_g->copyright_str_.length_i())
			$2->copyright_str_ = midi_parser_l_g->copyright_str_;
		if  (midi_parser_l_g->track_name_str_.length_i())
			$2->name_str_ = midi_parser_l_g->track_name_str_;
		if  (midi_parser_l_g->instrument_str_.length_i())
			$2->instrument_str_ = midi_parser_l_g->instrument_str_;
		midi_parser_l_g->reset();
	}
	;

header:	
	HEADER INT32 INT16 INT16 INT16 {
		// ugh, already constructed; 
		// need to have score in My_midi_parser...
//		$$ = new Mudela_score ($3, $4, $5);
		$$ = midi_parser_l_g->mudela_score_p_;
		$$->format_i_ = $3;
		$$->tracks_i_ = $4;
		$$->tempo_i_ =  $5;
		midi_parser_l_g->set_division_4 ($5);
	}
	;

track: 
	TRACK INT32 {
		LOGOUT (NORMAL_ver) << "\ntrack " << midi_parser_l_g->track_i_ << ": " << flush;
		$$ = new Mudela_staff (midi_parser_l_g->track_i_++,
			// silly, cause not set yet!
			midi_parser_l_g->copyright_str_,
			midi_parser_l_g->track_name_str_,
			midi_parser_l_g->instrument_str_);
		//ugh, need to know now!
		midi_parser_l_g->mudela_staff_l_ = $$;
	}
	| track item {
		if  ($2) {
			$2->mudela_column_l_ = midi_parser_l_g->mudela_column_l_;
			$$->add_item ($2);
		}
	}
	;

item:	
	varint the_item {
		$$ = $2;
		if  ($2) {
			String str = $2->str();
			if  (str.length_i())
				LOGOUT (DEBUG_ver) << str << " " << flush;
		}
	}
	;
	
varint:
	VARINT {
		midi_parser_l_g->forward ($1);
	}
	;

the_item: 
	meta_item { 
	}
	| mudela_item {
	}
	| sysex_item {
	}
	;

meta_item:
	META_EVENT the_meta_item {
		$$ = $2;
	}
	|
	META_EVENT U8 U8 U8 {
		$$ = 0;
	}
	;

the_meta_item:
	SEQUENCE INT16 {
	}
	| text_item DATA {
		Mudela_text::Type type = (Mudela_text::Type)$1;
		$$ = 0;
		switch  (type)
			{
			case Mudela_text::COPYRIGHT:
			        midi_parser_l_g->copyright_str_ = *$2;
				break;
			case Mudela_text::TRACK_NAME:
			        midi_parser_l_g->track_name_str_ = *$2;
				break;
			case Mudela_text::INSTRUMENT_NAME:
			        midi_parser_l_g->instrument_str_ = *$2;
				break;
			default:
				$$ = new Mudela_text (type, *$2);
				break;
			}
		LOGOUT (DEBUG_ver) << *$2 << endl;
		delete $2;
	}
	| END_OF_TRACK {
		$$ = 0;
	}
	| TEMPO U8 U8 U8 { 
		$$ = new Mudela_tempo ( ($2 << 16) +  ($3 << 8) + $4);
		LOGOUT (DEBUG_ver) << $$->str() << endl;
		midi_parser_l_g->set_tempo ( ($2 << 16) +  ($3 << 8) + $4);
	}
	| SMPTE_OFFSET U8 U8 U8 U8 U8 { 
		$$ = 0;
	}
	| TIME U8 U8 U8 U8 { 
		$$ = new Mudela_meter ($2, $3, $4, $5);
		LOGOUT (DEBUG_ver) << $$->str() << endl;
		midi_parser_l_g->set_meter ($2, $3, $4, $5);
	}
	| KEY I8 I8 { 
		$$ = new Mudela_key ($2, $3);
		midi_parser_l_g->set_key ($2, $3 );
	}
	| SSME DATA {
		$$ = new Mudela_text ((Mudela_text::Type)0, *$2);
		delete $2;
	}
	;

text_item: 
	YYTEXT {
		LOGOUT (DEBUG_ver) << "\n% Text: ";
	}
	| YYCOPYRIGHT {
		LOGOUT (DEBUG_ver) << "\n% Copyright: ";
	}
	| YYTRACK_NAME {
		LOGOUT (DEBUG_ver) << "\n% Track  name: ";
	}
	| YYINSTRUMENT_NAME {
		LOGOUT (DEBUG_ver) << "\n% Instrument  name: ";
	}
	| YYLYRIC {
		LOGOUT (DEBUG_ver) << "\n% Lyric: ";
	}
	| YYMARKER {
		LOGOUT (DEBUG_ver) << "\n% Marker: ";
	}
	| YYCUE_POINT {
		LOGOUT (DEBUG_ver) << "\n% Cue point: ";
	}
	;

mudela_item: 
	running_status {
	}
	| data_entry {
	}
	| all_notes_off {
	}
	| note_off {
	}
	| note_on {
	}
	| polyphonic_aftertouch {
	}
	| controlmode_change {
	}
	| program_change {
	}
	| channel_aftertouch {
	}
	| pitchwheel_range {
	}
	;

running_status:
	RUNNING_STATUS mudela_item {
		$$ = $2;
	}
	;

data_entry:
	DATA_ENTRY U8 {
		$$ = 0;
	}
	;

all_notes_off:
	ALL_NOTES_OFF U8 U8 {
		midi_parser_l_g->note_end_all();
		$$ = 0;
	}
	;

note_off:
	NOTE_OFF U8 U8 {
		int i = $1;
		i = i & ~0x80;
		midi_parser_l_g->note_end ($1 & ~0x80, $2, $3);
		$$ = 0;
	}
	;

note_on:
	NOTE_ON U8 U8 {
		int i = $1;
		i = i & ~0x90;
		$$ = 0;
                if  ($3)
			midi_parser_l_g->note_begin ($1 & ~0x90, $2, $3);
		/*
		  sss: some broken devices encode NOTE_OFF as 
		       NOTE_ON with zero volume
		 */
		else 
			midi_parser_l_g->note_end ($1 & ~0x90, $2, $3);
	}
	;

polyphonic_aftertouch:
	POLYPHONIC_AFTERTOUCH U8 U8 {
		$$ = 0;
	}
	;

controlmode_change:
	CONTROLMODE_CHANGE U8 U8 {
		$$ = 0;
	}
	;

program_change:
	PROGRAM_CHANGE U8 {
		$$ = 0;
	}
	;

channel_aftertouch:
	CHANNEL_AFTERTOUCH U8 U8 {
		$$ = 0;
	}
	;

pitchwheel_range:
	PITCHWHEEL_RANGE U8 U8 {
		$$ = 0;
	}
	;

sysex_item:
	SYSEX_EVENT1 DATA {
		$$ = 0;
	}
	| SYSEX_EVENT2 DATA { // U8 ?
		$$ = 0;
	}
	;
