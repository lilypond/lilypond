%{//-*-Fundamental-*-
// midi-lexer.l

/* 
  yes, i know that midi is not really a (n evolving?) language,
  and that using lex/yacc to parse midi is overkill, as well as
  a grand example of misuse and asking for performance loss.

  it is, however, quite robust, simple, and very easy to extend
  incrementally.
 */

/*
  backup rules

  after making a change to the lexer rules, run 
      flex -b <this lexer file>
  and make sure that 
      lex.backup
  contains no backup states, but only the reminder
      Compressed tables always back up.
  (don-t forget to rm lex.yy.cc :-)
 */

#include "string-convert.hh"
#include "mi2mu-global.hh"
#include "mi2mu-proto.hh"
#include "my-midi-lexer.hh"
#include "midi-parser.hh"

#define YY_USER_ACTION char_count_ += YYLeng(); // ugh

%}

%option c++
%option noyywrap
%option nodefault
%option debug
%option yyclass="My_midi_lexer"
%option stack

%x data
%x event
%x i8
%x u8
%x int16
%x int32
%x meta_event
%x track

U8		[\x00-\xff]
I8		{U8}
INT16		{U8}{U8}
BACKUP_INT16_0	{U8}
INT32		{INT16}{INT16}
BACKUP_INT32_0	{U8}
BACKUP_INT32_1	{U8}{U8}
BACKUP_INT32_2	{INT16}{U8}
INT7_8UNSET	[\x00-\x7f]
INT7_8SET	[\x80-\xff]
VARINT		{INT7_8SET}{0,3}{INT7_8UNSET}
BACKUP_VARINT_0 {INT7_8SET}
BACKUP_VARINT_1 {INT7_8SET}{INT7_8SET}
BACKUP_VARINT_2 {INT7_8SET}{INT7_8SET}{INT7_8SET}

HEADER		MThd
TRACK		MTrk
BACKUP_TOP_0	MT
BACKUP_TOP_1	MTh
BACKUP_TOP_2	MTr

RUNNING_STATUS	[\x00-\x5f]
DATA_ENTRY	[\x60-\x79]
ALL_NOTES_OFF	[\x7a-\x7f]
NOTE_OFF	[\x80-\x8f]
NOTE_ON		[\x90-\x9f]
POLYPHONIC_AFTERTOUCH	[\xa0-\xaf]
CONTROLMODE_CHANGE	[\xb0-\xbf]
PROGRAM_CHANGE	[\xc0-\xcf]
CHANNEL_AFTERTOUCH	[\xd0-\xdf]
PITCHWHEEL_RANGE	[\xe0-\xef]

SYSEX_EVENT1	[\xf0]
SYSEX_EVENT2	[\xf7]

META_EVENT	[\xff]

SEQUENCE	[\x00][\x02]
YYTEXT		[\x01] 
YYCOPYRIGHT	[\x02]
YYTRACK_NAME	[\x03]
YYINSTRUMENT_NAME	[\x04]
YYLYRIC		[\x05]
YYMARKER		[\x06]
YYCUE_POINT	[\x07]

END_OF_TRACK	[\x2f][\x00]
TEMPO		[\x51][\x03]
SMPTE_OFFSET	[\x54][\x05]
TIME		[\x58][\x04]
KEY		[\x59][\x02]
SSME		[\0x7f][\x03]

%%

{HEADER} {
	LOGOUT(DEBUG_ver) << "lex: header" << endl;
	yy_push_state(int16); 
	yy_push_state(int16); 
	yy_push_state(int16); 
	yy_push_state(int32); 
	return HEADER;
}

{TRACK} {
	LOGOUT(DEBUG_ver) << "lex: track" << endl;
	yy_push_state(track); 
	yy_push_state(int32); 
	return TRACK;
}
{U8}	{
	error(String("top level: header expected: ")
		+ String_convert::bin2hex_str(String(*YYText())));
	exit(1);
}
{BACKUP_TOP_0} {
	error(String("top level: header expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
{BACKUP_TOP_1} {
	error(String("top level: header expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
{BACKUP_TOP_2} {
	error(String("top level: header expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
<int32>{INT32}	{ // really signed? 
	LOGOUT(DEBUG_ver) << "lex: int32" << endl;
	assert(YYLeng() == 4);
	String str((Byte const*)YYText(), YYLeng());
	yylval.i = String_convert::bin2_i(str);
	yy_pop_state();
	return INT32;
}
<int32>{BACKUP_INT32_0} {
	error(String("int32: int32 expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
<int32>{BACKUP_INT32_1} {
	error(String("int32: int32 expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
<int32>{BACKUP_INT32_2} {
	error(String("int32: int32 expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
<int16>{INT16}	{ // really signed?
	LOGOUT(DEBUG_ver) << "lex: int16" << endl;
	assert(YYLeng() == 2);
	String str((Byte const*)YYText(), YYLeng());
	yylval.i = (short)String_convert::bin2_i(str);
	yy_pop_state();
	return INT16;
}
<int16>{BACKUP_INT16_0} {
	error(String("int16: int16 expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
<i8>{I8}	{
	LOGOUT(DEBUG_ver) << "lex: i8" << endl;
	assert(YYLeng() == 1);
//	yylval.byte = *(signed char*)YYText();
	yylval.i = *(signed char*)YYText();
	yy_pop_state(); 
	return I8;
}
<u8>{U8}	{
	LOGOUT(DEBUG_ver) << "lex: u8" << endl;
	assert(YYLeng() == 1);
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	yy_pop_state(); 
	return U8;
}

<track>{VARINT} {
	String str((Byte const*)YYText(), YYLeng());
	yylval.i = My_midi_lexer::varint2_i(str);
	LOGOUT(DEBUG_ver) << String("lex: track: varint(") 
		+ String(yylval.i) + "): "
		+ String_convert::bin2hex_str(str) << endl;
	yy_push_state(event); 
	return VARINT;
}
<track>{U8}	{
	error(String("track: illegal byte: ") 
		+ String_convert::bin2hex_str(String(*YYText())));
	exit(1);
}
<track>{BACKUP_VARINT_0}{U8} {
	error(String("track: varint expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
<track>{BACKUP_VARINT_1}{U8} {
	error(String("track: varint expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
<track>{BACKUP_VARINT_2}{U8} {
	error(String("track: varint expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
<event>{RUNNING_STATUS}	{
//	yylval.byte = *(Byte*)YYText();
//	yylval.i = *(Byte*)YYText();
	yylval.i = running_status_i_;
	LOGOUT(DEBUG_ver) << String ("lex: running status: ") + String(yylval.i) << endl;
	/*
	 'running status' rather means 'missing status'.
	 we'll put the running status data back, prepend (unput) 
	 the running status, and try again.
	 */
	yyless(0);
	unput(running_status_i_);
	return RUNNING_STATUS;
}
<event>{DATA_ENTRY}	{
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	LOGOUT(DEBUG_ver) << String ("lex: undefined data entry: ") + String(yylval.i) << endl;
	yy_pop_state(); 
	yy_push_state(u8);
	return DATA_ENTRY;
}
<event>{ALL_NOTES_OFF}	{
	LOGOUT(DEBUG_ver) << "lex: all note off" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	LOGOUT(DEBUG_ver) << String ("lex: all notes off: ") + String(yylval.i) << endl;
	yy_pop_state(); 
	yy_push_state(u8);
	yy_push_state(u8);
	return ALL_NOTES_OFF;
}
<event>{NOTE_OFF}	{
	LOGOUT(DEBUG_ver) << "lex: note off" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	running_status_i_ = yylval.i;
	yy_pop_state(); 
	yy_push_state(u8);
	yy_push_state(u8);
	return NOTE_OFF;
}
<event>{NOTE_ON}	{
	LOGOUT(DEBUG_ver) << "lex: note on" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	running_status_i_ = yylval.i;
	yy_pop_state(); 
	yy_push_state(u8);
	yy_push_state(u8);
	return NOTE_ON;
}
<event>{POLYPHONIC_AFTERTOUCH}	{
	LOGOUT(DEBUG_ver) << "lex: polyphonic aftertouch" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	running_status_i_ = yylval.i;
	yy_pop_state(); 
	yy_push_state(u8);
	yy_push_state(u8);
	return POLYPHONIC_AFTERTOUCH;
}
<event>{CONTROLMODE_CHANGE}	{
	LOGOUT(DEBUG_ver) << "lex: controlmode change" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	running_status_i_ = yylval.i;
	yy_pop_state(); 
	yy_push_state(u8);
	yy_push_state(u8);
	return CONTROLMODE_CHANGE;
}
<event>{PROGRAM_CHANGE}	{
	LOGOUT(DEBUG_ver) << "lex: program change" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	running_status_i_ = yylval.i;
	yy_pop_state(); 
	yy_push_state(u8);
	return PROGRAM_CHANGE;
}
<event>{CHANNEL_AFTERTOUCH}	{
	LOGOUT(DEBUG_ver) << "lex: channel aftertouch" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	running_status_i_ = yylval.i;
	yy_pop_state(); 
	yy_push_state(u8);
	yy_push_state(u8);
	return CHANNEL_AFTERTOUCH;
}
<event>{PITCHWHEEL_RANGE} {
	LOGOUT(DEBUG_ver) << "lex: pitchwheel range" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	running_status_i_ = yylval.i;
	yy_pop_state(); 
	yy_push_state(u8);
	yy_push_state(u8);
	return PITCHWHEEL_RANGE;
}
<event>{SYSEX_EVENT1} {	// len data
	LOGOUT(DEBUG_ver) << "lex: sysex1" << endl;
	yy_pop_state(); 
	yy_push_state(data);
	return SYSEX_EVENT1;
}
<event>{SYSEX_EVENT2} {	// len data
	LOGOUT(DEBUG_ver) << "lex: sysex2" << endl;
	yy_pop_state(); 
//	yy_push_state(u8); //?
	yy_push_state(data);
	return SYSEX_EVENT2;
}
<event>{META_EVENT}	{
	LOGOUT(DEBUG_ver) << "lex: meta" << endl;
	yy_push_state(meta_event);
	return META_EVENT;
}
<event>{U8}	{
	error(String("event: illegal byte: ") 
		+ String_convert::bin2hex_str(String(*YYText())));
	exit(1);
}
<meta_event>{SEQUENCE}	{	// ssss sequence number
	LOGOUT(DEBUG_ver) << "lex: sequence" << endl;
	yy_pop_state();
	yy_pop_state();
	yy_push_state(int16);
	return SEQUENCE;
}
<meta_event>{YYTEXT}	{		// len data
	LOGOUT(DEBUG_ver) << "lex: text" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	yy_pop_state();
	yy_pop_state();
	yy_push_state(data);
	return YYTEXT;
}
<meta_event>{YYCOPYRIGHT}	{
	LOGOUT(DEBUG_ver) << "lex: copyright" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	yy_pop_state();
	yy_pop_state();
	yy_push_state(data);
	return YYCOPYRIGHT;
}
<meta_event>{YYTRACK_NAME}	{
	LOGOUT(DEBUG_ver) << "lex: track name" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	yy_pop_state();
	yy_pop_state();
	yy_push_state(data);
	return YYTRACK_NAME;
}
<meta_event>{YYINSTRUMENT_NAME}	{
	LOGOUT(DEBUG_ver) << "lex: instrument name" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	yy_pop_state();
	yy_pop_state();
	yy_push_state(data);
	return YYINSTRUMENT_NAME;
}
<meta_event>{YYLYRIC}	{
	LOGOUT(DEBUG_ver) << "lex: lyric" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	yy_pop_state();
	yy_pop_state();
	yy_push_state(data);
	return YYLYRIC;
}
<meta_event>{YYMARKER}	{
	LOGOUT(DEBUG_ver) << "lex: marker" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	yy_pop_state();
	yy_pop_state();
	yy_push_state(data);
	return YYMARKER;
}
<meta_event>{YYCUE_POINT}	{
	LOGOUT(DEBUG_ver) << "lex: cue point" << endl;
//	yylval.byte = *(Byte*)YYText();
	yylval.i = *(Byte*)YYText();
	yy_pop_state();
	yy_pop_state();
	yy_push_state(data);
	return YYCUE_POINT;
}
<meta_event>{TEMPO}	{ 	// tttttt usec
	LOGOUT(DEBUG_ver) << "lex: tempo" << endl;
	yy_pop_state();
	yy_pop_state();
	yy_push_state(u8);
	yy_push_state(u8);
	yy_push_state(u8);
	return TEMPO;
}
<meta_event>{SMPTE_OFFSET}	{ 		// hr mn se fr ff
	LOGOUT(DEBUG_ver) << "lex: smpte offset" << endl;
	yy_pop_state();
	yy_pop_state();
	yy_push_state(u8);
	yy_push_state(u8);
	yy_push_state(u8);
	yy_push_state(u8);
	yy_push_state(u8);
	return SMPTE_OFFSET;
}
<meta_event>{TIME}	{		// nn dd cc bb
	LOGOUT(DEBUG_ver) << "lex: time" << endl;
	yy_pop_state();
	yy_pop_state();
	yy_push_state(u8);
	yy_push_state(u8);
	yy_push_state(u8);
	yy_push_state(u8);
	return TIME;
}
<meta_event>{KEY}	{	// sf mi
	LOGOUT(DEBUG_ver) << "lex: key" << endl;
	yy_pop_state();
	yy_pop_state();
	yy_push_state(i8);
	yy_push_state(i8);
	return KEY;
}
<meta_event>{SSME}	{	// len data
	LOGOUT(DEBUG_ver) << "lex: smme" << endl;
	yy_pop_state();
	yy_pop_state();
	yy_push_state(data);
	return SSME;
}
<meta_event>{END_OF_TRACK} {
	LOGOUT(DEBUG_ver) << "lex: end of track" << endl;
	yy_pop_state();
	yy_pop_state();
	yy_pop_state();
	return END_OF_TRACK;
}
<meta_event>{U8} {
	warning(String("meta_event: unimplemented event: ")
		+ String_convert::bin2hex_str(String(*YYText()))
// huh?
//		,this->here_ch_C() 
	);
	yy_pop_state();
	yy_pop_state();
	yy_push_state(u8); 
	yy_push_state(u8);
	return U8;
}

<data>{VARINT} {
	LOGOUT(DEBUG_ver) << "lex: data" << endl;
	String str((Byte const*)YYText(), YYLeng());
	int i = My_midi_lexer::varint2_i(str);
	String* str_p = new String;
	while (i--)
		*str_p += (char)yyinput();
	yylval.str_p = str_p;
	yy_pop_state();
	return DATA;
}
<data>{U8}	{
	error(String("data: illegal byte: ")
		+ String_convert::bin2hex_str(String(*YYText())));
	exit(1);
}
<data>{BACKUP_VARINT_0}{U8} {
	error(String("data: varint expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
<data>{BACKUP_VARINT_1}{U8} {
	error(String("data: varint expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}
<data>{BACKUP_VARINT_2}{U8} {
	error(String("data: varint expected: ")
		+ String_convert::bin2hex_str(String(*(YYText()))));
	exit(1);
}

<<EOF>> {
//	LOGOUT(NORMAL_ver) << "<<EOF>>";

	if (!close_i())
 	  yyterminate(); // can't move this, since it actually rets a YY_NULL
}

%%

