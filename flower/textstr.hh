
#ifndef TEXTSTR_HH
#define TEXTSTR_HH

#include <stdio.h>
#include <ctype.h>
#include "string.hh"
#include "varray.hh"

/// line counting input stream.
/**
 a stream for textfiles. linecounting. Thin interface getchar and
 ungetchar.  (ungetc is unlimited) 

 should protect get and unget against improper use
*/


class Text_stream
{
    int line_no;

    // could just have used streams. 
    FILE *f;  
    Array<char> pushback;
    String name;
    
 public:
    Text_stream(String fn);
    String get_name() { return name; }
    bool eof() {
	return feof(f);
    }
    bool eol() {
	return (peek() == '\n');
    }
    char peek() {
	char c = get();
	unget(c);
	return c;
    }
    int line(){
	return line_no;
    }

    char    get() {
	char c;
	
	if (pushback.empty())
	    c = getc(f);	
	else 
	    c = pushback.pop();

	if (c =='\n')
	    line_no++;
	return c;	
    }
    void unget(char c) {
	if (c =='\n')
	    line_no--;
	pushback.push(c);
    }
    ~Text_stream (){
	if (!eof()) 
	    cerr <<__FUNCTION__<< ": closing unended file";
    
	fclose(f);
    }

    /// GNU format message.
    void message(String s); 
};
/// read a data file
class Data_file : private Text_stream
{
    
 public:
    bool rawmode;

    Text_stream::line;    
    Text_stream::eof;
    Text_stream::get_name;    

    char data_get();    
    void data_unget(char c) {
	unget(c);
    }

    /// read line, eat #\n#
    String get_line();
    
    /// read a word till next space, leave space. Also does quotes
    String get_word();

    /// gobble horizontal white stuff.
    void gobble_white();

    /// gobble empty stuff before first field.
    void gobble_leading_white();
    Data_file(String s) : Text_stream(s) {
	//*mlog << "(" << s << flush;	
	rawmode=  false;	
    }

    ~Data_file()  {
	//	*mlog << ")"<<flush;	
    }    

    warning(String s) {
	message("warning: " + s);
    }
    error(String s){
	message(s);
	exit(1);    
    }
};
#endif
