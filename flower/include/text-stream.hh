
#ifndef TEXTSTR_HH
#define TEXTSTR_HH

#include <stdio.h>
#include <ctype.h>
#include "string.hh"
#include "array.hh"

/**
  line counting input stream. 
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
  Text_stream (String fn);
  String get_name() { return name; }
  bool eof_b();
  char get() {
    char c;
	
    if (pushback.empty())
      c = getc (f);	
    else 
      c = pushback.pop();

    if (c =='\n')
      line_no++;
    return c;	
  }
  void unget (char c) {
    if (c =='\n')
      line_no--;
    pushback.push (c);
  }
  char peek() {
    if (eof_b ())
      return -1;
    char c = get();
    unget (c);
    return c;
  }
  bool eol() {
    return (peek() == '\n');
  }
  int line(){
    return line_no;
  }

  ~Text_stream(){
    if (!eof_b()) 
      cerr <<__FUNCTION__<< ": closing unended file";
    
    fclose (f);
  }

  /// GNU format message.
  void message (String s); 
};

#endif
