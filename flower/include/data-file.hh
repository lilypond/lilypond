/*
  data-file.hh -- declare Data_file

  source file of the LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef DATAFILE_HH
#define DATAFILE_HH

#include "text-stream.hh"

/// read a data file
class Data_file : private Text_stream
{
    
public:
  bool rawmode;

  Text_stream::line;    
  Text_stream::eof;
  Text_stream::get_name;    

  char data_get();    
  void data_unget (char c) {
    unget (c);
  }

  /// read line, eat #\n#
  String get_line();
    
  /// read a word till next space, leave space. Also does quotes
  String get_word();

  /// gobble horizontal white stuff.
  void gobble_white();

  /// gobble empty stuff before first field.
  void gobble_leading_white();
  Data_file (String s) : Text_stream (s) {
    //*mlog << "(" << s << flush;	
    rawmode=  false;	
  }

  ~Data_file()  {
    //	*mlog << ")"<<flush;	
  }    

  warning (String s) {
    message ("warning: " + s);
  }
  error (String s){
    message (s);
    exit (1);    
  }
};
#endif // DATAFILE_HH
