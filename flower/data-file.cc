/*   
  data-file.cc --  implement Data_file 
  
  source file of the Flower Library
  
  (c) '95, '96, '97 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
  */
#include <fstream.h>
#include <ctype.h>

#include "international.hh"
#include "data-file.hh"

void
Data_file::gobble_white()
{
  char c;

  while ((c=data_get()) == ' ' ||c == '\t')
    if (eof_b())
      return;

  data_unget (c);
}

String
Data_file::get_word()
{// should handle escape seq's
  String s;

  while (1)
    {
      char 	c  = data_get();
      
      if  (eof_b ())
	break;

      if (isspace (c))
	{
	  data_unget (c);
	  break;
	}


      if (c == '\"')
	{
	  rawmode= true;

	  while ((c  = data_get()) != '\"')
	    if (eof_b ())
	      error (_ ("EOF in a string"));
	    else
	      s += to_str (c);


	  rawmode= false;
	}
      else
	s += to_str (c);
    }

  return s;
}

/**  get a char
   Only class member who uses text_file::get
   */
char
Data_file::data_get()
{
  char c =  get();
  if (!rawmode && c == '#') // gobble comment
    {
      while (!eof_b () && (c = get()) != '\n')
	;
      return '\n';
    }

  return c;
}

/// read line, gobble '\n'
String
Data_file::get_line()
{
  char c;
  String s;

  while (!eof_b () && (c  = data_get()) != '\n')
    s += to_str (c);
  return s;
}

/// gobble stuff before first entry on a line.
void
Data_file::gobble_leading_white()
{
  // eat blank lines.
  while (!eof_b ())
    {
      char c = data_get();
      if (!isspace (c))
	{
	  data_unget (c);
	  break;
	}
    }
}

Data_file::Data_file (String s)
  : Text_stream (s) 
{
  //*mlog << "(" << s << flush;	
  rawmode=  false;	
}

void
Data_file::warning (String s)
{
  message (_ ("warning: ") + s);
}

void
Data_file::error (String s)
{
  message (s);
  exit (1);    
}

String
Data_file::gulp ()
{
  String s;

  while (!eof_b ())
    {
      s += to_str (data_get ());
    }
  return s;
}
  

Data_file::~Data_file ()
{
}
