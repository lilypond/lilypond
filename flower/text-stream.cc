#include <iostream.h>
#include "text-stream.hh"

Text_stream::Text_stream (String fn)
{
  ios::sync_with_stdio();
  if (fn == "")
    {
      name = _ ("<stdin>");
      f = stdin;
    }

  else
    {
      name = fn;
      f = fopen (fn.ch_C (), "r");
    }

  if (!f)
    {
      cerr << __FUNCTION__ 
	   << ": " << _f ("can't open file: `%s'", fn) << '\n';
      exit (1);
    }

  line_no = 1;
}

void
Text_stream::message (String s)
{
  cerr << '\n'<<get_name() << ": " << line ()<<": "<<s<<endl;
}

bool
Text_stream::eof_b ()
{
  /* UGH UGH ugh*/
    return
      // !pushback.size () && 
      feof (f);
}

Text_stream::~Text_stream()
{
    if (!eof_b()) 
      cerr <<__FUNCTION__<< ": closing unended file";
    
    fclose (f);
  }
