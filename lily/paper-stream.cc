/*
  paper-stream.cc -- implement Paper_stream

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <fstream.h>

#include "main.hh"
#include "paper-stream.hh"
#include "debug.hh"

const int MAXLINELEN = 200;

ostream *
open_file_stream (String filename)
{
  ostream *os;
  if (filename.length_i () && (filename != "-"))
    os = new ofstream (filename.ch_C ());
  else
    os = new ostream (cout._strbuf);
  if (!*os)
    error (_f ("can't open file: `%s'", filename));
  return os;
}

void
close_file_stream (ostream *os)
{
  *os << flush;
  if (!*os)
    {
      warning (_ ("Error syncing file (disk full?)"));
      exit_status_i_ = 1;
    }
  delete os;
}  

Paper_stream::Paper_stream (String filename)
{
  os_ = open_file_stream (filename);
  nest_level = 0;
  line_len_i_ = 0;
  outputting_comment_b_=false;
}

Paper_stream::~Paper_stream ()
{
  close_file_stream (os_);
  assert (nest_level == 0);
}

// print string. don't forget indent.
Paper_stream&
Paper_stream::operator << (String s)
{
  for (char const *cp = s.ch_C (); *cp; cp++)
    {
      if (outputting_comment_b_)
	{
	  *os_ << *cp;
	  if (*cp == '\n')
	    {
	      outputting_comment_b_=false;
	      line_len_i_ =0;
	    }
	  continue;
	}
      line_len_i_ ++;
      switch (*cp)
	{
	case '%':
	  outputting_comment_b_ = true;
	  *os_ << *cp;
	  break;
	case '{':
	  nest_level++;
	  *os_ << *cp;
	  break;
	case '}':
	  nest_level--;
	  *os_ << *cp;

	  if (nest_level < 0)
	    {
	      delete os_;	// we want to see the remains.
	      assert (nest_level>=0);
	    }

	  /* don't break line if not nested; very ugly for ps */
	  if (nest_level == 0)
	    break;

	  *os_ << '%';
	  break_line ();
	  break;
	case '\n':
	  break_line ();
	  break;
	case ' ':
	  *os_ <<  ' ';
	  if (line_len_i_ > MAXLINELEN)
	    break_line ();

	  break;
	default:
	  *os_ << *cp;
	  break;
	}
    }
  //urg, for debugging only!!
  *os_ << flush;
  return *this;
}

void
Paper_stream::break_line ()
{
  *os_ << '\n';
  *os_ << to_str (' ', nest_level);
  outputting_comment_b_ = false;
  line_len_i_ = 0;
}

