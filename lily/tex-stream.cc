/*
  tex-stream.cc -- implement Tex_stream

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/

#include <fstream.h>
#include <time.h>

#include "tex.hh"
#include "main.hh"
#include "tex-stream.hh"
#include "debug.hh"

const int MAXLINELEN = 200;

Tex_stream::Tex_stream (String filename)
{
  if (filename.length_i () && (filename != "-"))
    os = new ofstream (filename.ch_C ());
  else
//    os = new ostream (cout.ostreambuf ());
    os = new ostream (cout._strbuf);
  if (!*os)
    error (_f ("can't open file: `%s\'", filename));
  nest_level = 0;
  line_len_i_ = 0;
  outputting_comment=false;
  header();
}
void
Tex_stream::header()
{
  *os << _ ("% Creator: ");
  if (no_timestamps_global_b)
    *os << "GNU LilyPond\n";
  else
    *os << get_version_str() << '\n';
  *os << _ ("% Automatically generated");
  if (no_timestamps_global_b)
    *os << ".\n";
  else
    {
      *os << _ (", at ");
      time_t t (time (0));
      *os << ctime (&t) << "%\n";
    }
}

Tex_stream::~Tex_stream()
{
  *os << flush;
  if (!*os)
    {
      warning(_ ("error syncing file (disk full?)"));
      exit_status_i_ = 1;
    }
  delete os;
  assert (nest_level == 0);
}

// print string. don't forget indent.
Tex_stream&
Tex_stream::operator << (Scalar s)
{
  for (char const *cp = s.ch_C (); *cp; cp++)
    {
	if (outputting_comment)
	  {
	    *os << *cp;
	    if (*cp == '\n')
	      {
		outputting_comment=false;

	      }
	    continue;
	  }
	line_len_i_ ++;
	switch (*cp)
	    {
	    case '%':
		outputting_comment = true;
		*os << *cp;
		break;
	    case '{':
		nest_level++;
		*os << *cp;
		break;
	    case '}':
		nest_level--;
		*os << *cp;

		if (nest_level < 0)
		  {
		    delete os;	// we want to see the remains.
		    assert (nest_level>=0);
		  }
		/* FALLTHROUGH */

	    case '\n':
		break_line();
		break;
	    case ' ':
		*os <<  ' ';
		if (line_len_i_ > MAXLINELEN)
		   break_line();

		break;
	    default:
		*os << *cp;
		break;
	      }
    }
  return *this;
}

void
Tex_stream::break_line()
{
  *os << "%\n";
  *os << to_str (' ', nest_level);
  line_len_i_ = 0;
}

