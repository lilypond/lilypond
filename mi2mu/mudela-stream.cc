//
// mudela-stream.cc
//
// source file of the LilyPond music typesetter
//
// (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include <assert.h>
#include <time.h>
#include <fstream.h>
#include "mi2mu-global.hh"
#include "mudela-item.hh"
#include "mudela-stream.hh"

extern String filename_str_g;

static int const INDENT_i = 8;

Mudela_stream::Mudela_stream (String filename_str)
{
  filename_str_ = filename_str;
  pending_indent_i_ = 0;
  os_p_ = 0;
  indent_i_ = 0;
  comment_mode_b_ = false;
  column_i_ = 0;
  wrap_column_i_ = 68;
  open();
  header();
}

Mudela_stream::~Mudela_stream()
{
  delete os_p_;
  if  (indent_i_)
    warning (_("lily indent level: ") + String (indent_i_));
}

Mudela_stream&
Mudela_stream::operator << (String str)
{
  static String word_sep_str = "{} \t\n";
  while  (str.length_i())
    {
      int i = str.index_any_i (word_sep_str) + 1;
      if  (!i)
	i = str.length_i();
      String word = str.left_str (i);
      str = str.cut (i, str.length_i());
      output_wrapped (word);
    }
  return *this;
}

Mudela_stream&
Mudela_stream::operator << (Mudela_item& mudela_item_r)
{
  mudela_item_r.output (*this);
  *os_p_ << flush;
  return *this;
}

void
Mudela_stream::handle_pending_indent()
{
  *os_p_ << String ('\t', pending_indent_i_);
  column_i_ += pending_indent_i_ * INDENT_i;
  pending_indent_i_ = 0;
}

void
Mudela_stream::header()
{
  *os_p_ << _("% Creator: ");
  if (no_timestamps_b_g)
    *os_p_ << "GNU LilyPond\n"; 
  else
    *os_p_ << mi2mu_version_str() << "\n";
  *os_p_ << _("% Automatically generated");
  if (no_timestamps_b_g)
    *os_p_ << ".\n";
  else
    {
      *os_p_ << _(", at ");
      time_t t (time (0));
      *os_p_ << ctime (&t) << "%\n";
    }
  *os_p_ << _("% from input file: ");
  //  *os_p_ << midi_parser_l_g->filename_str_;
  // ugh
  *os_p_ << filename_str_g;
  *os_p_ << "\n\n";
  // ugh
  *os_p_ << "\\version \"0.1.9\";\n";
}

void
Mudela_stream::open()
{
  os_p_ = new ofstream (filename_str_.ch_C ());
  if  (!*os_p_)
    error  (_("can't open: `") + filename_str_ + "\'");
}

void
Mudela_stream::output (String str)
{
  for  (int i = 0; i < str.length_i(); i++)
    {
      char c = str[ i ];
      switch  (c)
	{
	case '{' :
	case '<' :
	  handle_pending_indent();
	  if  (column_i_ == indent_i_ * INDENT_i)
	    output ("\t");
	  indent_i_++;
	  *os_p_ << c;
	  column_i_++;
	  break;
	case '}' :
	case '>' :
	  assert (indent_i_);
	  indent_i_--;
	  if  (pending_indent_i_)
	    pending_indent_i_--;
	  handle_pending_indent();
	  *os_p_ << c;
	  column_i_++;
	  break;
	case '%' :
	  handle_pending_indent();
	  comment_mode_b_ = true;
	  *os_p_ << c;
	  column_i_++;
	  break;
	case '\t' :
	  handle_pending_indent();
	  *os_p_ << c;
	  column_i_ += INDENT_i;
	  break;
	case '\n' :
	  *os_p_ << endl;
	  pending_indent_i_ = indent_i_;
	  column_i_ = 0;
	  comment_mode_b_ = false;
	  break;
	default :
	  handle_pending_indent();
	  *os_p_ << c;
	  column_i_++;
	  break;
	}
    }
}

void
Mudela_stream::output_wrapped (String str)
{
  // enough room left -> doit
  if  (column_i_ + str.length_i() <= wrap_column_i_)
    {
      output (str);
      return;
    }

  // we're at BOL already; this will never fit -> doit
  if  (column_i_ == indent_i_ * INDENT_i)
    {
      output (str);
      return;
    }

  // ok, let's wrap
  // preserve comment mode
  if  (comment_mode_b_)
    output (String ("\n%"));
  else
    output (String ("\n"));

  output (str);
}
