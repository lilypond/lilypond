/*
  process command line, GNU style.

  this is Copyleft (c) 1996--2007 Han-Wen Nienhuys, <hanwen@xs4all.nl>
*/

#include "getopt-long.hh"

#include <cstring>
#include <cassert>
#include <cstdlib>

#include "config.hh"
#include "international.hh"

#if !HAVE_GETTEXT
inline char *
gettext (char const *s)
{
  return (char *)s;
}
#else
#include <libintl.h>
#endif

long
Getopt_long::get_argument_index ()
{
  long l;
  if (!optional_argument_str0_
      || sscanf (optional_argument_str0_, "%ld", &l) != 1)
    report (E_ILLEGALARG);

  return l;
}

const Long_option_init *
Getopt_long::parselong ()
{
  char const *optnm = arg_value_char_a_a_[array_index_] + 2;
  assert (*optnm);

  char const *endopt = strchr (optnm, '=');
  int searchlen = (endopt) ? endopt - optnm : strlen (optnm);

  found_option_ = 0;
  for (int i = 0; i < table_len_; i++)
    {
      char const *ln = option_a_[i].longname_str0_;

      if (ln && !strncmp (ln, optnm, searchlen))
	{
	  found_option_ = option_a_ + i;
	  break;
	}
    }

  if (!found_option_)
    {
      report (E_UNKNOWNOPTION);
      return 0;
    }
  array_index_++;
  argument_index_ = 0;

  if (found_option_->take_arg_str0_)
    {
      if (endopt)
	optional_argument_str0_ = endopt +1; // a '='
      else
	{
	  optional_argument_str0_ = arg_value_char_a_a_[array_index_];
	  array_index_++;
	}
      if (!optional_argument_str0_)
	report (E_ARGEXPECT);
    }
  else
    {
      optional_argument_str0_ = 0;
      if (endopt)
	report (E_NOARGEXPECT);
    }

  return found_option_;
}

string
Long_option_init::to_string () const
{
  string str;
  if (shortname_char_)
    str += "-" + shortname_char_;
  if (shortname_char_ && longname_str0_)
    str += ", ";
  if (longname_str0_)
    str += string ("`--") + longname_str0_ + "'";
  return str;
}

string
Long_option_init::str_for_help () const
{
  string s;
  if (shortname_char_)
    s = "-" + ::to_string (shortname_char_);
  else
    s = "  ";

  s = s + ((shortname_char_ && longname_str0_) ? ", " : "  ");

  if (longname_str0_)
    s = s + "--" + longname_str0_;

  if (take_arg_str0_)
    {
      if (longname_str0_)
	s = s + "=";
      else
	s = s + " ";

      s = s + gettext (take_arg_str0_);
    }
  return s;
}

// report an error, GNU style.
void
Getopt_long::report (Errorcod c)
{
  error_ = c;
  if (!error_out_)
    return;

  string str = arg_value_char_a_a_[0];
  str += ": ";
  switch (c)
    {
    case E_ARGEXPECT:
      str += _f ("option `%s' requires an argument",
		 found_option_->to_string ());
      break;
    case E_NOARGEXPECT:
      str += _f ("option `%s' does not allow an argument",
		 found_option_->to_string ());
      break;
    case E_UNKNOWNOPTION:
      str += _f ("unrecognized option: `%s'",
		 string (argument_index_
			     ? string ("-" + string (1, arg_value_char_a_a_[array_index_][argument_index_]))
			 : string (arg_value_char_a_a_[array_index_])));
      break;
    case E_ILLEGALARG:
      str += _f ("invalid argument `%s' to option `%s'",
		 optional_argument_str0_, found_option_->to_string ());
      break;
    default:
      assert (false);
    }
  fprintf (error_out_, "%s\n", str.c_str ());
  exit (2);
}

const Long_option_init *
Getopt_long::parseshort ()
{
  char c = arg_value_char_a_a_[array_index_][argument_index_];
  found_option_ = 0;
  assert (c);

  for (int i = 0; i < table_len_; i++)
    if (option_a_[i].shortname_char_ == c)
      {
	found_option_ = option_a_ + i;
	break;
      }

  if (!found_option_)
    {
      report (E_UNKNOWNOPTION);
      return 0;
    }

  argument_index_++;
  if (!found_option_->take_arg_str0_)
    {
      optional_argument_str0_ = 0;
      return found_option_;
    }
  optional_argument_str0_ = arg_value_char_a_a_[array_index_] + argument_index_;

  array_index_++;
  argument_index_ = 0;

  if (!optional_argument_str0_[0])
    {
      optional_argument_str0_ = arg_value_char_a_a_[array_index_];
      array_index_++;
    }
  if (!optional_argument_str0_)
    report (E_ARGEXPECT);

  return found_option_;
}

const Long_option_init *
Getopt_long::operator () ()
{
  if (!ok ())
    return 0;

  next ();
  if (!ok ())
    return 0;

  if (argument_index_)
    return parseshort ();

  char const *argument = arg_value_char_a_a_[array_index_];

  if (argument[0] != '-')
    return 0;

  if (argument[1] == '-') {// what to do with "command  --  bla"
    if (argument[2])
      return parselong ();
    else
      return 0;
  }
  else
    {
      if (argument[ 1 ])
	{
	  argument_index_ = 1;
	  return parseshort ();
	}
      else
	return 0;
    }
}

Getopt_long::Getopt_long (int c, char **v, Long_option_init *lo)
{
  option_a_ = lo;
  error_out_ = stderr;
  arg_value_char_a_a_ = v;
  argument_count_ = c;
  array_index_ = 1;
  argument_index_ = 0;

  //    reached end of option table?
  table_len_ = 0;
  for (int i = 0; option_a_[i].longname_str0_ || option_a_[i].shortname_char_; i++)
    table_len_++;
}

bool
Getopt_long::ok () const
{
  return array_index_ < argument_count_;
}

void
Getopt_long::next ()
{
  error_ = E_NOERROR;
  while (array_index_ < argument_count_
	 && !arg_value_char_a_a_[array_index_][argument_index_])
    {
      array_index_++;
      argument_index_ = 0;
    }
}

char const *
Getopt_long::current_arg ()
{
  if (array_index_ >= argument_count_)
    return 0;
  char const *a = arg_value_char_a_a_[array_index_];
  return a + argument_index_;
}

char const *
Getopt_long::get_next_arg ()
{
  char const *a = current_arg ();
  if (a)
    {
      array_index_++;
      argument_index_ = 0;
    }
  return a;
}

const int EXTRA_SPACES = 5;

string
Long_option_init::table_string (Long_option_init *l)
{
  string tabstr = "";

  int wid = 0;
  for (int i = 0; l[i].shortname_char_ || l[i].longname_str0_; i++)
    wid = max (int(wid), int(l[i].str_for_help ().length ()));

  for (int i = 0; l[i].shortname_char_ || l[i].longname_str0_; i++)
    {
      string s = "  " + l[i].str_for_help ();
      s += string (wid - s.length () + EXTRA_SPACES, ' ');

      string help_text (gettext (l[i].help_str0_));
      replace_all (&help_text, "\n",
		   "\n" + string (wid + EXTRA_SPACES + 2, ' '));
      tabstr += s + help_text + "\n";
    }

  return tabstr;
}

int
Long_option_init::compare (Long_option_init const &a, Long_option_init const &b)
{
  if (a.shortname_char_ && b.shortname_char_ && a.shortname_char_- b.shortname_char_)
    return a.shortname_char_ - b.shortname_char_;

  if (b.shortname_char_ && a.longname_str0_)
    {
      char s[2] = {b.shortname_char_, 0};
      return strcmp (a.longname_str0_, s);
    }
  if (a.shortname_char_ && b.longname_str0_)
    {
      char s[2] = {a.shortname_char_, 0};
      return strcmp (s, b.longname_str0_);
    }

  return strcmp (a.longname_str0_, b.longname_str0_);
}
