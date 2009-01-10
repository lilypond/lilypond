/*
  includable-lexer.cc -- implement Includable_lexer

  source file of the LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "includable-lexer.hh"

#include <sstream>
using namespace std;

#include "config.hh"

#include "file-name.hh"
#include "file-path.hh"
#include "international.hh"
#include "main.hh"
#include "source-file.hh"
#include "sources.hh"
#include "warn.hh"

#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE 16384
#endif

#ifndef YY_START
#define YY_START				\
  ((yy_start - 1) / 2)
#define YYSTATE YY_START
#endif

/* Flex >= 2.5.29 has include stack; but we don't use that yet.  */
#if !HAVE_FLEXLEXER_YY_CURRENT_BUFFER
#define yy_current_buffer						\
  (yy_buffer_stack != 0 ? yy_buffer_stack[yy_buffer_stack_top] : 0)
#endif

extern bool relative_includes;

Includable_lexer::Includable_lexer ()
{
#if HAVE_FLEXLEXER_YY_CURRENT_BUFFER
  yy_current_buffer = 0;
#endif
}

/** Set the new input file to NAME, remember old file.  */
void
Includable_lexer::new_input (string name, Sources *sources)
{
  string current_dir = dir_name (main_input_name_);
  if (relative_includes)
    current_dir = include_stack_.size () ? dir_name (include_stack_.back ()->name_string ()) : "";

  Source_file *file = sources->get_file (name, current_dir);
  if (!file)
    {
      string msg = _f ("cannot find file: `%s'", name);
      msg += "\n";
      msg += _f ("(search path: `%s')",
		 (current_dir.length () ? (current_dir + PATHSEP) : "") + sources->path_->to_string ().c_str ());
      LexerError (msg.c_str ());
      return;
    }
  file_name_strings_.push_back (file->name_string ());

  char_count_stack_.push_back (0);
  if (yy_current_buffer)
    state_stack_.push_back (yy_current_buffer);

  if (be_verbose_global)
    progress_indication (string ("[") + file->name_string ());

  include_stack_.push_back (file);

  /* Ugh. We'd want to create a buffer from the bytes directly.

  Whoops.  The size argument to yy_create_buffer is not the
  filelength but a BUFFERSIZE.  Maybe this is why reading stdin fucks up.  */
  yy_switch_to_buffer (yy_create_buffer (file->get_istream (), YY_BUF_SIZE));
}

void
Includable_lexer::new_input (string name, string data, Sources *sources)
{
  Source_file *file = new Source_file (name, data);
  sources->add (file);
  file_name_strings_.push_back (name);

  char_count_stack_.push_back (0);
  if (yy_current_buffer)
    state_stack_.push_back (yy_current_buffer);

  if (be_verbose_global)
    progress_indication (string ("[") + name);
  include_stack_.push_back (file);

  yy_switch_to_buffer (yy_create_buffer (file->get_istream (), YY_BUF_SIZE));
}

/** pop the inputstack.  conceptually this is a destructor, but it
    does not destruct the Source_file that Includable_lexer::new_input
    creates.  */
bool
Includable_lexer::close_input ()
{
  include_stack_.pop_back ();
  char_count_stack_.pop_back ();
  if (be_verbose_global)
    progress_indication ("]");
  yy_delete_buffer (yy_current_buffer);
#if HAVE_FLEXLEXER_YY_CURRENT_BUFFER
  yy_current_buffer = 0;
#endif
  if (state_stack_.empty ())
    {
#if HAVE_FLEXLEXER_YY_CURRENT_BUFFER
      yy_current_buffer = 0;
#endif
      return false;
    }
  yy_switch_to_buffer (state_stack_.back ());
  state_stack_.pop_back ();
  return true;
}

char const *
Includable_lexer::here_str0 () const
{
  if (include_stack_.empty ())
    return 0;
  return include_stack_.back ()->c_str () + char_count_stack_.back ();
}

Includable_lexer::~Includable_lexer ()
{
  while (!include_stack_.empty ())
    close_input ();
}

Source_file *
Includable_lexer::get_source_file () const
{
  if (include_stack_.empty ())
    return 0;
  return include_stack_.back ();
}
