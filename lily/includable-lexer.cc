/*
  includable-lexer.cc -- implement Includable_lexer

  source file of the LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <sstream>
#include "config.h"

#include "includable-lexer.hh"
#include "file-path.hh"
#include "source-file.hh"
#include "source.hh"
#include "warn.hh"
#include "main.hh"

#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE 16384
#endif

#ifndef YY_START
#define YY_START\
 ((yy_start - 1) / 2)
#define YYSTATE YY_START
#endif

/* Flex >= 2.5.29 has include stack; but we don't use that yet.  */
#ifndef HAVE_FLEXLEXER_YY_CURRENT_BUFFER
#define yy_current_buffer \
  (yy_buffer_stack != 0 ? yy_buffer_stack[yy_buffer_stack_top] : 0)
#endif

Includable_lexer::Includable_lexer ()
{
#ifdef HAVE_FLEXLEXER_YY_CURRENT_BUFFER
  yy_current_buffer = 0;
#endif
  allow_includes_b_ = true;
}

/** Set the new input file to NAME, remember old file.  */
void
Includable_lexer::new_input (String name, Sources *sources)
{
  if (!allow_includes_b_)
    {
      LexerError (_ ("include files are not allowed").to_str0 ());
      return;
    }
  
  Source_file *file = sources->get_file (name);
  if (!file)
    {
      String msg = _f ("can't find file: `%s'", name);
      msg += "\n";
      msg += _f ("(search path: `%s')",
		 sources->path_->to_string ().to_str0 ());
      msg += "\n";
      LexerError (msg.to_str0 ());
      return;
    }
  filename_strings_.push (file->name_string ());

  char_count_stack_.push (0);
  if (yy_current_buffer)
    state_stack_.push (yy_current_buffer);

  if (verbose_global_b)
    progress_indication (String ("[") + name);
	
  include_stack_.push (file);

  /* Ugh. We'd want to create a buffer from the bytes directly.

    Whoops.  The size argument to yy_create_buffer is not the
    filelength but a BUFFERSIZE.  Maybe this is why reading stdin fucks up.  */
  yy_switch_to_buffer (yy_create_buffer (file->get_istream (), YY_BUF_SIZE));
}

void
Includable_lexer::new_input (String name, String data, Sources *sources)
{
  Source_file *file = new Source_file (name, data);
  sources->add (file);
  filename_strings_.push (name);

  char_count_stack_.push (0);
  if (yy_current_buffer)
    state_stack_.push (yy_current_buffer);

  if (verbose_global_b)
    progress_indication (String ("[") + name);
  include_stack_.push (file);

  yy_switch_to_buffer (yy_create_buffer (file->get_istream (), YY_BUF_SIZE));
}

/** pop the inputstack.  conceptually this is a destructor, but it
  does not destruct the Source_file that Includable_lexer::new_input
  creates.  */
bool
Includable_lexer::close_input ()
{
  include_stack_.pop ();
  char_count_stack_.pop ();
  if (verbose_global_b)
    progress_indication ("]");
  yy_delete_buffer (yy_current_buffer);
#ifdef HAVE_FLEXLEXER_YY_CURRENT_BUFFER  
  yy_current_buffer = 0;
#endif  
  if (state_stack_.is_empty ())
    {
#ifdef HAVE_FLEXLEXER_YY_CURRENT_BUFFER  
      yy_current_buffer = 0;
#endif  
      return false;
    }
  yy_switch_to_buffer (state_stack_.pop ());
  return true;
}

char const*
Includable_lexer::here_str0 () const
{
  if (include_stack_.is_empty ())
    return 0;
  return include_stack_.top ()->to_str0 () + char_count_stack_.top ();
}

Includable_lexer::~Includable_lexer ()
{
  while (!include_stack_.is_empty ())
    {
      close_input ();
    }
}
/**
  Since we don't create the buffer state from the bytes directly, we
  don't know about the location of the lexer. Add this as a
  YY_USER_ACTION */
void
Includable_lexer::add_lexed_char (int count)
{
  char_count_stack_.top () += count;
}

Source_file*
Includable_lexer::get_source_file () const
{
  if (include_stack_.is_empty ())
    return 0;
  else
    return include_stack_.top ();
}
