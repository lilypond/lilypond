/*
  includable-lexer.cc -- implement Includable_lexer

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "includable-lexer.hh"
#include "source-file.hh"
#include "source.hh"

#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE 16384
#endif

Includable_lexer::Includable_lexer ()
{
  yy_current_buffer = 0;
}

/** set the  new input to s, remember old file.
*/
void
Includable_lexer::new_input (String s, Sources  * global_sources)
{
  Source_file * sl = global_sources->get_file_l (s);
  if (!sl)
    {
      String msg =_ ("Can't find file `") + s+ "'";
      LexerError (msg.ch_C ());
      return;
    }


  char_count_stack_.push (0);
  if (yy_current_buffer)
    state_stack_.push (yy_current_buffer);
  cout << "[" << s<<flush;
  include_stack_.push (sl);

  /*
    ugh. We'd want to create a buffer from the bytes directly.

    Whoops. The size argument to yy_create_buffer is not the
    filelength but a BUFFERSIZE. Maybe this is why reading stdin fucks up.

    */
  yy_switch_to_buffer (yy_create_buffer (sl->istream_l (), YY_BUF_SIZE));
}

/** pop the inputstack.  conceptually this is a destructor, but it
  does not destruct the Source_file that Includable_lexer::new_input creates.  */
bool
Includable_lexer::close_input ()
{
  include_stack_.pop ();
  char_count_stack_.pop ();
  cout << "]"<<flush;
  yy_delete_buffer (yy_current_buffer);
  yy_current_buffer = 0;
  if (state_stack_.empty ())
    {
      return false;
    }
  else
      {
	yy_switch_to_buffer (state_stack_.pop ());
	return true;
      }
}

char const*
Includable_lexer::here_ch_C ()
{
  if (include_stack_.empty ())
    return 0;
  return include_stack_.top ()->ch_C () + char_count_stack_.top ();
}

Includable_lexer::~Includable_lexer ()
{
  while (!include_stack_.empty ())
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
Includable_lexer::source_file_l () const
{
  if (include_stack_.empty ())
    return 0;
  else
    return include_stack_.top ();
}
