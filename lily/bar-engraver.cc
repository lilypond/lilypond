/*
  bar-engraver.cc -- implement Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "bar-line.hh"
#include "context.hh"
#include "score-engraver.hh"
#include "warn.hh"
#include "item.hh"

#include "translator.icc"

/*
  generate bars. Either user ("|:"), or default (new measure)
*/
class Bar_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Bar_engraver);
  void request_bar (string type_string);

protected:
  virtual void finalize ();
  void stop_translation_timestep ();
  void process_acknowledged ();

private:
  void typeset_bar ();
  void create_bar ();

  Item *bar_;
};

Bar_engraver::Bar_engraver ()
{
  bar_ = 0;
}

void
Bar_engraver::create_bar ()
{
  if (!bar_)
    {
      bar_ = make_item ("BarLine", SCM_EOL);
      SCM gl = get_property ("whichBar");
      if (scm_equal_p (gl, bar_->get_property ("glyph")) != SCM_BOOL_T)
	bar_->set_property ("glyph", gl);
    }
}

void
Bar_engraver::finalize ()
{
  typeset_bar ();
}

/*
  Bar_engraver should come *after* any engravers that
  modify whichBar

  This is a little hairy : whichBar may be set by
  Repeat_acknowledge_engraver::process_music, which is at score
  context. This means that grobs could should be created after
  process_music. We do stuff process_acknowledged (), just to be
  on the safe side.
*/

void
Bar_engraver::process_acknowledged ()
{
  if (!bar_ && scm_is_string (get_property ("whichBar")))
    create_bar ();
}

void
Bar_engraver::typeset_bar ()
{
  bar_ = 0;
}

/*
  lines may only be broken if there is a barline in all staves
*/
void
Bar_engraver::stop_translation_timestep ()
{
  if (!bar_)
    context ()->get_score_context ()->set_property ("forbidBreak", SCM_BOOL_T);
  else
    typeset_bar ();
}

ADD_TRANSLATOR (Bar_engraver,
		/* doc */ "Create barlines. This engraver is controlled through the "
		"@code{whichBar} property. If it has no bar line to create, it will forbid a linebreak at this point",
		/* create */ "BarLine",
		/* read */ "whichBar",
		/* write */ "forbidBreak");
