/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "engraver-group.hh"

#include "item.hh"
#include "international.hh"
#include "misc.hh"
#include "time-signature.hh"
#include "warn.hh"

/**
   generate time_signatures.
*/
class Time_signature_engraver : public Engraver
{
  Item *time_signature_;
  SCM last_time_fraction_;

protected:
  virtual void derived_mark () const;
  void stop_translation_timestep ();
  void process_music ();
public:
  TRANSLATOR_DECLARATIONS (Time_signature_engraver);
};

void
Time_signature_engraver::derived_mark () const
{
  scm_gc_mark (last_time_fraction_);
}

Time_signature_engraver::Time_signature_engraver ()
{
  time_signature_ = 0;
  last_time_fraction_ = SCM_BOOL_F;
}

void
Time_signature_engraver::process_music ()
{
  /*
    not rigorously safe, since the value might get GC'd and
    reallocated in the same spot */
  SCM fr = get_property ("timeSignatureFraction");
  if (!time_signature_
      && last_time_fraction_ != fr
      && scm_is_pair (fr))
    {
      int den = scm_to_int (scm_cdr (fr));
      if (den != (1 << intlog2 (den)))
	{
	  /*
	    Todo: should make typecheck?

	    OTOH, Tristan Keuris writes 8/20 in his Intermezzi.
	  */
	  warning (_f ("strange time signature found: %d/%d",
		       int (scm_to_int (scm_car (fr))),
		       den));
	}

      time_signature_ = make_item ("TimeSignature", SCM_EOL);
      time_signature_->set_property ("fraction", fr);

      if (last_time_fraction_ == SCM_BOOL_F)
	time_signature_->set_property ("break-visibility",
				       get_property ("implicitTimeSignatureVisibility"));
      
      last_time_fraction_ = fr;
    }
}

void
Time_signature_engraver::stop_translation_timestep ()
{
  time_signature_ = 0;
}

#include "translator.icc"

ADD_TRANSLATOR (Time_signature_engraver,
		/* doc */
		"Create a @ref{TimeSignature} whenever"
		" @code{timeSignatureFraction} changes.",

		/* create */
		"TimeSignature ",
		
		/* read */
		"implicitTimeSignatureVisibility "
		"timeSignatureFraction ",

		/* write */
		""
		);
