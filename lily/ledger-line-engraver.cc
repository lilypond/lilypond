/*
  ledger-line-engraver.cc -- implement Ledger_line_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "engraver.hh"
#include "staff-symbol.hh"

class Ledger_line_engraver : public Engraver
{
  Spanner *span_;

public:
  TRANSLATOR_DECLARATIONS (Ledger_line_engraver);

protected:
  virtual void finalize ();
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);

  void start_spanner ();
  void stop_spanner ();
};

Ledger_line_engraver::Ledger_line_engraver ()
{
  span_ = 0;
}

void
Ledger_line_engraver::start_spanner ()
{
  assert (!span_);
  span_ = make_spanner ("LedgerLineSpanner", SCM_EOL);

  span_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
}

void
Ledger_line_engraver::process_music ()
{
  if (!span_)
    start_spanner ();
}

void
Ledger_line_engraver::finalize ()
{
  stop_spanner();
}

void
Ledger_line_engraver::stop_spanner ()
{
  if (span_)
    {
      span_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
      span_ = 0;
    }
}

void
Ledger_line_engraver::acknowledge_grob (Grob_info s)
{
  if (Staff_symbol::has_interface (s.grob ()))
    {
      Spanner *sym = dynamic_cast<Spanner*> (s.grob ());

      if (!span_
	  || span_->get_bound (LEFT) != sym->get_bound (LEFT))
	{
	  stop_spanner ();
	  start_spanner ();
	}
    }
  else if (span_)
    {
      if (!to_boolean (s.grob ()->get_property ("no-ledgers")))
	Pointer_group_interface::add_grob (span_, ly_symbol2scm ("note-heads"),
					   s.grob ());
    }
}

ADD_TRANSLATOR (Ledger_line_engraver,
		"Creates the spanner to draw ledger lines, and notices objects that need ledger lines",
		/* creats*/ "LedgerLineSpanner",
		/* accepts */ "",
		/* acks  */ "staff-symbol-interface ledgered-interface", // ledgered-interface? 
		/* reads */ "",
		/* write */ "")
