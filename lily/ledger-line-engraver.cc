/* 
  ledger-line-engraver.cc --  implement Ledger_line_engraver =
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
  
*/

#include "group-interface.hh"
#include "spanner.hh"
#include "engraver.hh"

class Ledger_line_engraver : public Engraver
{
  Spanner * span_;

public:
  TRANSLATOR_DECLARATIONS (Ledger_line_engraver);

protected:
  virtual void finalize ();
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
};

Ledger_line_engraver::Ledger_line_engraver()
{
  span_ = 0;
}

void
Ledger_line_engraver::process_music ()
{
  if (!span_)
    {
      span_ = make_spanner("LedgerLineSpanner", SCM_EOL);
  
      span_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}

void
Ledger_line_engraver::finalize ()
{
  if (span_)
    span_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
}


void
Ledger_line_engraver::acknowledge_grob (Grob_info s)
{
  if (!to_boolean (s.grob_->get_property ("no-ledgers")))
    Pointer_group_interface::add_grob (span_, ly_symbol2scm ("note-heads"),
				       s.grob_);
}
ADD_TRANSLATOR (Ledger_line_engraver,
		   "Creates the spanner to draw ledger lines, and notices objects that need ledger lines",
		   /* creats*/       "LedgerLineSpanner",
		   /* accepts */     "",
		   /* acks  */      "ledgered-interface", // ledgered-interface? 
		   /* reads */       "",
		   /* write */       "")
