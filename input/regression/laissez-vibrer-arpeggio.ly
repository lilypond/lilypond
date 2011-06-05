\header {

  texidoc = "
l.v. ties should not collide with arpeggio indications.
"
}

\version "2.14.0" % regression: 2.10.33 and 2.8.8 are ok

{
 <e'>\laissezVibrer <f  f'> \arpeggio
 <e'>\laissezVibrer <g  f'> \arpeggio \mark "Ties should not collide"
 <e'>\laissezVibrer <a  f'> \arpeggio
 <e'>\laissezVibrer <b  f'> \arpeggio
 }
