/*
  gregorian-ligature.cc -- implement Gregorian_ligature

  source file of the GNU LilyPond music typesetter

  (c) 2003--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "gregorian-ligature.hh"

#include "grob.hh"

void check_prefix (string name, int mask, int prefix_set, string *str)
{
  if (prefix_set & mask)
    {
      if (!str->empty ())
	*str += ", ";
      *str += name;
    }
}

string
Gregorian_ligature::prefixes_to_str (Grob *primitive)
{
  string str;
  int prefix_set
    = scm_to_int (primitive->get_property ("prefix-set"));
  check_prefix ("virga", VIRGA, prefix_set, &str);
  check_prefix ("stropha", STROPHA, prefix_set, &str);
  check_prefix ("inclinatum", INCLINATUM, prefix_set, &str);
  check_prefix ("auctum", AUCTUM, prefix_set, &str);
  check_prefix ("descendens", DESCENDENS, prefix_set, &str);
  check_prefix ("ascendens", ASCENDENS, prefix_set, &str);
  check_prefix ("oriscus", ORISCUS, prefix_set, &str);
  check_prefix ("quilisma", QUILISMA, prefix_set, &str);
  check_prefix ("deminutum", DEMINUTUM, prefix_set, &str);
  check_prefix ("cavum", CAVUM, prefix_set, &str);
  check_prefix ("linea", LINEA, prefix_set, &str);
  return str;
}

/*
  CHECK ME -- does prefix-set come from here ?

  In a way, yes.  Actually, prefix-set is a property that is written
  by code of GregorianLigatureEngraver that is virtually invoked by a
  subclass like VaticanaLigatureEngraver.  The property is lateron
  read by the associated item class, such as VaticanaLigature.--jr
*/
ADD_INTERFACE (Gregorian_ligature,
	       "A gregorian ligature.",

	       /* properties */
	       "virga "
	       "stropha "
	       "inclinatum "
	       "auctum "
	       "descendens "
	       "ascendens "
	       "oriscus "
	       "quilisma "
	       "deminutum "
	       "cavum "
	       "linea "
	       "pes-or-flexa "
	       "context-info "
	       "prefix-set "
	       );
