/*   
  kpath.hh -- declare kpathsea functions.

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef KPATH_HH
#define KPATH_HH

class String;

String kpathsea_find_afm (char const *name);
String kpathsea_find_tfm (char const *name);
void initialize_kpathsea (char *av0);

#endif /* KPATH_HH */

