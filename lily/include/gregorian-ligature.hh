/*
  gregorian-ligature.hh -- part of GNU LilyPond

  source file of the GNU LilyPond music typesetter
  
  (c) 2003 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef GREGORIAN_LIGATURE_HH
#define GREGORIAN_LIGATURE_HH

/*
 * Head prefixes: these bit-mask constants are used to represent
 * attributes immediately derived from user input (e.g. by the user
 * setting a gregorian ligature grob property or using the "\~"
 * keyword).  If the according bit of the head prefix value is set,
 * the attribute applies for this head.  The binary opereator "\~" is
 * marked only upon the second head (i.e. the note that comes after
 * the operator).
 */
#define VIRGA        0x0001 // attribute "\virga"
#define STROPHA      0x0002 // attribute "\stropha"
#define INCLINATUM   0x0004 // attribute "\inclinatum"
#define AUCTUM       0x0008 // attribute "\auctum"
#define DESCENDENS   0x0010 // attribute "\descendens"
#define ASCENDENS    0x0020 // attribute "\ascendens"
#define ORISCUS      0x0040 // attribute "\oriscus"
#define QUILISMA     0x0080 // attribute "\quilisma"
#define DEMINUTUM    0x0100 // attribute "\deminutum"
#define SEMIVOCALIS  0x0100 // attribute "\semivocalis"
#define CAVUM        0x0200 // attribute "\cavum"
#define LINEA        0x0400 // attribute "\linea"
#define PES_OR_FLEXA 0x0800 // operator "\~"

/*
 * Ligature context info: these attributes are derived from the head
 * prefixes by considering the current and the following head.
 */
#define PES_LOWER    0x0001 // this is a head before "\~" in an ascending melody
#define PES_UPPER    0x0002 // this is a head after "\~" in an ascending melody
#define FLEXA_LEFT   0x0004 // this is a head before "\~" in a descending melody
#define FLEXA_RIGHT  0x0008 // this is a head after "\~" in a descending melody

#endif /* GREGORIAN_LIGATURE_HH */
