\header {

texidoc="
gives

@example
      |    |
      |    |
 +--  |  --+
 +----+----+
 |
 |

instead of the desired

      |    |
      |    |
      |  --+
 +----+----+
 +--
 |
 |

@end example
"

}

\score {
  \notes\relative c' {
     c16 c''8 c16
     
     % it's very helpful to have this one too,
     % because a fix is likely to break

     c,, c'' c,, cc
  }
  \paper { linewidth = -1 }
}
	  