\version "2.25.0"

\header {
  texidoc = "When a markup command takes a music argument, it
sets the default duration for the following music, whereas a
duration argument does not."
}

\markup \rhythm { 8 8 }

%% Expect { c'8 }
{ c' }

\markup \rhythm { 16 }

%% Expect { c'16 }
{ c' }

\markup \note { 8 } #UP

%% Still { c'16 }
{ c' }
