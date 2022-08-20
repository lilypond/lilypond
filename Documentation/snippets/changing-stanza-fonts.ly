%% DO NOT EDIT this file manually; it was automatically
%% generated from the LilyPond Snippet Repository
%% (http://lsr.di.unimi.it).
%%
%% Make any changes in the LSR itself, or in
%% `Documentation/snippets/new/`, then run
%% `scripts/auxiliar/makelsr.pl`.
%%
%% This file is in the public domain.

\version "2.23.12"

\header {
  lsrtags = "really-simple, vocal-music"

  texidoc = "
Fonts can be changed independently for each stanza, including the font
used for printing the stanza number.
"

  doctitle = "Changing stanza fonts"
} % begin verbatim


%{
You may have to install additional fonts.

Red Hat Fedora

  dejavu-fonts-all

Debian GNU/Linux, Ubuntu

  fonts-dejavu-core
  fonts-dejavu-extra
%}

\relative c'' {
  \time 3/4
  g2 e4
  a2 f4
  g2.
}
\addlyrics {
  \set stanza = #"1. "
  Hi, my name is Bert.
}
\addlyrics {
  \override StanzaNumber.font-name = #"DejaVu Sans"
  \set stanza = #"2. "
  \override LyricText.font-family = #'typewriter
  Oh, ché -- ri, je t'aime
}
