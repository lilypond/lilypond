#!/bin/sh


# set environment for LilyPond.  To be installed in /etc/profile.d/

LILYINCLUDE="@datadir@/ly"
MFINPUTS="@datadir@/mf:"${MFINPUTS:=":"}
TEXINPUTS="@datadir@/tex:"${TEXINPUTS:=":"}

# todo: GS stuff

export  LILYINCLUDE MFINPUTS TEXINPUTS

# echo  $LILYINCLUDE $MFINPUTS $TEXINPUTS


