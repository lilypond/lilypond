#!/bin/sh
# make-examples

examples="twinkle-pop wtk1-fugue2 standchen-16 standchen-20 wtk1-prelude1 toccata-fuga-E prelude-cello menuetto-cello menuetto-alto cadenza gallina twinkle collisions font font20 scales rhythm multi spacing beams sleur stem"
pss=$(echo $examples | sed "s/[^ ][^ ]*/out\\/&.ps/g")

# cd ~/lelie/current/Documentation/out
cd ~/lelie/current/input
make -C ../Documentation $pss
