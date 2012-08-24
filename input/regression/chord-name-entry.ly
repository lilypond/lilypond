\version "2.16.0"


\header {

    texidoc = "Chords can be produced with the chordname entry 
code (@code{\\chordmode} mode), using a pitch and a suffix. Here,
the suffixes are printed below pitches.
"

}

{ \context Voice \chordmode {
    c1_"(nothing)"
    c:7_":7"
    c:m_":m"
    c:m7_":m7"
    c:aug_":aug"
    c:maj7_":maj7"
    c:dim_":dim"
    c:dim7_":dim7"
    c:sus4_":sus4"
    c:sus2_":sus2"
    c:6_":6"
    c:m6_":m6"
    c:7sus4_":7sus4"
    c:3-_":3-"
    c:3+_":3+"
    c:5+.3-_":5+.3-"
    c:7_":7"
    c:9_":9"
    c:11_":11"
    c:13_":13"
    c:m13_":m13"
    c:7^5_":7\\^{~}5"
    c^3_"\\^{~}3"
    c/g_"/g"
    c/gis_"/gis"
    c/a_"/a"
    c/+f_"/+f"
    c/+g_"/+g"
}
}
