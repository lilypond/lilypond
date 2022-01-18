#!@TARGET_PYTHON@

# Copyright (c) 2006--2022 Brailcom, o.p.s.
#
# Author: Milan Zamazal <pdm@brailcom.org>
#
# This file is part of LilyPond, the GNU music typesetter.
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


import codecs
import optparse
import os
import subprocess
import sys
import tempfile

"""
@relocate-preamble@
"""


FESTIVAL_COMMAND = ['festival', '--pipe']
VOICE_CODINGS = {'voice_czech_ph': 'iso-8859-2'}

_USAGE = """lilysong [-p PLAY-PROGRAM] FILE.xml [LANGUAGE-CODE-OR-VOICE [SPEEDUP]]
       lilysong FILE.ly [LANGUAGE-CODE-OR-VOICE]
       lilysong --list-voices
       lilysong --list-languages
"""


def usage():
    print('Usage:', _USAGE)
    sys.exit(2)


def process_options(args):
    parser = optparse.OptionParser(usage=_USAGE, version="@TOPLEVEL_VERSION@")
    parser.add_option('', '--list-voices', action='store_true', dest='list_voices',
                      help="list available Festival voices")
    parser.add_option('', '--list-languages', action='store_true', dest='list_languages',
                      help="list available Festival languages")
    parser.add_option('-p', '--play-program', metavar='PROGRAM',
                      action='store', type='string', dest='play_program',
                      help="use PROGRAM to play song immediately")
    options, args = parser.parse_args(args)
    return options, args


def call_festival(scheme_code):
    p = subprocess.Popen(FESTIVAL_COMMAND, stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE, close_fds=True)
    p.stdin.write(scheme_code)
    p.stdin.close()
    answer = ''
    while True:
        process_output = p.stdout.read()
        if not process_output:
            break
        answer = answer + process_output
    return answer


def select_voice(language_or_voice):
    if language_or_voice[:6] == 'voice_':
        voice = language_or_voice
    else:
        voice = call_festival('''
(let ((candidates '()))
  (mapcar (lambda (v)
            (if (eq (cadr (assoc 'language (cadr (voice.description v)))) '%s)
                (set! candidates (cons v candidates))))
          (append (voice.list) (mapcar car Voice_descriptions)))
  (if candidates
      (format t "voice_%%s" (car candidates))
      (format t "nil")))
''' % (language_or_voice,))
        if voice == 'nil':
            voice = None
    return voice


def list_voices():
    print(call_festival('''
(let ((voices (voice.list))
      (print-voice (lambda (v) (format t "voice_%s\n" v))))
  (mapcar print-voice voices)
  (mapcar (lambda (v) (if (not (member v voices)) (print-voice v)))
          (mapcar car Voice_descriptions)))
'''))


def list_languages():
    print(call_festival('''
(let ((languages '()))
  (let ((voices (voice.list))
        (print-language (lambda (v)
                          (let ((language (cadr (assoc 'language (cadr (voice.description v))))))
                            (if (and language (not (member language languages)))
                                (begin
                                  (set! languages (cons language languages))
                                  (print language)))))))
    (mapcar print-language voices)
    (mapcar (lambda (v) (if (not (member v voices)) (print-language v)))
            (mapcar car Voice_descriptions))))
'''))


def process_xml_file(file_name, voice, speedup, play_program):
    if speedup == 1:
        speedup = None
    coding = (VOICE_CODINGS.get(voice) or 'iso-8859-1')
    _, xml_temp_file = tempfile.mkstemp('.xml')
    try:
        # recode the XML file
        recodep = (coding != 'utf-8')
        if recodep:
            decode = codecs.getdecoder('utf-8')
            encode = codecs.getencoder(coding)
        input = open(file_name, encoding='utf-8')
        output = open(xml_temp_file, 'w', encoding='utf-8')
        while True:
            data = input.read()
            if not data:
                break
            if recodep:
                data = encode(decode(data)[0])[0]
            output.write(data)
        output.close()
        # synthesize
        wav_file = file_name[:-3] + 'wav'
        if speedup:
            _, wav_temp_file = tempfile.mkstemp('.wav')
        else:
            wav_temp_file = wav_file
        try:
            print("text2wave -eval '(%s)' -mode singing '%s' -o '%s'" %
                  (voice, xml_temp_file, wav_temp_file,))
            result = os.system("text2wave -eval '(%s)' -mode singing '%s' -o '%s'" %
                               (voice, xml_temp_file, wav_temp_file,))
            if result:
                sys.stdout.write("Festival processing failed.\n")
                return
            if speedup:
                result = os.system("sox '%s' '%s' speed '%f'" %
                                   (wav_temp_file, wav_file, speedup,))
                if result:
                    sys.stdout.write("Festival processing failed.\n")
                    return
        finally:
            if speedup:
                try:
                    os.delete(wav_temp_file)
                except OSError:
                    pass
        sys.stdout.write("%s created.\n" % (wav_file,))
        # play
        if play_program:
            os.system("%s '%s' >/dev/null" % (play_program, wav_file,))
    finally:
        try:
            os.delete(xml_temp_file)
        except OSError:
            pass


def process_ly_file(file_name, voice):
    result = os.system("lilypond '%s'" % (file_name,))
    if result:
        return
    xml_file = None
    for f in os.listdir(os.path.dirname(file_name) or '.'):
        if (f[-4:] == '.xml' and
                (not xml_file or os.stat.st_mtime(f) > os.stat.st_mtime(xml_file))):
            xml_file = f
    if xml_file:
        process_xml_file(xml_file, voice, None, None)
    else:
        sys.stderr.write("No XML file found\n")


def go():
    options, args = process_options(sys.argv[1:])
    if options.list_voices:
        list_voices()
    elif options.list_languages:
        list_languages()
    else:
        arglen = len(args)
        if arglen < 1:
            usage()
        file_name = args[0]
        if arglen > 1:
            language_or_voice = args[1]
            voice = select_voice(language_or_voice)
        else:
            voice = None
        if file_name[-3:] == '.ly':
            if arglen > 2:
                usage()
            process_ly_file(file_name, voice)
        else:
            if arglen > 3:
                usage()
            elif arglen == 3:
                try:
                    speedup = float(args[2])
                except ValueError:
                    usage()
            else:
                speedup = None
            process_xml_file(file_name, voice, speedup, options.play_program)


if __name__ == '__main__':
    go()
