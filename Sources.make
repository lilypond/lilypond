# Sources.make
# sourcefiles to be shipped. Also used for dependencies

hdr=bar.hh bar-reg.hh beam.hh\
	binary-source-file.hh\
	boxes.hh break.hh class-name.hh clef-reg.hh clef-item.hh\
	colhpos.hh  commandrequest.hh   \
	complex-walker.hh complex-staff.hh\
	const.hh debug.hh dimen.hh directional-spanner.hh\
	glob.hh grouping.hh headreg.hh idealspacing.hh\
	identifier.hh identparent.hh \
	input-music.hh input-score.hh input-staff.hh\
	input-file.hh\
	item.hh key.hh key-item.hh\
	key-reg.hh\
	keyword.hh leastsquares.hh lexer.hh linespace.hh \
	local-key-item.hh local-key-reg.hh lookup.hh \
	lyric-item.hh lyricstaff.hh lyricwalker.hh\
	main.hh  meter.hh meter-reg.hh\
	midi-def.hh midi-item.hh midi-output.hh midi-stream.hh\
	midi-walker.hh\
	misc.hh\
	molecule.hh moment.hh musicalrequest.hh\
	notehead.hh notename.hh offset.hh paper-def.hh\
	parseconstruct.hh pcol.hh proto.hh\
	pscore.hh pstaff.hh qlp.hh\
	qlpsolve.hh register.hh registergroup.hh reqtodo.hh \
	request.hh rest.hh score-column.hh score.hh\
	scoreline.hh score-walker.hh script.hh script-def.hh script-reg.hh \
	slur.hh slur-reg.hh source.hh source-file.hh\
	spanner.hh staff.hh\
	staff-elem.hh staff-elem-info.hh staffline.hh staffsym.hh stem-beam-reg.hh\
	staff-column.hh stem.hh staff-walker.hh symbol.hh symtable.hh\
	tex.hh text-def.hh \
	textitem.hh text-reg.hh textspanner.hh time-description.hh \
	tex-stream.hh voice.hh\
	voice-element.hh voice-regs.hh voice-group-regs.hh walkregs.hh

mycc=bar.cc bar-reg.cc beam.cc \
	binary-source-file.cc\
	boxes.cc break.cc calcideal.cc clef-reg.cc\
	clef-item.cc colhpos.cc  commandrequest.cc\
	complex-staff.cc complex-walker.cc \
	debug.cc dimen.cc\
	directional-spanner.cc\
	grouping.cc groupregs.cc headreg.cc\
	idealspacing.cc identifier.cc\
	input-music.cc input-score.cc\
	input-staff.cc\
	input-file.cc\
	item.cc key.cc key-item.cc \
	key-reg.cc keyword.cc\
	leastsquares.cc lexerinit.cc linespace.cc \
	local-key-item.cc local-key-reg.cc lookup.cc\
	lyric-item.cc lyricstaff.cc lyricwalker.cc\
	main.cc  meter.cc meter-reg.cc\
	midi-def.cc  midi-item.cc midi-output.cc midi-stream.cc\
	midi-walker.cc misc.cc molecule.cc mylexer.cc note.cc\
	notehead.cc  notename.cc\
	paper-def.cc pcol.cc pscore.cc pstaff.cc qlp.cc qlpsolve.cc\
	register.cc registergroup.cc request.cc rest.cc\
	score-column.cc score.cc\
	scoreline.cc scores.cc score-walker.cc script.cc\
	script-def.cc script-reg.cc slur.cc\
	slur-reg.cc source.cc source-file.cc\
	spanner.cc staff.cc\
	staff-elem.cc staffline.cc staffsym.cc\
	stem-beam-reg.cc staff-column.cc stem.cc\
	staff-elem-info.cc staff-walker.cc symbol.cc\
	symtable.cc tex.cc texbeam.cc\
	texslur.cc text-def.cc textitem.cc text-reg.cc textspanner.cc\
	time-description.cc tex-stream.cc voice.cc voiceelt.cc \
	voice-regs.cc voice-group-regs.cc\
	walkregs.cc warn.cc windhoos-suck-suck-suck-thank-you-cygnus.cc wordwrap.cc\
	template1.cc template2.cc template3.cc template4.cc\
	template5.cc template6.cc version.cc

# a bit of a hack to keep exec size under control.
stablecc=request.cc bar.cc boxes.cc break.cc  \
	item.cc keyword.cc leastsquares.cc \
	molecule.cc meter.cc\
	paper-def.cc parser.cc lexer.cc pstaff.cc qlp.cc qlpsolve.cc\
	template1.cc template2.cc template3.cc template4.cc\
	template5.cc template6.cc version.cc tex-stream.cc  tex.cc\
	voice.cc wordwrap.cc spanner.cc 


# m2m headers
#
mym2mhh=\
 duration.hh\
 lily-stream.hh\
 midi-event.hh\
 midi-main.hh\
 midi-score.hh\
 midi-track.hh\
 my-midi-lexer.hh\
 my-midi-parser.hh\
 track-column.hh\

#

# m2m shared headers
#
mym2msharedhh=\
 binary-source-file.hh\

#

# m2m source
#
mym2mcc=\
 duration.cc\
 lily-stream.cc\
 midi-event.cc\
 midi-main.cc\
 midi-score.cc\
 midi-template.cc\
 midi-track.cc\
 my-midi-lexer.cc\
 my-midi-parser.cc\
 track-column.cc\

#

# m2m shared source
#
mym2msharedcc=\
 binary-source-file.cc\
 input-file.cc\
 source-file.cc\
 source.cc\

# 

