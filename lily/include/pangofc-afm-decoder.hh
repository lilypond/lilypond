/*
  pangofc-afm-decoder.h -- 

  source file of the GNU LilyPond music typesetter

  Copyright (C) 2004  Jan Nieuwenhuizen <janneke@gnu.org>

  Note: in C and with explicit LPGL header for easier use with PANGO
  outside of LilyPond.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
 
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
 
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*/

#ifndef __PANGOFC_AFM_DECODER_H__
#define __PANGOFC_AFM_DECODER_H__

#include "config.h"
#ifdef HAVE_PANGO_FC_FONT_MAP_ADD_DECODER_FIND_FUNC

/* Note: in C and with explicit header for use outside of LilyPond.  */

#include <pango/pangofc-fontmap.h>

G_BEGIN_DECLS

#define PANGO_TYPE_FC_AFM_DECODER              (pango_fc_afm_decoder_get_type ())
#define PANGO_FC_AFM_DECODER(object)           (G_TYPE_CHECK_INSTANCE_CAST ((object), PANGO_TYPE_FC_AFM_DECODER, PangoFcAfmDecoder))
#define PANGO_IS_FC_AFM_DECODER(object)        (G_TYPE_CHECK_INSTANCE_TYPE ((object), PANGO_TYPE_FC_AFM_DECODER))

#define PANGO_FC_AFM_DECODER_CLASS(clss)      (G_TYPE_CHECK_CLASS_CAST ((clss), PANGO_TYPE_FC_AFM_DECODER, PangoFcAfmDecoderClass))
#define PANGO_IS_FC_AFM_DECODER_CLASS(clss)   (G_TYPE_CHECK_CLASS_TYPE ((clss), PANGO_TYPE_FC_AFM_DECODER))
#define PANGO_FC_AFM_DECODER_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), PANGO_TYPE_FC_AFM_DECODER, PangoFcAfmDecoderClass))

typedef struct _PangoFcAfmDecoder        PangoFcAfmDecoder;
typedef struct _PangoFcAfmDecoderClass   PangoFcAfmDecoderClass;
typedef struct _PangoFcAfmDecoderPrivate PangoFcAfmDecoderPrivate;


/**
 * PangoFcAfmDecoder:
 * 
 * #PangoFcAfmDecoder implements an PangoFcDecoder for fonts
 * with an afm mapping.
 **/
struct _PangoFcAfmDecoder
{
  PangoFcDecoder parent_instance;

  /*< private >*/
  PangoFcAfmDecoderPrivate *priv;
};

PangoFcAfmDecoder *pango_fc_afm_decoder_new (void);
PangoFcDecoder *pango_fc_afm_find_decoder (FcPattern *pattern, gpointer user_data);
void pango_fc_afm_add_decoder (char const *family_name);

/**
 * PangoFcAfmDecoderClass:
 * @read_afm: Read afm mapping for #fcfont.
 *
 * Class structure for #PangoFcAfmDecoder.
 **/
struct _PangoFcAfmDecoderClass
{
  /*< private >*/
  PangoFcDecoderClass parent_class;

  /*< public >*/
  void (*read_afm) (PangoFcAfmDecoder *self, PangoFcFont *fcfont);
};

G_END_DECLS

#endif /* HAVE_PANGO_FC_FONT_MAP_ADD_DECODER_FIND_FUNC */

#endif /* __PANGOFC_AFM_DECODER_H__ */
