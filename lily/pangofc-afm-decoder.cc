/*
  pango-afm-decoder.c -- AFM fontencoding for Pango fontconfig

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

#include "config.h"
#if HAVE_PANGO_FC_FONT_MAP_ADD_DECODER_FIND_FUNC

/* Need to access to PangoFcFont.full_pattern.  */
#define PANGO_ENABLE_BACKEND

#include <pango/pango-font.h>
#include <pango/pangoxft.h>
#include <pango/pangofc-font.h>
#include <gdk/gdkx.h>
#include <X11/Xft/Xft.h>

#include "pangofc-afm-decoder.hh"

#ifdef DEBUG_PANGO_AFM
#include <stdio.h>
#define dprintf(args...) fprintf (stderr, args)
#else
#define dprintf(args...)
#endif

struct _PangoFcAfmDecoderPrivate
{
  GString encoding[256];
  //GString file_name;
  char const *file_name;
  PangoFcFont *fc_font;
};

static void pango_fc_afm_decoder_init (PangoFcAfmDecoder *fontmap);
static void pango_fc_afm_decoder_class_init (PangoFcAfmDecoderClass *clss);
static void pango_fc_afm_decoder_finalize (GObject *object);

static FcCharSet *pango_fc_afm_get_charset (PangoFcFont *fcfont);
static PangoGlyph pango_fc_afm_get_glyph (PangoFcFont *fcfont, guint32 wc);
static void pango_fc_afm_decoder_set_file_name (PangoFcAfmDecoder *self, char const *file_name);

static PangoFcDecoderClass *parent_class;

#if 0
/* ugly warning */
G_DEFINE_TYPE (PangoFcAfmDecoder, pango_fc_afm_decoder, PANGO_TYPE_FC_DECODER);
#else
GType
pango_fc_afm_decoder_get_type (void)
{
  static GType object_type = 0;

  if (!object_type)
    {
      static const GTypeInfo object_info =
      {
        sizeof (PangoFcAfmDecoderClass),
        (GBaseInitFunc) 0,
        (GBaseFinalizeFunc) 0,
        (GClassInitFunc) pango_fc_afm_decoder_class_init,
        0,           /* class_finalize */
        0,           /* class_data */
        sizeof (PangoFcAfmDecoder),
        0,              /* n_preallocs */
        (GInstanceInitFunc) pango_fc_afm_decoder_init,
	0, /* value table */
      };
      
      object_type = g_type_register_static (PANGO_TYPE_FC_DECODER,
                                            "PangoFcAfmDecoder",
                                            &object_info, (GTypeFlags)0);
    }
  
  return object_type;
}
#endif

static void 
pango_fc_afm_decoder_init (PangoFcAfmDecoder *fcafmdecoder)
{
  PangoFcAfmDecoderPrivate *priv = fcafmdecoder->priv;
  priv = fcafmdecoder->priv
    = G_TYPE_INSTANCE_GET_PRIVATE (fcafmdecoder,
				   PANGO_TYPE_FC_AFM_DECODER,
				   PangoFcAfmDecoderPrivate);
  /*
    init members
   */
}

static void
pango_fc_afm_decoder_class_init (PangoFcAfmDecoderClass *clss)
{
  GObjectClass *object_class = G_OBJECT_CLASS (clss);
  object_class->finalize = pango_fc_afm_decoder_finalize;
  g_type_class_add_private (object_class, sizeof (PangoFcAfmDecoderPrivate));

  PangoFcDecoderClass *parent_class = PANGO_FC_DECODER_CLASS (clss);
  parent_class->get_charset = pango_fc_afm_get_charset;
  parent_class->get_glyph = pango_fc_afm_get_glyph;
}

static void
pango_fc_afm_decoder_finalize (GObject *object)
{
#if 0  
  PangoFcAfmDecoder *fcafmdecoder = PANGO_FC_AFM_DECODER (object);
  PangoFcAfmDecoderPrivate *priv = fcafmdecoder->priv;
#endif

  /*
    destroy members
   */
  G_OBJECT_CLASS (parent_class)->finalize (object);
}

static FcCharSet *
pango_fc_afm_get_charset (PangoFcFont *fcfont)
{
  //dprintf ("get charset: %s\n", fcfont->font_pattern);
  dprintf ("get charset: \n");
#if 0  
  FcCharSet *charset = 0;
  FcPatternGetCharSet (fcfont->font_pattern, FC_CHARSET, 0, &charset);
#else
  /* Return plain, undecoded charset.
     TODO:
       - actually read AFM?
       - caching?
       - PUA mapping ? */
  (void) fcfont;
  int i;
  FcChar32 chr = 0;
  FcCharSet *charset = FcCharSetCreate ();
  for (i = 0; i < 256; i++)
    if (!FcCharSetAddChar (charset, chr++))
      return 0;
#endif  
  return charset;
}

static PangoGlyph
pango_fc_afm_get_glyph (PangoFcFont *fcfont, guint32 wc)
{
#if 0
  XftFont *xft_font;
  xft_font = XftFontOpenPattern (GDK_DISPLAY (),
				 FcPatternDuplicate (fcfont->font_pattern));
  PangoGlyph g = XftCharIndex (0, xft_font, wc);
  dprintf ("get glyph! 0x%x --> 0x%x\n", wc, (unsigned)g);
#else
  /* TODO:
       - PUA mapping?
       
     Shortcut PUA mapping/AFM reading: The Feta charsets are encoded
     without any gaps, starting at 0x21.  *grin*  */
  (void) fcfont;
  return wc - 0x21;
#endif  
}

static void
pango_fc_afm_decoder_set_file_name (PangoFcAfmDecoder *self,
				    char const *file_name)
{
  self->priv->file_name = file_name;
}

PangoFcAfmDecoder *
pango_fc_afm_decoder_new (void)
{
  return PANGO_FC_AFM_DECODER (g_object_new (PANGO_TYPE_FC_AFM_DECODER, 0));
}

PangoFcDecoder *
pango_fc_afm_find_decoder (FcPattern *pattern, gpointer user_data)
{
  FcChar8 *family_name;
  
  if (FcPatternGetString (pattern, FC_FAMILY, 0, &family_name)
      == FcResultMatch)
    {
      dprintf ("Family name: %s\n", family_name);
      dprintf ("user_data: %s\n", (char const*) user_data);

      if (!strcasecmp ((char const*) family_name, (char const*) user_data))
	{
	  PangoFcAfmDecoder *afm = pango_fc_afm_decoder_new ();
	  char const *file_name = "feta20.afm";
	  pango_fc_afm_decoder_set_file_name (afm, file_name);
	  dprintf ("Adding decoder: %s\n", file_name);
	  return PANGO_FC_DECODER (afm);
	}
    }
  return 0;
}

void
pango_fc_afm_add_decoder (char const *family_name)
{
  PangoFcFontMap *fc_map
    = PANGO_FC_FONT_MAP (pango_xft_get_font_map (GDK_DISPLAY (), 0));

  pango_fc_font_map_add_decoder_find_func (fc_map, pango_fc_afm_find_decoder,
					   (gpointer)family_name, 0);
}

#ifdef PANGO_FC_AFM_DECODER_TEST

#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>
#include <pango/pangoft2.h>
#include <pango/pangox.h>
#include <pango/pangoxft.h>

#include "pangofc-afm-decoder.h"

#define CANVAS_WIDTH 600
#define CANVAS_HEIGHT 300

static void
exit_clicked (GtkWidget *widget, gpointer data)
{
  gtk_widget_destroy (GTK_WIDGET (data));
  gtk_main_quit ();
}

static gint
delete_event (GtkWidget *widget, GdkEvent *event, gpointer data)
{
  gtk_widget_destroy (widget);
  gtk_main_quit ();
  return TRUE;
}

GnomeCanvas *
make_canvas (int width, int height)
{
  GtkWidget *window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
  GtkWidget *canvas = gnome_canvas_new ();
  GtkWidget *hbox = gtk_hbox_new (TRUE, 0);
  GtkWidget *button = gtk_button_new_with_label ("Exit");

  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_widget_set_size_request (canvas, width, height);
  gtk_box_pack_start (GTK_BOX (vbox), canvas, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (hbox), button, TRUE, TRUE, 0);

  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      (GtkSignalFunc) exit_clicked, window);
  
  gtk_signal_connect (GTK_OBJECT (window), "delete_event",
		      (GtkSignalFunc) delete_event, 0);

  gtk_widget_show_all (window);
  return GNOME_CANVAS (canvas);
}

#define gnome_canvas_text(x, y, font, text) \
  gnome_canvas_item_new (root, text_item, "x", x, "y", y, "font", font, \
			 "text", text, "anchor", GTK_ANCHOR_SOUTH_WEST, \
			 "fill_color", "black", 0)

int
main (int argc, char **argv)
{
  gtk_init (&argc, &argv);
  GnomeCanvas *canvas = make_canvas (CANVAS_WIDTH, CANVAS_HEIGHT);

  /* Register GNU LilyPond FETA AFM decoders.  */
  pango_fc_afm_add_decoder ("lilypond-feta");
  pango_fc_afm_add_decoder ("lilypond-braces");
  pango_fc_afm_add_decoder ("lilypond-dyn");
  pango_fc_afm_add_decoder ("lilypond-parmesan");

  int text_item = gnome_canvas_text_get_type ();
  GnomeCanvasGroup *root = gnome_canvas_root (canvas);
  char const *g_clef_utf8 = "\302\220";

  gnome_canvas_text (45.0, 122.5, "LilyPond-feta-nummer 16", "3");
  gnome_canvas_text (45.0, 142.5, "LilyPond-feta-nummer, r 16", "4");
  gnome_canvas_text (10.0, 142.0, "LilyPond-feta, 32", g_clef_utf8);

  gtk_main ();
  return 0;
}
#endif /* PANGO_FC_AFM_DECODER_TEST */

#endif /* HAVE_PANGO_FC_FONT_MAP_ADD_DECODER_FIND_FUNC */
