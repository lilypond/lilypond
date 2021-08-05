"""Library package to build release binaries of LilyPond"""

from .config import Config
from .dependencies import all_dependencies
from .lilypond import LilyPond, LilyPondPackager
