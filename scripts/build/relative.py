# Make a path relative to the current working directory. Equivalent to
# realpath --relative-to=. except that realpath isn't available on
# macOS/BSD.

import os
import sys

print(os.path.relpath(sys.argv[1]))
