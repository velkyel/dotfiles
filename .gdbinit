set history filename ~/.gdb_history
set history save
set print pretty on
tui enable

python
import sys
import os
sys.path.insert(0, os.path.expanduser('~/dotfiles/'))
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers(None)
end
