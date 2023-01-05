# pylint: disable=C0111
# config.py
# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

import yt

# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config: ConfigAPI = config  # noqa: F821 pylint: disable=E0602,C0103
c: ConfigContainer = c  # noqa: F821 pylint: disable=E0602,C0103

# autoload yaml config (Bool)
config.load_autoconfig(True)

# user agent settings
agent ='Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0'
config.set('content.cookies.accept', 'all', 'chrome-devtools://*')
config.set('content.headers.user_agent', agent) 

# Set aliases for qutebrowser commands (Dict)
c.aliases = {'q' : 'quit', 'w' : 'session-save', 'wq' : 'quit --save'}

# Render all web contents using a dark theme  (Bool)
c.colors.webpage.darkmode.enabled = True

# use vertical tabs on the left (String)
c.tabs.position = "left"
# only show tabs if there is more than one
c.tabs.show = 'always'

# downloads dir
c.downloads.location.directory = '~/downloads'
# start settings
c.url.default_page = 'https://startpage.com'
c.url.start_pages = 'https://startpage.com'

c.url.searchengines = {
        'DEFAULT' : 'https://www.startpage.com/do/dsearch?query={}',
        'aw'      : 'https://wiki.archlinux.org/?search={}',
        'g'       : 'https://www.google.com/search/?q={}',
        'w'       : 'https://en.wikipedia.org/wiki/{}', 
        'yt'      : 'https://www.youtube.com/results?search_query={}',
        'so'      : 'https://stackoverflow.com/search?q={}',
        'gh'      : 'https://github.com/search?type=&q={}',
        'gs'      : 'https://scholar.google.com/scholar?hl=en&q={}',
        }

c.fonts.default_family = '"Anonymous Pro"'
c.fonts.default_size   = '12pt'

# Bindings for normal mode
config.bind(',m', 'spawn umpv {url}')
config.bind(',M', 'hint links spawn umpv {hint-url}')
config.bind(';M', 'hint --rapid links spawn umpv {hint-url}')
# config.bind('t', 'set-cmd-text -s :open -t')
config.bind('xb', 'config-cycle statusbar.show always never')
config.bind('xt', 'config-cycle tabs.show always never')


# filter youtube ads if we don't want to use mpv for some reason
yt.main()
