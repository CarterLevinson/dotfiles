import XMonad

import Control.Monad

import Data.Maybe
import Data.Monoid

import qualified Data.Map as M

import System.IO
import System.Exit

import Graphics.X11

import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleWS 
import XMonad.Actions.DynamicProjects -- TODO
import XMonad.Actions.FindEmptyWorkspace -- TODO
import XMonad.Actions.GridSelect -- TODO: finish
import XMonad.Actions.Minimize
import XMonad.Actions.Sift
import XMonad.Actions.OnScreen
import XMonad.Actions.WindowMenu
import XMonad.Actions.MouseResize 

import qualified XMonad.Actions.Search as S  -- clean up
import qualified XMonad.Actions.Submap as SM -- add more submaps

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.RefocusLast (shiftRLWhen, isFloat)

-- import XMonad.Layout.Circle
import XMonad.Layout.Mosaic
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.ResizableThreeColumns
-- import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed

import XMonad.Layout.IndependentScreens
import XMonad.Layout.Magnifier 
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger

import qualified XMonad.Layout.ToggleLayouts as T  -- TODO
import qualified XMonad.Layout.MultiToggle as MT -- TODO

import XMonad.Prompt as P
import XMonad.Prompt.Input -- TODO: use input to write xmonad papis prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Layout
import XMonad.Prompt.Pass
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh
import XMonad.Prompt.Unicode
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad
import XMonad.Prompt.Zsh

import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Dzen as D
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Paste
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Scratchpad
import XMonad.Util.Ungrab
import XMonad.Util.WorkspaceCompare

--TODO: Mouse configs
--TODO: Project configs
--TODO: clean formattings
--TODO: change default workspace bindings

-- workspaces

-------------------------------------------------------------------------------

xmWs = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)

currentScreen :: X ScreenId
currentScreen = gets ( W.screen . W.current . windowset)

spacesOnCurrentScreen :: WSType
spacesOnCurrentScreen = WSIs (isOnScreen <$> currentScreen)

-------------------------------------------------------------------------------

-- define grid selects 

guiGrid :: [(String,String)]
guiGrid = 
    [ ("Vimiv", "vimiv")
    , ("Spotify", "spotify")
    , ("Gimp", "gimp")
    , ("Inkscape","inkscape")
    , ("TradingView", "tradingview")
    ]

tuiGrid :: [(String,String)]
tuiGrid = 
    [ ("Ledger", "ledger")
    , ("Hledger", "hledger")
    , ("VisiData", "vd")
    , ("Cmus", "cmus")
    , ("Mpv", "mpv")
    , ("Papis", "papis")
    , ("WeeChat", "weechat")
    ]

utilGrid :: [(String,String)]
utilGrid =
    [ ("Htop", "htop")
    , ("Btop++", "btop")
    , ("S-tui", "s-tui")
    , ("Termshark", "termshark")
    , ("Ncdu", "ncdu")
    , ("Cfdisk", "cfdisk")
    , ("Lvm Manager", "lvm")
    , ("Ranger", "ranger")
    ]

bookmarksGrid :: [(String,String)]
bookmarksGrid = 
    [ ("Gmail", "mail.google.com")
    , ("Posteo", "posteo.de/en")
    , ("YouTube", "youtube.com")
    , ("LessWrong", "lesswrong.com")
    , ("Github", "github.com")
    , ("LeetCode", "leetcode.com")
    , ("Cppreference", "en.cppreference.com")
    , ("Python docs", "docs.python.com")
    , ("Haskell wiki", "wiki.haskell.org")
    , ("Vim cheatsheet", "vim.rotrr.com")
    , ("Neovim docs", "neovim.io/doc")
    , ("Git docs", "git-scm.com/docs")
    , ("Arch wiki", "wiki.archlinux.org")
    , ("Linux man pages", "linux.die.net/man")
    , ("Wikichip", "wikichip.org")
    , ("Libgen", "libgen.is")
    , ("Sci-hub", "sci-hubtw.hkvisa.net")
    , ("Wikibooks", "wikibooks.org")
    , ("Open Library", "openlibrary.org")
    , ("arXiv", "arxiv.org")
    , ("SSRN", "ssrn.com")
    , ("Jstor", "jstor.org")
    , ("Lexis Nexis", "lexisnexis.com")
    , ("ACM Digital Library", "dl.acm.org")
    , ("CIA World Factbook", "cia.gov/the-world-factbook")
    , ("Project Euler", "projecteuler.net")
    , ("Wolfram MathWorld", "mathworld.wolfram.com")
    ]

cliShellGrid :: [(String, String)]
cliShellGrid = 
    [ ("Ipython", "ipython")
    , ("GHCi", "ghci")
    , ("Radian", "radian")
    , ("GiNsh", "ginsh")
    , ("Basic Calculator", "bc -q")
    , ("Maxima", "maxima")
    , ("FriCAS", "fricas")
    , ("Gap", "gap")
    , ("Gnuplot", "gnuplot")
    ] -- sqlite, psql, pgcli

guiShellGrid :: [(String, String)]
guiShellGrid = 
    [ ("wxMaxima", "wxmaxima")
    , ("Xasy", "xasy")
    ]

dfGrid :: [(String, String)]
dfGrid = 
    [ ("Neovim",      "~/.config/nvim")
    , ("XMonad",      "~/.config/xmonad")
    , ("XMobar",      "~/.config/xmobar")
    , ("KMonad",      "~/.config/kmonad")
    , ("Kitty",       "~/.config/kitty")
    , ("Papis",       "~/.config/papis")
    , ("Ranger",      "~/.config/ranger")
    , ("i3",          "~/.config/i3/config")
    , ("py3status",   "~/.config/py3status/config")
    , ("Zathura",     "~/.config/zathura/zathurarc")
    , ("Aerc",        "~/.config/aerc")
    , ("qutebrowser", "~/.config/qutebrowser")
    ]

-- does this need to be debugged?
generateCmdFromStr str = alacritty ++ " " ++ str

appSpawnGrid = (zip names (map generateCmdFromStr cmds)) ++ guiGrid
    where 
      (names, cmds) = unzip tuiGrid

shellSpawnGrid = (zip names (map generateCmdFromStr cmds)) ++ guiShellGrid
    where 
      (names, cmds) = unzip cliShellGrid

utilSpawnGrid = zip names (map generateCmdFromStr cmds)
    where 
      (names, cmds) = unzip utilGrid

fullSpawnGrid = appSpawnGrid ++ shellSpawnGrid ++ utilSpawnGrid

dfSpawnGrid = zip names (map (\p -> alacritty ++ " " ++ nvim ++ " " ++ p) paths)
    where 
      (names, paths) = unzip dfGrid

bmSpawnGrid = zip names (map (\url -> qutebrowser ++ " " ++ url) urls)
    where 
      (names, urls) = unzip bookmarksGrid

-------------------------------------------------------------------------------

-- look into XMonad.Actions.Modal
dz str  = D.dzenConfig (timeout 15 >=> onCurr xScreen) str

exitStr = "(l)ock screen; (p)oweroff; (s)uspend; (h)ibernate; (e)xit;"

exitMap = M.fromList $
    [ ((0, xK_l),          spawn "xscreensaver-command -lock")
    , ((0, xK_p),          spawn "systemctl poweroff")
    , ((0, xK_r),          spawn "systemctl reboot")
    , ((0, xK_s),          spawn "systemctl suspend")
    , ((0, xK_h),          spawn "systemctl hibernate")
    , ((0, xK_x),          io (exitWith ExitSuccess))
    ]

exit = dz exitStr >> SM.submap exitMap


promptStr = "(m)an; (s)sh; (l)ayouts; (u)nicode; (r)unOrRaaise; (w)orkspace;"

promptMap = M.fromList $
    [ ((0, xK_m), manPrompt xmPConf)
    -- , ((0, xK_p), passOTPPRompt xmPConf)
    , ((0, xK_s), sshPrompt xmPConf)
    , ((0, xK_x), xmonadPrompt xmPConf)
    , ((0, xK_l), layoutPrompt xmPConf)
    , ((0, xK_u), typeUnicodePrompt unicodeDataPath xmPConf)
    , ((0, xK_r), runOrRaisePrompt xmPConf)
    , ((0, xK_w), workspacePrompt xmPConf (windows . W.shift))
    ]

unicodeDataPath = home ++ "/.local/share/xmonad/ucd.all.flat.xml"

prompt = dz promptStr >> SM.submap promptMap

gridSelectStr = "(a)pps; (b)ookmakrks; (d)otfiles; (f)ull; (s)hell; (u)tils;"

-- is there a way to replace or supplement this submapping with
    --another  grid menu ?
gridSelectMap = M.fromList $
    -- add more grids as we grow comfortable with grid select?
    [ ((0, xK_a),          spawnSelected' appSpawnGrid)
    , ((0, xK_b),          spawnSelected' bmSpawnGrid)
    , ((0, xK_d),          spawnSelected' dfSpawnGrid)
    , ((0, xK_f),          spawnSelected' fullSpawnGrid)
    , ((0, xK_s),          spawnSelected' shellSpawnGrid)
    , ((0, xK_u),          spawnSelected' utilSpawnGrid)
    -- TODO: figure out colorizers and grid select themes
    ] -- To whoever came up with this: it's really clever!
    where
      spawnSelected' l = gridselect def l >>= flip whenJust spawn

gridSelect = dz gridSelectStr >> SM.submap gridSelectMap

-- look into to use of dzen to display the submappings on screen
searchStr = "(a)rchWiki; wolfram(A)lpha; (s)tartpage; (S)tack Overflow;\
            \(g)ithub; (G)oogle scholar; (h)oogle; (H)ackage;\
            \(i)mages; (I)MDB; google (n)ews; wolfram (m)athworld;\
            \(t)hesaurus; (v)ocabulary; (w)ikipedia; (y)outube;"

searchMap method = M.fromList $
    [ ((0, xK_a),          method archWiki)

    , ((shift, xK_a),      method S.alpha)
    , ((0, xK_s),          method startpage)
    , ((shift, xK_s),      method stackOver)

    , ((0, xK_g),          method S.github)
    , ((shift, xK_g),      method S.scholar)

    , ((0, xK_h),          method S.hoogle)
    , ((shift, xK_h),      method S.hackage)

    -- , ((xmMod, xK_s),      method S.stackage)
    -- , ((0, xK_d),          method S.duckduckgo)
    -- , ((0, xK_g),          method S.google)

    , ((0, xK_i),          method S.images)
    , ((shift, xK_i),      method S.imdb)

    , ((0, xK_n),          method googleNews)
    , ((0, xK_m),          method S.mathworld)
    , ((0, xK_t),          method S.thesaurus)
    , ((0, xK_v),          method S.vocabulary)
    , ((0, xK_w),          method S.wikipedia)
    , ((0, xK_y),          method S.youtube)
    ]

makeSearch name url = S.searchEngine name url

archWiki = makeSearch "Archwiki" "https://wiki.archlinux.org/index.php?search="

googleNews = makeSearch "news" "https://news.google.com/search?q="

stackOver = makeSearch "Stack Overflow" "https://stackoverflow.com/search?q="

startpage = makeSearch "startpage" url
    where
      url = "https://www.startpage.com/rvd/search?query=%s&language=english"

-------------------------------------------------------------------------------

rmKeymap =
    [ (xmMod, xK_q)
    , (xmModShift, xK_q)
    , (xmMod, xK_w)
    , (xmModShift, xK_w)
    , (xmMod, xK_e)
    , (xmModShift, xK_e)
    , (xmMod, xK_r)
    , (xmModShift, xK_r)
    , (xmMod, xK_t)
    , (xmMod, xK_p)
    , (xmModShift, xK_p)

    , (xmMod, xK_h)
    , (xmMod, xK_j)
    , (xmModShift, xK_j)
    , (xmMod, xK_k)
    , (xmModShift, xK_k)
    , (xmMod, xK_l)

    , (xmMod, xK_Return)
    , (xmModShift, xK_Return)

    , (xmModShift, xK_c)
    , (xmMod, xK_n)
    , (xmMod, xK_m)

    -- , (xmMod, xK_comma)
    -- , (xmMod, xK_period)

    , (xmMod, xK_slash)
    , (xmModShift, xK_slash)
    , (xmMod, xK_space)
    , (xmModShift, xK_space)

    , (xmMod, xK_1)
    , (xmMod, xK_2)
    , (xmMod, xK_3)
    , (xmMod, xK_4)
    , (xmMod, xK_5)
    , (xmMod, xK_6)
    , (xmMod, xK_7)
    , (xmMod, xK_8)
    , (xmMod, xK_9)

    , (xmModShift, xK_1)
    , (xmModShift, xK_2)
    , (xmModShift, xK_3)
    , (xmModShift, xK_4)
    , (xmModShift, xK_5)
    , (xmModShift, xK_6)
    , (xmModShift, xK_7)
    , (xmModShift, xK_8)
    , (xmModShift, xK_9)
    ]


xmKeymap =
    [ ((xmMod, xK_Return),      spawn alacritty)
    -- <M-S-Return> launch terminal running cf from $HOME
    , ((xmModShift, xK_Return), spawn (alacritty ++ " zsh -is eval 'cf ~'"))
    -- <M-A-Return> launch terminal running rangercd
    , ((xmModAlt, xK_Return),   spawn (alacritty ++ " zsh -is eval 'rcd'"))
    -- <A-S-Return> launch terminnal running cf from root
    , ((altShift, xK_Return),   spawn (alacritty ++ " zsh -is eval 'cf /'"))

    -- vi-like move focus
    , ((xmMod, xK_h),           sendMessage $ Go L)
    , ((xmMod, xK_j),           sendMessage $ Go D)
    , ((xmMod, xK_k),           sendMessage $ Go U)
    , ((xmMod, xK_l),           sendMessage $ Go R)

    -- vi-like window swap
    , ((xmModShift, xK_h),      sendMessage $ Swap L)
    , ((xmModShift, xK_j),      sendMessage $ Swap D)
    , ((xmModShift, xK_k),      sendMessage $ Swap U)
    , ((xmModShift, xK_l),      sendMessage $ Swap R)

    , ((xmMod, xK_t),           withFocused $ windows . W.sink)
    , ((xmModShift, xK_t),      sendMessage ToggleStruts)
    -- <M-e> launch editor with nice menu
    , ((xmMod, xK_e),           spawn (alacritty ++ " "  ++ nvim))
    -- <M-S-e> launch bitwarden w/ rofi &  clear xclip 15s after copying pw
    , ((xmModShift, xK_e),      passPrompt xmPConf)


    -- Rotate through the available layout algorithms
    , ((xmMod, xK_space),       sendMessage NextLayout)

    -- <M-n> & <M-p> cycle focus up / down window stack
    , ((xmMod, xK_p),           windows W.focusUp)
    , ((xmMod, xK_n),           windows W.focusDown)

    -- <M-S-p> & <M-S-n> "bubblesort" windows down/up the stack
    , ((xmModShift, xK_p),      windows siftDown)
    , ((xmModShift, xK_n),      windows siftUp)
    -- alt tab cycle focus up / down window stack
    , ((alt, xK_Tab),           windows W.focusUp)
    , ((altShift, xK_Tab),      windows W.focusDown)

    , ((xmMod, xK_Tab),         moveTo Next  spacesOnCurrentScreen)
    , ((xmModShift, xK_Tab),    moveTo Prev  spacesOnCurrentScreen)

    -- <M-z> toggleWS?
    , ((xmMod, xK_z),           toggleWS)

    -- <M-x> prompt submap
    , ((xmMod, xK_x),           prompt)
    -- <M-S-x> exit submap
    , ((xmModShift, xK_x),      exit)

    -- <M-q> kill focused window
    , ((xmMod, xK_q),           kill)

    -- <M-c> edit xmonad config in neovim
    , ((xmMod, xK_c),           spawn editConfig)
    -- <M-S-c> reload xmonad
    , ((xmModShift, xK_c),      spawn reloadConfig)

    -- <M-w> launch grid select window menu
    , ((xmMod, xK_w),           windowMenu)
    -- <M-s> launch search sub mappings
    , ((xmMod, xK_s),           search)
    -- <M-o> launch zsh prompt "opener"
    , ((xmMod, xK_o),           zshPrompt xmPConf zshCapturePath)

    -- TODO Fix this layout switch thing
    -- <M-S-w> launch layout switch prompt
    , ((xmModShift, xK_w),      layoutPrompt xmPConf)
     -- <M-S-s> launch search select sub mappings
    , ((xmModShift, xK_s),      searchSelect)
    -- <M-S-o> launch grid select submappings
    , ((xmModShift, xK_o),      gridSelect)

    -- , ((xmMod, xK_slash),       SM.submap $ promptMap)
    -- , ((xmModShift, xK_slash),  spawnSelected def promptGrid)

    -- <M-b> launch qutebrowser
    -- , ((xmMod, xK_b),           spawn qutebrowser)
    -- <M-f> launch ranger
    , ((xmMod, xK_f ),          spawn (alacritty ++ " ranger"))

    -- <Insert> pastes from X selection (xclip)
    , ((0, xK_Insert),          pasteSelection)

    --TODO Debug these
    , ((0, xK_Print),           unGrab *> spawn scrotSelect)
    , ((xmMod, xK_Print),       unGrab *> spawn scrotFocus)
    , ((xmModShift, xK_Print),  unGrab *> spawn scrotFull)

    , ((xmMod, xK_b),           bringSelected def)
    , ((xmMod, xK_g),           goToSelected def)

    , ((xmModShift, xK_g),      windowPrompt xmPConf Goto wsWindows)
    , ((xmModShift, xK_b),      windowPrompt xmPConf Bring allWindows)

    -- <M-'<'> is shrink
    , ((xmModShift, xK_comma),  sendMessage Shrink)
    -- <M-'>' is grow
    , ((xmModShift, xK_period), sendMessage Expand)

    -- magnify
    , ((xmModCtrl, xK_plus),    sendMessage MagnifyMore)
    , ((xmModCtrl, xK_minus),   sendMessage MagnifyLess)
    , ((xmModCtrl, xK_m),       sendMessage Toggle)

    -- minimize
    , ((xmMod, xK_m),           withFocused minimizeWindow)
    , ((xmModShift, xK_m),      withLastMinimized maximizeWindowAndFocus)
    -- maximize
    , ((xmModAlt, xK_m),        withFocused (sendMessage . maximizeRestore))
    ]
    ++
    [((m .|. xmMod, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' xmConf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shift)]]
    ++
    [((m .|. xmMod, k), screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip [xK_bracketleft,xK_bracketright] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shift)]]
    where
      editConfig      = alacritty ++ " " ++ nvim ++ " " ++ configPath
      reloadConfig    = "xmonad --recompile; xmonad --restart"
      configPath      = "~/.config/xmonad/xmonad.hs"

      zshCapturePath  = "/home/carterlevo/scripts/capture.zsh"

      search          = SM.submap $ searchMap $ S.promptSearch xmPConf
      searchSelect    = SM.submap $ searchMap $ S.selectSearch

      scrotBase       = "scrot -q 100 "
      scrotPath       = "~/pictures/screnshots/%Y-%m-%d-%T-screenshot.png"
      scrotFull       = scrotBase ++ "-f" ++ " " ++ scrotPath
      scrotSelect     = scrotBase ++ "-s" ++ " " ++ "-f" ++ " " ++ scrotPath
      scrotFocus      = scrotBase ++ "-u" ++ " " ++ "-f" ++ " " ++ scrotPath


xmMouseMap (xmConf) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((xmMod, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((xmMod, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((xmMod, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------------

xmobarMain = statusBarPropTo "_XMONAD_LOG_0" cmd $ pure (marshallPP (S 0) def)
    where
      cmd = "xmobar -x 0 ~/.config/xmobar/xmobarrc.main" 

xmobarAlt = statusBarPropTo "_XMONAD_LOG_1" cmd $ pure (marshallPP (S 1) def)
    where
      cmd = "xmobar -x 1 ~/.config/xmobar/xmobarrc.alt"

------------------------------------------------------------------------------


xmStartupHook = do
    -- start some nice programs
    spawnOnce     "~/.fehbg"
    spawnOnce     "picom"
    spawnOnce     "xscreensaver -no-splash"
    spawnOnce     "xrandr --output DP1 --primary --output HDMI1 --left-of DP1"
    -- Focus the second screen.
    screenWorkspace 1 >>= flip whenJust (windows . W.view)
    -- Force the second screen to "1_1", e.g. if the first screen already has
    -- the workspace associated the screens will swap workspaces.
    windows $ W.greedyView "1_1"
    -- Focus the first screen again.
    screenWorkspace 0 >>= flip whenJust (windows . W.view)
    -- For tab cycle
    windows $ W.view "0_1"


-- Note that each layout is separated by ||| which denotes layout choice.
xmLayoutHook =
   mouseResize .
   windowArrange .
   windowNavigation .
   smartBorders .
   avoidStruts $  -- The available layouts start here
   noBorders Full
   ||| mosaic 9 [7,5]
   ||| mosaic 2 [3,2]
   ||| spiral (7/9)
   ||| spiral (6/7)
   ||| spiral (16/9)
   ||| ResizableTall 1 (3/100) (1/2) []
   ||| ResizableThreeColMid 1 (3/100) (1/2) []
   ||| Grid (16/9)
   ||| simpleTabbed
   -- ||| simpleFloat

-- not quite done tweaking layout list yet, also renaming layouts?


-- create some X window rules:
-- look into the hook helpers import
xmManageHook = composeAll
    [ isDialog                                           --> doFloat
    , className =? "mpv"                                 --> doFloat
    , className =? "Gimp"                                --> doFloat
    , className =? "toolbar"                             --> doFloat
    , className =? "notification"                        --> doFloat
    , className =? "confirm"                             --> doFloat
    , className =? "error"                               --> doFloat
    , className =? "download"                            --> doFloat
    , className =? "Toolkit"                             --> doFloat
    , className =? "Xmessage"                            --> doFloat
    , className =? "pinentry-qt"                         --> doFloat
    , className =? "pinentry"                            --> doFloat
    ]

xmEventHook = mempty

-- logging
-- Perform an arbitrary action on each internal state change or X event.Bring
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
-- xmLogHook = workspaceHistoryHook >> fadeWindowsLogHook xmFadeHook
xmLogHook = workspaceHistoryHook



------------------------------------------------------------------------------

xmMod, alt, shift, ctrl :: KeyMask
xmMod        = mod4Mask
alt          = mod1Mask
shift        = shiftMask
ctrl         = controlMask

xmModShift, xmModCtrl, xmModAlt :: KeyMask
xmModShift   = xmMod .|. shift
xmModCtrl    = xmMod .|. ctrl
xmModAlt     = xmMod .|. alt

altShift, ctrlAlt :: KeyMask
altShift     = alt   .|. shift
ctrlAlt      = alt   .|. ctrl

alacritty, qutebrowser, nvim, home :: String
alacritty    = "/usr/bin/alacritty"
qutebrowser  = "/usr/bin/qutebrowser"
nvim         = "/usr/bin/nvim"
home         = "home/carterlevo"

xmNBC, xmFBC, xmFont :: String
xmNBC        = "#DDDDDD"
xmFBC        = "#A865C9"
xmFont       = "xft:Hack Nerd Font Mono:weight=bold:pixelsize=14:antialias=true"

xmBW :: Dimension
xmBW         = 2

xmFocusFollowsMouse, xmClickJustFocuses :: Bool
xmFocusFollowsMouse = False
xmClickJustFocuses = True

-------------------------------------------------------------------------------

xmConf   = def { terminal           = alacritty
               , borderWidth        = xmBW
               , modMask            = xmMod
               , workspaces         = withScreens 2 xmWs
               , normalBorderColor  = xmNBC
               , focusedBorderColor = xmFBC
               , layoutHook         = xmLayoutHook
               , logHook            = xmLogHook
               , manageHook         = xmManageHook <+> manageHook def
               , handleEventHook    = xmEventHook
               , startupHook        = xmStartupHook
               , focusFollowsMouse  = xmFocusFollowsMouse
               , clickJustFocuses   = xmClickJustFocuses
               , mouseBindings      = xmMouseMap
               }

-------------------------------------------------------------------------------

xmPConf :: XPConfig
xmPConf  = def { P.font                = xmFont
               , P.position            = P.CenteredAt 0.5 0.5
               , P.height              = 36
               , P.alwaysHighlight     = True
               , P.autoComplete        = Just 10000 -- in μs so 10000μs = .01s
               , P.maxComplRows        = Just 5
               , P.bgColor             = "#000000"
               , P.fgColor             = "#7F00FF"
               , P.borderColor         = "#7F00FF"
               , P.promptBorderWidth   = 2
               }

-------------------------------------------------------------------------------

main :: IO()
main = xmonad
     . withSB (xmobarMain <> xmobarAlt)
     . ewmhFullscreen
     . ewmh
     . docks
     $ xmConf
     `removeKeys`     rmKeymap
     `additionalKeys` xmKeymap

-------------------------------------------------------------------------------
