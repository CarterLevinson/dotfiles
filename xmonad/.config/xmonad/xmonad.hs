{-# LANGUAGE NoMonomorphismRestriction #-}

import XMonad

import Control.Monad

import Data.Monoid

import Data.Map qualified as M

import System.Exit (exitSuccess)

import XMonad.StackSet qualified as W

import XMonad.Actions.Commands
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleSelectedLayouts
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Minimize
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Actions.TreeSelect qualified as TS

import XMonad.Actions.Sift
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowMenu (windowMenu)

import XMonad.Actions.Search qualified as S
import XMonad.Actions.Submap qualified as SM

import XMonad.Hooks.DynamicIcons
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Modal
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory

import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutCombinators (JumpToLayout (..))
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

import XMonad.Layout.BinarySpacePartition qualified as BSP
import XMonad.Layout.Mosaic
import XMonad.Layout.Spiral

import XMonad.Layout.MultiToggle qualified as MT
import XMonad.Layout.ToggleLayouts qualified as T

-- import           XMonad.Util.ClickableWorkspaces
-- import           XMonad.Util.Loggers
-- import           XMonad.Util.WorkspaceCompare

import XMonad.Util.Dmenu
import XMonad.Util.Dzen as DZ
import XMonad.Util.EZConfig
import XMonad.Util.Paste (pasteSelection)
import XMonad.Util.Run (runInTerm)
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)

-- TODO: dmenu
-- TODO: Advanced layuouts config
-- TODO: XMonad.contrib modal
-- TODO: more workspace and monitor control

-- workspaces
-------------------------------------------------------------------------------

xmWs :: [String]
xmWs = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

spacesOnCurrentScreen :: WSType
spacesOnCurrentScreen = WSIs (isOnScreen <$> currentScreen)

-------------------------------------------------------------------------------

generateCmdFromStr :: String -> String
generateCmdFromStr str = term ++ " -e " ++ str

-- define grid selects

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

shellSpawnGrid :: [(String, String)]
shellSpawnGrid = zip names (map generateCmdFromStr cmds) ++ guiShellGrid
  where
    (names, cmds) = unzip cliShellGrid

guiGrid :: [(String, String)]
guiGrid =
    [ ("Vimiv", "vimiv")
    , ("Spotify", "spotify")
    , ("Gimp", "gimp")
    , ("Inkscape", "inkscape")
    , ("TradingView", "tradingview")
    ]

tuiGrid :: [(String, String)]
tuiGrid =
    [ ("Ledger", "ledger")
    , ("Hledger", "hledger")
    , ("VisiData", "vd")
    , ("Cmus", "cmus")
    , ("Mpv", "mpv")
    , ("Papis", "papis")
    , ("WeeChat", "weechat")
    ]

appSpawnGrid :: [(String, String)]
appSpawnGrid = zip names (map generateCmdFromStr cmds) ++ guiGrid
  where
    (names, cmds) = unzip tuiGrid

utilGrid :: [(String, String)]
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

utilSpawnGrid :: [(String, String)]
utilSpawnGrid = zip names (map generateCmdFromStr cmds)
  where
    (names, cmds) = unzip utilGrid

fullSpawnGrid :: [(String, String)]
fullSpawnGrid = appSpawnGrid ++ shellSpawnGrid ++ utilSpawnGrid

-- replace with treeselect
dfGrid :: [(String, String)]
dfGrid =
    [ ("Neovim", "~/.config/nvim")
    , ("XMonad", "~/.config/xmonad")
    , ("XMobar", "~/.config/xmobar")
    , ("KMonad", "~/.config/kmonad")
    , ("Kitty", "~/.config/kitty")
    , ("Papis", "~/.config/papis")
    , ("Ranger", "~/.config/ranger")
    , ("i3", "~/.config/i3/config")
    , ("py3status", "~/.config/py3status/config")
    , ("Zathura", "~/.config/zathura/zathurarc")
    , ("Aerc", "~/.config/aerc")
    , ("qutebrowser", "~/.config/qutebrowser")
    ]

-- replace with treeselect or dmenu
bookmarksGrid :: [(String, String)]
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

dfSpawnGrid :: [(String, String)]
dfSpawnGrid = zip names (map (\p -> term ++ " -e " ++ nvim ++ " " ++ p) paths)
  where
    (names, paths) = unzip dfGrid

bmSpawnGrid :: [(String, String)]
bmSpawnGrid = zip names (map (\url -> browser ++ " " ++ url) urls)
  where
    (names, urls) = unzip bookmarksGrid

gridSelect = spawnSelected' fullSpawnGrid
  where
    spawnSelected' l = gridselect gsconf l >>= flip whenJust spawn
    gsconf =
        def
            { gs_cellheight = 40
            , gs_cellwidth = 200
            , gs_cellpadding = 6
            , gs_originFractX = 0.5
            , gs_originFractY = 0.5
            , gs_font = xmFont
            }

-------------------------------------------------------------------------------

-- look into XMonad-Contrib hooks Modal

-- app launcher submap
dz :: String -> X ()
dz = DZ.dzenConfig (timeout 15 >=> onCurr xScreen)

exitStr :: String
exitStr = "(l)ock; (p)oweroff; (r)eboot; (s)uspend; (h)ibernate; (e)xit;"

exitMap :: M.Map KeyPair (X ())
exitMap =
    M.fromList
        [ ((0, xK_l), spawn "xscreensaver-command -lock")
        , ((0, xK_p), spawn "systemctl poweroff")
        , ((0, xK_r), spawn "systemctl reboot")
        , ((0, xK_s), spawn "systemctl suspend")
        , ((0, xK_h), spawn "systemctl hibernate")
        , ((0, xK_x), io exitSuccess)
        ]

exit :: X ()
exit = dz exitStr >> SM.submap exitMap

-- look into to use of dzen to display the submappings on screen
searchStr :: String
searchStr =
    "(a)rchWiki; wolfram(A)lpha; (s)tartpage; (S)tack Overflow;\
    \(g)ithub; (G)oogle scholar; (h)oogle; (H)ackage;\
    \(i)mages; (I)MDB; google (n)ews; wolfram (m)athworld;\
    \(t)hesaurus; (v)ocabulary; (w)ikipedia; (y)outube;"

searchMap method =
    M.fromList
        [ ((0, xK_a), method archWiki)
        , ((shift, xK_a), method S.alpha)
        , ((0, xK_s), method startpage)
        , ((shift, xK_s), method stackOver)
        , ((0, xK_g), method S.github)
        , ((shift, xK_g), method S.scholar)
        , ((0, xK_h), method S.hoogle)
        , ((shift, xK_h), method S.hackage)
        , ((0, xK_i), method S.images)
        , ((shift, xK_i), method S.imdb)
        , ((0, xK_n), method googleNews)
        , ((0, xK_m), method S.mathworld)
        , ((0, xK_t), method S.thesaurus)
        , ((0, xK_v), method S.vocabulary)
        , ((0, xK_w), method S.wikipedia)
        , ((0, xK_y), method S.youtube)
        -- , ((xmMod, xK_s),      method S.stackage)
        -- , ((0, xK_d),          method S.duckduckgo)
        -- , ((0, xK_g),          method S.google)
        ]

makeSearch :: S.Name -> String -> S.SearchEngine
makeSearch = S.searchEngine

archWiki :: S.SearchEngine
archWiki = makeSearch "Archwiki" "https://wiki.archlinux.org/index.php?search="

googleNews :: S.SearchEngine
googleNews = makeSearch "news" "https://news.google.com/search?q="

stackOver :: S.SearchEngine
stackOver = makeSearch "Stack Overflow" "https://stackoverflow.com/search?q="

startpage :: S.SearchEngine
startpage = makeSearch "startpage" url
  where
    url = "https://www.startpage.com/rvd/search?query=%s&language=english"

-------------------------------------------------------------------------------

type KeyPair = (KeyMask, KeySym)
type KeyMap = (KeyPair, X ())

-- clear almost all keybindings
rmKeyPairs :: [KeyPair]
rmKeyPairs =
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
    , -- , (xmMod, xK_comma)
      -- , (xmMod, xK_period)

      (xmMod, xK_slash)
    , (xmModShift, xK_slash)
    , -- , (xmMod, xK_space)
      -- , (xmModShift, xK_space)
      (xmMod, xK_1)
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

-- replace alll keybindings
xmKeyMaps :: [KeyMap]
xmKeyMaps =
    [ ((xmMod, xK_h), sendMessage $ Go L) -- window commands
    , ((xmMod, xK_j), sendMessage $ Go D) -- vi-style move focus
    , ((xmMod, xK_k), sendMessage $ Go U)
    , ((xmMod, xK_l), sendMessage $ Go R)
    , ((xmModShift, xK_h), sendMessage $ Swap L) -- vi-style swap windows
    , ((xmModShift, xK_j), sendMessage $ Swap D)
    , ((xmModShift, xK_k), sendMessage $ Swap U)
    , ((xmModShift, xK_l), sendMessage $ Swap R)
    , ((xmMod, xK_t), sendMessage ToggleStruts) -- toggle struts
    , ((xmModShift, xK_t), withFocused $ windows . W.sink) -- sink from floating
    , ((alt, xK_Tab), windows W.focusUp) -- cycle window focus
    , ((altShift, xK_Tab), windows W.focusDown)
    , ((xmModShift, xK_w), windowMenu)
    , ((xmModShift, xK_comma), sendMessage Shrink) -- "<" == shrink
    , ((xmModShift, xK_period), sendMessage Expand) -- ">" == grow
    , -- magnify
      --   ((xmModCtrl, xK_plus), sendMessage MagnifyMore)
      -- , ((xmModCtrl, xK_minus), sendMessage MagnifyLess)
      -- , ((xmModCtrl, xK_m), sendMessage Toggle)
      -- minimize
      -- ((xmMod, xK_m), withFocused minimizeWindow)
      -- maximize
      -- ((xmModAlt, xK_m), withFocused (sendMessage . maximizeRestore))
      -- ((xmModShift, xK_m), withLastMinimized maximizeWindowAndFocus)

      ((xmModShift, xK_b), bringMenuArgs dmenu_args)
    , ((xmMod, xK_b), bringSelected def)
    , ((xmModShift, xK_g), gotoMenuArgs dmenu_args)
    , ((xmMod, xK_g), goToSelected def)
    , -- spawn apps
      ((xmMod, xK_Return), spawn term) -- like a terminal
    , ((xmModShift, xK_Return), runInTerm "" "bash -is eval 'cf ~'")
    , ((xmModAlt, xK_Return), runInTerm "" "bash -is eval 'rcd'")
    , ((altShift, xK_Return), runInTerm "" "bash -is eval 'cf /'")
    , ((xmMod, xK_e), runInTerm "" nvim)
    , ((xmMod, xK_d), spawn dmenu_run)
    , ((xmMod, xK_w), spawn browser)
    , ((xmMod, xK_o), defaultCommands >>= runCommand)
    , -- , ((xmModShift, xK_b), spawn browser)
      ((xmMod, xK_f), runInTerm "" ranger)
    , -- workspace commands
      -- <M-z> toggleWS?
      ((xmMod, xK_z), toggleWS)
    , ((xmMod, xK_Tab), moveTo Next spacesOnCurrentScreen)
    , ((xmModShift, xK_Tab), moveTo Prev spacesOnCurrentScreen)
    , ((xmModShift, xK_x), exit)
    , -- <M-q> kill focused window
      ((xmMod, xK_q), kill)
    , -- <M-c> edit xmonad config in neovim
      ((xmMod, xK_c), editConfig)
    , -- <M-S-c> reload xmonad
      ((xmModShift, xK_c), reloadConfig)
    , -- <M-s> launch search sub mappings
      ((xmMod, xK_s), search)
    , -- <M-S-s> launch search select sub mappings
      ((xmModShift, xK_s), searchSelect)
    , -- <M-S-o> launch grid select submappings
      ((xmModShift, xK_o), gridSelect)
    , -- <Insert> pastes from X selection (xclip)
      ((0, xK_Insert), pasteSelection)
    , -- TODO Debug these
      ((0, xK_Print), unGrab *> spawn scrot)
    ]
        ++ [ ((m .|. xmMod, k), windows $ onCurrentScreen f i)
           | (i, k) <- zip (workspaces' xmConf) [xK_1 .. xK_9]
           , (f, m) <- [(W.view, 0), (W.shift, shift)]
           ] -- switch ws on screen
        ++ [ ((m .|. xmMod, k), screenWorkspace sc >>= flip whenJust (windows . f))
           | (k, sc) <- zip [xK_bracketleft, xK_bracketright] [0 ..]
           , (f, m) <- [(W.view, 0), (W.shift, shift)]
           ] -- switch ws between screens
  where
    dmenu_args = ["-b", "-h", "20"]
    dmenu_run = "dmenu_run " ++ unlines dmenu_args ++ " -p 'Yes, Master?'"

    editConfig = runInTerm "" (nvim ++ " " ++ configFile)
    reloadConfig = spawn "xmonad --recompile; xmonad --restart"

    search = SM.submap $ searchMap $ S.promptSearch def
    searchSelect = SM.submap $ searchMap S.selectSearch

    configFile = "~/.config/xmonad/xmonad.hs"
    scrotFile = "~/pictures/screnshots/%Y-%m-%d-%T-screenshot.png"

    scrot = "sleep 0.2; scrot -sf -q 100 -t 25 " ++ scrotFile

------------------------------------------------------------------------------

xmobarMain :: StatusBarConfig
xmobarMain = statusBarPropTo "_XMONAD_LOG_0" cmd $ pure (marshallPP (S 0) def)
  where
    cmd = "xmobar -x 0 ~/.config/xmobar/main.xmobarrc"

xmobarAlt :: StatusBarConfig
xmobarAlt = statusBarPropTo "_XMONAD_LOG_1" cmd $ pure (marshallPP (S 1) def)
  where
    cmd = "xmobar -x 1 ~/.config/xmobar/alt.xmobarrc"

------------------------------------------------------------------------------

xmStartupHook :: X ()
xmStartupHook = do
    -- start some nice programs
    spawnOnce "~/.fehbg"
    spawnOnce "picom"
    spawnOnce "xrandr --output DP1 --primary --output HDMI1 --left-of DP1"
    spawnOnce "xscreensaver -no-splash"
    -- Focus the second screen.
    screenWorkspace 1 >>= flip whenJust (windows . W.view)
    -- Force the second screen to "1_1", e.g. if the first screen already has
    -- the workspace associated the screens will swap workspaces.
    windows $ W.greedyView "1_1"
    -- Focus the first screen again.
    -- For tab cycle
    windows $ W.view "0_1"

-- Note that each layout is separated by ||| which denotes layout choice.

xmLayoutHook =
    mouseResize
        . windowArrange
        . windowNavigation
        . smartBorders
        . avoidStruts
        $ noBorders Full -- The available layouts start here
            ||| BSP.emptyBSP
            ||| spiral (6 / 7)

xmIcons :: Query [String]
xmIcons =
    composeAll
        [ className =? "Firefox" --> appIcon "\xE745"
        , className =? "Spotify" --> appIcon "\xF1BC"
        ]

-- not quite done tweaking layout list yet, also renaming layouts?

-- create some X window rules:
-- look into the hook helpers import
xmManageHook :: Query (Endo WindowSet)
xmManageHook =
    composeAll
        [ isDialog --> doFloat
        , className =? "mpv" --> doFloat
        , className =? "Gimp" --> doFloat
        , className =? "toolbar" --> doFloat
        , className =? "notification" --> doFloat
        , className =? "confirm" --> doFloat
        , className =? "error" --> doFloat
        , className =? "download" --> doFloat
        , className =? "Toolkit" --> doFloat
        , className =? "Xmessage" --> doFloat
        , className =? "pinentry-qt" --> doFloat
        , className =? "pinentry" --> doFloat
        ]

xmEventHook :: Event -> X All
xmEventHook = swallowEventHook query (return True)
  where
    query = className =? "kitty" <||> className =? "alacritty"

-- logging
-- Perform an arbitrary action on each internal state change or X event.Bring
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
-- xmLogHook = workspaceHistoryHook >> fadeWindowsLogHook xmFadeHook
xmLogHook :: X ()
xmLogHook = workspaceHistoryHook

------------------------------------------------------------------------------

xmMod, alt, shift, ctrl :: KeyMask
xmMod = mod4Mask
alt = mod1Mask
shift = shiftMask
ctrl = controlMask

xmModShift, xmModCtrl, xmModAlt :: KeyMask
xmModShift = xmMod .|. shift
xmModCtrl = xmMod .|. ctrl
xmModAlt = xmMod .|. alt

altShift, ctrlAlt :: KeyMask
altShift = alt .|. shift
ctrlAlt = alt .|. ctrl

term, browser, ranger, nvim, home :: String
term = "kitty"
browser = "qutebrowser"
nvim = "nvim"
ranger = "ranger"
home = "/home/carterlevo"

xmNBC, xmFBC, xmFont :: String
xmNBC = "#DDDDDD"
xmFBC = "#A865C9"
xmFont = "xft:Anonymous Pro Mono:weight=bold:pixelsize=14:antialias=true"

xmBW :: Dimension
xmBW = 2

xmFocusFollowsMouse, xmClickJustFocuses :: Bool
xmFocusFollowsMouse = False
xmClickJustFocuses = True

-------------------------------------------------------------------------------

xmConf =
    def
        { terminal = term
        , borderWidth = xmBW
        , modMask = xmMod
        , workspaces = withScreens 2 xmWs
        , normalBorderColor = xmNBC
        , focusedBorderColor = xmFBC
        , layoutHook = xmLayoutHook
        , logHook = xmLogHook
        , manageHook = xmManageHook <+> manageHook def
        , handleEventHook = xmEventHook
        , startupHook = xmStartupHook
        , focusFollowsMouse = xmFocusFollowsMouse
        , clickJustFocuses = xmClickJustFocuses
        -- , mouseBindings = xmMouseMap
        }

-------------------------------------------------------------------------------
main :: IO ()
main =
    xmonad
        . withSB (xmobarMain <> xmobarAlt)
        . ewmhFullscreen
        . ewmh
        . docks
        $ xmConf
            `removeKeys` rmKeyPairs
            `additionalKeys` xmKeyMaps
