{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad                         ((>=>))

import qualified Data.Map                              as M
import           Data.Monoid
import           Data.Tree

import           System.Exit                           (exitSuccess)

import           XMonad
import qualified XMonad.Prompt                         as P
import qualified XMonad.StackSet                       as W

import           XMonad.Actions.Commands
import           XMonad.Actions.CopyWindow             (copy)
import           XMonad.Actions.CycleWorkspaceByScreen
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Minimize
import           XMonad.Actions.MouseResize            (mouseResize)
import qualified XMonad.Actions.Search                 as S
import           XMonad.Actions.TopicSpace
import           XMonad.Actions.TreeSelect             as TS
import           XMonad.Actions.WindowMenu             (windowMenu)

import           XMonad.Hooks.DynamicIcons
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Modal
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Hooks.WorkspaceHistory

import qualified XMonad.Layout.BinarySpacePartition    as BSP
import           XMonad.Layout.LayoutCombinators       (JumpToLayout (..))
import           XMonad.Layout.Magnifier
import           XMonad.Layout.Minimize
import qualified XMonad.Layout.MultiToggle             as MT
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spiral
import           XMonad.Layout.SubLayouts
import qualified XMonad.Layout.ToggleLayouts           as T
import           XMonad.Layout.WindowArranger
import           XMonad.Layout.WindowNavigation


import           XMonad.Util.Dzen                      as DZ
import           XMonad.Util.EZConfig
import           XMonad.Util.Paste                     (pasteSelection)
import           XMonad.Util.Run                       (runInTerm)
import           XMonad.Util.SpawnOnce                 (spawnOnce)
import           XMonad.Util.Ungrab                    (unGrab)

-- import XMonad.Actions.CycleWS
-- import XMonad.Actions.WindowBringer
-- import XMonad.Hooks.RefocusLast
-- import XMonad.Util.Dmenu
-- import XMonad.Layout.Mosaic
-- import XMonad.Actions.Sift

-- TODO: dmenu
-- TODO: Advanced layuouts config
-- TODO: XMonad.contrib modal
-- TODO: more workspace and monitor control

shellGrid :: [(String, String)]
shellGrid =
    let (names, cmds) = unzip grid
     in zip names (map (\s -> term ++ " -e " ++ s) cmds)
    where
        hledger = "hledger-ui -f ~/finance/.hledger.journal"
        bc      = "bc -q"
        grid    =
          [ ("Ipython", "ipython")
          , ("GHCi", "ghci")
          , ("Radian", "radian")
          , ("GiNsh", "ginsh")
          , ("Basic Calculator", bc)
          , ("Maxima", "maxima")
          , ("Cmus", "cmus")
          , ("FriCAS", "fricas")
          , ("Gap", "gap")
          , ("VisiData", "vd")
          , ("WeeChat", "weechat")
          , ("Gnuplot", "gnuplot")
          , ("Htop", "htop")
          , ("Btop++", "btop")
          , ("S-tui", "s-tui")
          , ("Termshark", "termshark")
          , ("Ncdu", "ncdu")
          , ("Cfdisk", "cfdisk")
          , ("Lvm Manager", "lvm")
          , ("Ranger", "ranger")
          , ("Hledger", hledger)
          ]

appGrid :: [(String, String)]
appGrid = shellGrid ++ grid
    where
        grid =
          [ ("Vimiv", "vimiv")
          , ("Spotify", "spotify")
          , ("Gimp", "gimp")
          , ("wxMaxima", "wxmaxima")
          , ("Xasy", "xasy")
          , ("Inkscape", "inkscape")
          , ("Trader Workstation", "tws")
          , ("TradingView", "tradingview")
          ]

dotfilesGrid :: [(String, String)]
dotfilesGrid =
    let (names, paths) = unzip grid
     in  zip names (map (\p -> term ++ " -e " ++ editor ++ " " ++ p) paths)
    where
        grid =
          [ ("Neovim", "~/.config/nvim")
          , ("Kitty", "~/.config/kitty/kitty.conf")
          , ("ranger", "~/.config/ranger/rc.conf")
          , ("qutebrowser", "~/.config/qutebrowser")
          , ("XMonad", "~/.config/xmonad/xmonad.hs")
          , ("KMonad", "~/.config/kmonad")
          , ("i3", "~/.config/i3/config")
          , ("py3status", "~/.config/py3status/config")
          , ("polybar", "~/.config/polybar/config.ini")
          , ("taffybar", "~/.config/taffybar/taffybar.hs")
          , ("zathura", "~/.config/zathura/zathurarc")
          ]


bookmarkGrid :: [(String, String)]
bookmarkGrid =
    let (names, urls) = unzip grid
     in zip names (map (\u -> browser ++ " https://" ++ u) urls)
    where
        grid =
          [ ("Gmail", "mail.google.com")
          , ("Posteo", "posteo.de/en")
          , ("LessWrong", "lesswrong.com")
          , ("Leetcode", "leetcode.com")
          , ("Python Documentation", "docs.python.com")
          , ("Haskell Wiki", "wiki.haskell.org")
          , ("Vim cheatsheet", "vim.rotrr.com")
          , ("Neovim Documenation", "neovio.io/doc")
          , ("Git Documentation", "git-scm.com/doc")
          , ("Arch Wiki", "wiki.archlinux.org")
          , ("Linux manpages", "linux.die.net/man")
          , ("Wikichip", "wikichip.org")
          , ("Libgen", "libgen.is")
          , ("Sci-hub", "sc-hubtw.hkvisa.net")
          , ("Wikibooks", "wikibooks.org")
          , ("Open Library", "openlibrary.org")
          , ("arXiv", "arxiv.org")
          , ("SSRN", "ssrn.com")
          , ("JSTOR", "jstor.org")
          , ("Lexis Nexis", "lexisnexis.org")
          , ("ACM Library", "dl.acm.org")
          , ("CIA World Factbook", "cia.gov/the-world-factbook")
          , ("Project Euler", "projecteuler.net")
          , ("Wolfram Mathworld", "mathworld.wolfram.com")
          ]

-- spawnSelected' lst
gridSelectSpawn grid = gridselect gsConf grid >>= flip whenJust spawn

gsConf :: GSConfig String
gsConf =
    def
      { gs_cellheight = 40
      , gs_cellwidth = 200
      , gs_cellpadding = 6
      , gs_originFractX = 0.5
      , gs_originFractY = 0.5
      , gs_font = xmFont
      }

-- bookmarksTree =
--   [ Node (TSNode "Browser Bookmarks" "" (return ()))
--       [ Node (TSNode "Gmail" "" (browse "mail.google.com")) []
--       , Node (TSNode "Posteo" "" (browse "poste.de/en")) []
--       , Node (TSNode "LessWrong" "" (browse "lesswrong.com")) []
--       , Node (TSNode "Leetcode" "" (browse "leetcode.com")) []
--       , Node (TSNode "Cppreference" "" (browse "en.cppreference.com")) []
--       , Node (TSNode "Python docs" "" (browse "docs.python.com")) []
--       , Node (TSNode "Haskell wiki" "" (browse "wiki.haskell.org")) []
--       , Node (TSNode "Vim cheatsheet" "" (browse "vim.rotrr.com")) []
--       , Node (TSNode "Neovim docs" "" (browse "neovim.io/doc")) []
--       , Node (TSNode "Git docs" "" (browse "git-scm.com/doc")) []
--       , Node (TSNode "Arch wiki" "" (browse "wiki.archlinux.org")) []
--       , Node (TSNode "Linux manpages" "" (browse "linux.die.net/man")) []
--       , Node (TSNode "Wikichip" "" (browse "wikichip.org")) []
--       , Node (TSNode "Libgen" "" (browse "libgen.is")) []
--       , Node (TSNode "Sci-hub" "" (browse "sc-hubtw.hkvisa.net")) []
--       , Node (TSNode "Wikibooks" "" (browse "wikibooks.org")) []
--       , Node (TSNode "Open Library" "" (browse "openlibrary.org")) []
--       , Node (TSNode "arXiv" "" (browse "arxiv.org")) []
--       , Node (TSNode "SSRN" "" (browse "ssrn.com")) []
--       , Node (TSNode "Jstor" "" (browse "jstor.org")) []
--       , Node (TSNode "Lexis Nexis" "" (browse "lexisnexis.com")) []
--       , Node (TSNode "ACM Library" "" (browse "dl.acm.org")) []
--       , Node (TSNode "CIA Factbook" "" (browse "cia.gov/the-world-factbook")) []
--       , Node (TSNode "Project Euler" "" (browse "projecteuler.net")) []
--       , Node (TSNode "Wolfram Mathworld" "" (browse "mathworld.wolfram.com")) []
--       ]
--   ]
--     where
--         browse site = spawn (browser ++ " https://" ++ site)

-- searchTree =
--     [ Node (TSNode "Search Engines" "" (return ()))
--         [ Node (TSNode "WolframAlpha" "" (psearch S.alpha)) []
--         , Node (TSNode "arXiv" "" (psearch S.arXiv)) []
--         , Node (TSNode "AUR" "" (psearch S.aur)) []
--         , Node (TSNode "Dictionary" "" (psearch S.dictionary)) []
--         , Node (TSNode "Github" "" (psearch S.github)) []
--         , Node (TSNode "Google" "" (psearch S.google)) []
--         , Node (TSNode "Hackage" "" (psearch S.hackage)) []
--         , Node (TSNode "Hoogle" "" (psearch S.hoogle)) []
--         , Node (TSNode "IMDB" "" (psearch S.imdb)) []
--         , Node (TSNode "Mathworld" "" (psearch S.mathworld)) []
--         , Node (TSNode "Google Scholar" "" (psearch S.scholar)) []
--         , Node (TSNode "Thesaurus" "" (psearch S.thesaurus)) []
--         , Node (TSNode "Wikipedia" "" (psearch S.wikipedia)) []
--         , Node (TSNode "Youtube" "" (psearch S.youtube)) []
--         ]
--     ]
--         where
--             psearch = S.promptSearch promptConfig
--
-- tsConfig = def { ts_font = xmFont }
-------------------------------------------------------------------------------

xmModes :: [Mode]
xmModes = [xmExitMode, xmLaunchMode, xmWorkspaceMode, xmResizeMode]



-- letter: focus workspace beginning with letter 'a' to 'z'
-- shift+letter: move to workspace beginning with letter 'a' to 'z'
xmWorkspaceMode :: Mode
xmWorkspaceMode = mode "workspace" $ \cfg ->
    M.fromList $ focus ++ move
        where
            focus = zip ( zip (repeat noModMask) [xK_a..xK_z])
                      ( map (withLetWorkspace W.greedyView) ['a'.. 'z'])
            move = zip ( zip (repeat shift) [xK_a..xK_z])
                     ( map (withLetWorkspace W.shift) ['a'..'z'])

withLetWorkspace :: (String -> WindowSet -> WindowSet) -> Char -> X ()
withLetWorkspace job fstLet =
    do  ws      <- gets (map W.tag . W.hidden . windowset)
        current <- gets (W.currentTag . windowset)
        let appJob ws =
                case take 1 $ filter (\w -> fstLet == head w) ws of
                  (w:_) -> windows $ job w
                  []    -> return()
         in if head current == fstLet
           then appJob $ filter (/= current) ws
           else appJob ws

xmExitMode :: Mode
xmExitMode = mode "exit" $ \cfg ->
  M.fromList
    [ ((noModMask, xK_l), spawn "xscreensaver-command -lock")
    , ((noModMask, xK_p), spawn "systemctl poweroff")
    , ((noModMask, xK_r), spawn "systemctl reboot")
    , ((noModMask, xK_s), spawn "systemctl suspend")
    , ((noModMask, xK_h), spawn "systemctl hibernate")
    , ((noModMask, xK_x), io exitSuccess)
    ]


xmLaunchMode :: Mode
xmLaunchMode = mode "launch" $ \cfg ->
    M.fromList
      [ ((noModMask, xK_f), runInTerm "" "ranger"        >> exitMode)
      , ((noModMask, xK_b), spawn     browser            >> exitMode)
      , ((noModMask, xK_h), runInTerm "" "htop"          >> exitMode)
      , ((noModMask, xK_e), runInTerm "" "nvim"          >> exitMode)
      , ((noModMask, xK_s), spawn     "spotify-launcher" >> exitMode)
      , ((noModMask, xK_c), runInTerm "" "bc -q"         >> exitMode)
      , ((noModMask, xK_w), spawn     "firefox"          >> exitMode)
      , ((shift,     xK_w), spawn     "chromium"         >> exitMode)
      ]

xmResizeMode :: Mode
xmResizeMode = mode "resize" $ \cfg ->
    M.fromList []

type KeyPair = (KeyMask, KeySym)
rmKeys :: [KeyPair]
rmKeys =
  [ (modm, xK_Return)
  , (modShift, xK_Return)
  , (modm, xK_comma)
  , (modShift, xK_comma)
  , (modm, xK_period)
  , (modShift, xK_period)
  , (modm, xK_slash)
  , (modShift, xK_slash)
  , (modm, xK_space)
  -- , (modShift, xK_space)
  ]
  ++ zip (repeat modm)      [xK_a..xK_z]
  ++ zip (repeat modShift)  [xK_a..xK_z]
  ++ zip (repeat modm)      [xK_1..xK_9]
  ++ zip (repeat modShift)  [xK_1..xK_9]

type KeyMap = (KeyPair, X ())
addKeys :: [KeyMap]
addKeys =
  [ ((modShift, xK_h),       sendMessage $ Swap L) -- vi-style swap windows
  , ((modShift, xK_j),       sendMessage $ Swap D)
  , ((modShift, xK_k),       sendMessage $ Swap U)
  , ((modShift, xK_l),       sendMessage $ Swap R)
  , ((modm, xK_h),           sendMessage $ Go L) -- vi-style move focus
  , ((modm, xK_j),           sendMessage $ Go D)
  , ((modm, xK_k),           sendMessage $ Go U)
  , ((modm, xK_l),           sendMessage $ Go R)

  , ((modm, xK_t),           sendMessage ToggleStruts)
  , ((modShift, xK_t),       withFocused $ windows . W.sink)

  , ((alt, xK_Tab),          windows W.focusUp) -- cycle window focus
  , ((altShift, xK_Tab),     windows W.focusDown)

  , ((modShift, xK_comma),   sendMessage Shrink) -- "<" == shrink
  , ((modShift, xK_period),  sendMessage Expand) -- ">" == grow

  , ((modm, xK_space),       sendMessage NextLayout)
  -- , ((modShift,xK_space),    setLayout $ xmLayoutHook)

  , ((modm, xK_s),           selectWorkspace promptConfig) -- dynamic ws
  , ((modm, xK_a),           appendWorkspacePrompt promptConfig)
  , ((modm, xK_r),           renameWorkspace promptConfig)
  , ((modm, xK_m),           withWorkspace promptConfig (windows . W.shift))
  , ((modShift, xK_m),       withWorkspace promptConfig (windows . copy))

  , ((modCtrl, xK_plus),     sendMessage MagnifyMore ) -- magnify commands
  , ((modCtrl, xK_minus),    sendMessage MagnifyLess)
  , ((modCtrl, xK_m),        sendMessage Toggle)

  , ((modm, xK_underscore),  withFocused minimizeWindow) -- minimize commands


  , ((modShift, xK_w),       windowMenu) -- grid select commands
  , ((modShift, xK_b),       bringSelected def)
  , ((modShift, xK_g),       goToSelected def)
  , ((modShift, xK_c),       gridSelectSpawn dotfilesGrid)
  , ((modShift, xK_o),       gridSelectSpawn appGrid)

  , ((modm, xK_q),           kill)
  , ((modShift, xK_r),       spawn reloadConfig)

  , ((modm, xK_w),           setMode "workspace") -- modes
  , ((modm, xK_o),           setMode "launch")
  , ((modShift, xK_x),       setMode "exit")

  , ((modm, xK_Return),      spawn term)
  , ((modShift, xK_Return),  runInBash "'cf ~'") -- terminal
  , ((modAlt, xK_Return),    runInBash "'rcd'")
  , ((altShift, xK_Return),  runInBash "'cf /'")
  , ((modm, xK_c),           runInBash "'cf ~/.dotfiles'")

  , ((modm, xK_d),           spawn dmenu_run)
  , ((modm, xK_p),           defaultCommands >>= runCommand)

    -- TODO Debug these
  , ((noModMask, xK_Print),  unGrab *> spawn scrot)
  , ((noModMask, xK_Insert), pasteSelection)
  ]
  -- ++ -- switch to ws at index n
  -- zip (zip (repeat modm)) [xK_1..xK_9] (map (withWorkspaceIndex W.view) [1..])
  -- ++ -- set index N to the current workspace
  -- zip (zip (repeat modCtrl)) [xK_1..xK_9] (map setWorkspaceIndex [1..])
  ++ -- switch ws between screens
  [ ((m .|. modm, k), screenWorkspace sc >>= flip whenJust (windows . f))
         | (k, sc) <- zip [xK_bracketleft, xK_bracketright] [0 ..]
         , (f, m) <- [(W.view, 0), (W.shift, shift)]
  ]
    where
        dmenu_run = "dmenu_run -b -h 20 -p 'Yes, Master?'"

        reloadConfig = "xmonad --recompile; xmonad --restart"

        scrot = "sleep 0.2; scrot -sf -q 100 -t 25 " ++ scrotFile
        scrotFile = "~/pictures/screnshots/%Y-%m-%d-%T-screenshot.png"

        exitMsg = "(l)ock;(p)oweroff;(r)eboot;(s)uspend;(h)ibernate;(e)xit;"

        runInBash prog = runInTerm "" ("bash -is eval " ++ prog)

dz :: String -> X ()
dz = DZ.dzenConfig (timeout 10 >=> onCurr xScreen)

-- use loginctl?
-- start some nice programs
xmStartupHook :: X ()
xmStartupHook = do
  spawnOnce "xrandr --output DP1 --primary --output HDMI1 --left-of DP1"
  spawnOnce "~/.fehbg"
  spawnOnce "picom"
  spawnOnce "xscreensaver -no-splash"

  -- spawnOnce "pasystray"
  -- spawnOnce "blueman-applet"
  -- spawnOnce "nm-applet --sm-disable --indicator"

-- Note that each layout is separated by ||| which denotes layout choice.

xmLayoutHook =
  mouseResize
    . windowArrange
    . windowNavigation
    . smartBorders
    . avoidStruts
    . magnifier
    . minimize
    $ noBorders Full -- The available layouts start here
      ||| BSP.emptyBSP
      ||| spiral (6 / 7)

-- not quite done tweaking layout list yet, also renaming layouts?
-- nested layouts?
-- Toggle layouts?
--

-- create some X window rules:
-- look into the hook helpers import
xmManageHook :: Query (Endo WindowSet)
xmManageHook =
  composeAll
    [ isDialog                    --> doFloat
    , className =? "mpv"          --> doFloat
    , className =? "Gimp"         --> doFloat
    , className =? "toolbar"      --> doFloat
    , className =? "confirm"      --> doFloat
    , className =? "error"        --> doFloat
    , className =? "download"     --> doFloat
    , className =? "notification" --> doFloat
    , className =? "Toolkit"      --> doFloat
    , className =? "Xmessage"     --> doFloat
    , className =? "pinentry"     --> doFloat
    , className =? "pinentry-qt"  --> doFloat
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

promptConfig =
    def
      { P.font = xmFont
      , P.bgColor = "#000000"
      , P.fgColor= xmFBC
      , P.borderColor= xmFBC
      , P.position = P.Bottom
      }

xmobarMain = let xmobar = home   ++ "/.local/bin/xmobar"
                 cmd    = xmobar ++ " -x 0 " ++ "~/.config/xmobar/main.xmobarrc"
              in statusBarPropTo "_XMONAD_LOG_0" cmd (pure def)

xmobarAlt = let xmobar = home ++ "/.local/bin/xmobar"
                cmd    = xmobar ++ " -x 1 " ++ "~/.config/xmobar/alt.xmobarrc"
             in statusBarPropTo "_XMONAD_LOG_1" cmd (pure def)

------------------------------------------------------------------------------

xmWS :: [String]
xmWS = map show [1..9] ++ [ "music", "email", "irc"]

modm, alt, shift, ctrl :: KeyMask
modm  = mod4Mask
alt   = mod1Mask
shift = shiftMask
ctrl  = controlMask

modShift, modCtrl, modAlt :: KeyMask
modShift = modm .|. shift
modCtrl  = modm .|. ctrl
modAlt   = modm .|. alt

altShift, ctrlAlt :: KeyMask
altShift = alt  .|. shift
ctrlAlt  = alt  .|. ctrl

term, browser, editor, home :: String
term    = "kitty"
browser = "qutebrowser"
editor  = "nvim"
home    = "/home/carterlevo"

xmNBC, xmFBC, xmFont :: String
xmNBC  = "#DDDDDD"
xmFBC  = "#A865C9"
xmFont = "xft:Anonymous Pro Mono:weight=bold:pixelsize=14:antialias=true"

xmBW :: Dimension
xmBW = 2

-------------------------------------------------------------------------------

main :: IO()
main =
  xmonad
    . modal xmModes
    . ewmhFullscreen
    . ewmh
    . docks
    . withSB (xmobarMain <> xmobarAlt)
    $ def
        { terminal = term
        , borderWidth = xmBW
        , modMask = modm
        , workspaces = xmWS
        , normalBorderColor = xmNBC
        , focusedBorderColor = xmFBC
        , layoutHook = xmLayoutHook
        , logHook = xmLogHook
        , manageHook = xmManageHook <+> manageHook def
        , handleEventHook = xmEventHook
        , startupHook = xmStartupHook
        , focusFollowsMouse = False
        , clickJustFocuses = True
        }
        `removeKeys` rmKeys
        `additionalKeys` addKeys
