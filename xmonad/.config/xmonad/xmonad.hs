-- import System.Taffybar.XMonadLog (dbusLog)
import           Codec.Binary.UTF8.String              as UTF8

import           Control.Monad                         ((>=>))

import qualified DBus                                  as D
import qualified DBus.Client                           as D

import qualified Data.Map                              as M
import           Data.Monoid

import           System.Exit                           (exitSuccess)

import           XMonad

import           XMonad.Actions.Commands
import           XMonad.Actions.CopyWindow             (copy)
import           XMonad.Actions.CycleWorkspaceByScreen
import           XMonad.Actions.DynamicWorkspaces      as DW
import           XMonad.Actions.GridSelect
import           XMonad.Actions.MouseResize            (mouseResize)
import qualified XMonad.Actions.Search                 as S
import           XMonad.Actions.TopicSpace
import qualified XMonad.Actions.TreeSelect             as TS
import           XMonad.Actions.WindowBringer
import           XMonad.Actions.WindowMenu             (windowMenu)
import           XMonad.Actions.WorkspaceNames

import           XMonad.Hooks.DynamicIcons
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Modal
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Hooks.WorkspaceHistory

import qualified XMonad.Layout.BinarySpacePartition    as BSP
import           XMonad.Layout.LayoutCombinators       (JumpToLayout (..))
import           XMonad.Layout.Magnifier
import qualified XMonad.Layout.MultiToggle             as MT
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spiral
import           XMonad.Layout.SubLayouts
import qualified XMonad.Layout.ToggleLayouts           as T
import           XMonad.Layout.WindowArranger
import           XMonad.Layout.WindowNavigation

import           XMonad.Prompt                         as P

import qualified XMonad.StackSet                       as W

import           XMonad.Util.Dzen                      as DZ
import           XMonad.Util.EZConfig                  (additionalKeys,
                                                        removeKeys)
import           XMonad.Util.Paste                     (pasteSelection)
import           XMonad.Util.Run                       (runInTerm)
import           XMonad.Util.Scratchpad
import           XMonad.Util.SpawnOnce                 (spawnOnce)
import           XMonad.Util.Ungrab                    (unGrab)

-- import XMonad.Actions.CycleWS
-- import XMonad.Hooks.RefocusLast
-- import XMonad.Hooks.StatusBar
-- import XMonad.Hooks.StatusBar.PP
-- import XMonad.Util.Dmenu
-- import XMonad.Layout.Mosaic
-- import XMonad.Layout.Magnifier
-- import XMonad.Layout.Maximize
-- import XMonad.Layout.Minimize
-- import XMonad.Layout.IndependentScreens
-- import XMonad.Actions.Sift
-- import XMonad.Actions.CycleSelectedLayouts
-- import XMonad.Actions.Minimize

-- TODO: dmenu
-- TODO: Advanced layuouts config
-- TODO: XMonad.contrib modal
-- TODO: more workspace and monitor control

-------------------------------------------------------------------------------

generateCmdFromStr :: String -> String
generateCmdFromStr str = term ++ " -e " ++ str

-- define grid selects

cliShellGrid :: [(String, String)]
cliShellGrid =
  [ ("Ipython", "ipython"),
    ("GHCi", "ghci"),
    ("Radian", "radian"),
    ("GiNsh", "ginsh"),
    ("Basic Calculator", "bc -q"),
    ("Maxima", "maxima"),
    ("FriCAS", "fricas"),
    ("Gap", "gap"),
    ("Gnuplot", "gnuplot")
  ] -- sqlite, psql, pgcli

guiShellGrid :: [(String, String)]
guiShellGrid =
  [ ("wxMaxima", "wxmaxima"),
    ("Xasy", "xasy")
  ]

shellSpawnGrid :: [(String, String)]
shellSpawnGrid = zip names (map generateCmdFromStr cmds) ++ guiShellGrid
  where
    (names, cmds) = unzip cliShellGrid

guiGrid :: [(String, String)]
guiGrid =
  [ ("Vimiv", "vimiv"),
    ("Spotify", "spotify"),
    ("Gimp", "gimp"),
    ("Inkscape", "inkscape"),
    ("TradingView", "tradingview")
  ]

tuiGrid :: [(String, String)]
tuiGrid =
  [ ("Ledger", "ledger"),
    ("Hledger", "hledger"),
    ("VisiData", "vd"),
    ("Cmus", "cmus"),
    ("Mpv", "mpv"),
    ("Papis", "papis"),
    ("WeeChat", "weechat")
  ]

appSpawnGrid :: [(String, String)]
appSpawnGrid = zip names (map generateCmdFromStr cmds) ++ guiGrid
  where
    (names, cmds) = unzip tuiGrid

utilGrid :: [(String, String)]
utilGrid =
  [ ("Htop", "htop"),
    ("Btop++", "btop"),
    ("S-tui", "s-tui"),
    ("Termshark", "termshark"),
    ("Ncdu", "ncdu"),
    ("Cfdisk", "cfdisk"),
    ("Lvm Manager", "lvm"),
    ("Ranger", "ranger")
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
  [ ("Neovim", "~/.config/nvim"),
    ("XMonad", "~/.config/xmonad"),
    ("XMobar", "~/.config/xmobar"),
    ("KMonad", "~/.config/kmonad"),
    ("Kitty", "~/.config/kitty"),
    ("Papis", "~/.config/papis"),
    ("Ranger", "~/.config/ranger"),
    ("i3", "~/.config/i3/config"),
    ("py3status", "~/.config/py3status/config"),
    ("Zathura", "~/.config/zathura/zathurarc"),
    ("Aerc", "~/.config/aerc"),
    ("qutebrowser", "~/.config/qutebrowser")
  ]

-- replace with treeselect or dmenu
bookmarksGrid :: [(String, String)]
bookmarksGrid =
  [ ("Gmail", "mail.google.com"),
    ("Posteo", "posteo.de/en"),
    ("YouTube", "youtube.com"),
    ("LessWrong", "lesswrong.com"),
    ("Github", "github.com"),
    ("LeetCode", "leetcode.com"),
    ("Cppreference", "en.cppreference.com"),
    ("Python docs", "docs.python.com"),
    ("Haskell wiki", "wiki.haskell.org"),
    ("Vim cheatsheet", "vim.rotrr.com"),
    ("Neovim docs", "neovim.io/doc"),
    ("Git docs", "git-scm.com/docs"),
    ("Arch wiki", "wiki.archlinux.org"),
    ("Linux man pages", "linux.die.net/man"),
    ("Wikichip", "wikichip.org"),
    ("Libgen", "libgen.is"),
    ("Sci-hub", "sci-hubtw.hkvisa.net"),
    ("Wikibooks", "wikibooks.org"),
    ("Open Library", "openlibrary.org"),
    ("arXiv", "arxiv.org"),
    ("SSRN", "ssrn.com"),
    ("Jstor", "jstor.org"),
    ("Lexis Nexis", "lexisnexis.com"),
    ("ACM Digital Library", "dl.acm.org"),
    ("CIA World Factbook", "cia.gov/the-world-factbook"),
    ("Project Euler", "projecteuler.net"),
    ("Wolfram MathWorld", "mathworld.wolfram.com")
  ]

dfSpawnGrid :: [(String, String)]
dfSpawnGrid = zip names (map (\p -> term ++ " -e " ++ nvim ++ " " ++ p) paths)
  where
    (names, paths) = unzip dfGrid

bmSpawnGrid :: [(String, String)]
bmSpawnGrid = zip names (map (\url -> browser ++ " " ++ url) urls)
  where
    (names, urls) = unzip bookmarksGrid

gridSelectOpener :: X()
gridSelectOpener = spawnSelected' fullSpawnGrid
  where
    spawnSelected' l = gridselect gsConf l >>= flip whenJust spawn


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
-------------------------------------------------------------------------------

withLetWorkspace :: (String -> WindowSet -> WindowSet) -> Char -> X ()
withLetWorkspace job fstLet =
    do  ws <- gets (map W.tag . W.hidden . windowset)
        current <- gets (W.currentTag . windowset)
        if head current == fstLet
           then appJob $ filter (/= current) ws
           else appJob ws
        where
            appJob :: [String] -> X()
            appJob ws =
                case take 1 $ filter (\w -> fstLet == head w) ws of
                  (w:_) -> windows $ job w
                  []    -> return ()


xmWorkspaceMode :: Mode
xmWorkspaceMode = mode "workspace" $ \cfg ->
    M.fromList lst
        where
            -- focus workspace beginning with letter
            focus = zip ( zip (repeat noModMask) [xK_a..xK_z])
                      ( map (withLetWorkspace W.greedyView) ['a'.. 'z'])
            -- move to worksapce beginning with letter
            move = zip ( zip (repeat shift) [xK_a..xK_z])
                     ( map (withLetWorkspace W.shift) ['a'..'z'])
            lst = focus ++ move


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
      [ ((noModMask, xK_f), runInTerm "" ranger      >> exitMode)
      , ((noModMask, xK_b), spawn browser            >> exitMode)
      , ((noModMask, xK_h), runInTerm "" "htop"      >> exitMode)
      , ((noModMask, xK_e), runInTerm "" "nvim"      >> exitMode)
      , ((noModMask, xK_s), spawn "spotify-launcher" >> exitMode)
      , ((shift,     xK_f), spawn "firefox"          >> exitMode)
      ]

exit :: X ()
exit = dz str >> setMode "exit"
    where
        str =  "(l)ock;(p)oweroff;(r)eboot;(s)uspend;(h)ibernate;(e)xit;"


dz :: String -> X ()
dz = DZ.dzenConfig (timeout 10 >=> onCurr xScreen)

type KeyPair = (KeyMask, KeySym)
rmKeyPairs :: [KeyPair]
rmKeyPairs =
  [ (xmMod, xK_Return)
  , (xmModShift, xK_Return)
  , (xmMod, xK_comma)
  , (xmModShift, xK_comma)
  , (xmMod, xK_period)
  , (xmModShift, xK_period)
  , (xmMod, xK_slash)
  , (xmModShift, xK_slash)
  -- , (xmMod, xK_space)
  -- , (xmModShift, xK_space)
  ]
  ++ zip (repeat xmMod)      [xK_a..xK_z]
  ++ zip (repeat xmModShift) [xK_a..xK_z]
  ++ zip (repeat xmMod)      [xK_1..xK_9]
  ++ zip (repeat xmModShift) [xK_1..xK_9]

type KeyMap = (KeyPair, X ())
xmKeyMaps :: [KeyMap]
xmKeyMaps =
  [ ((xmMod, xK_Return),      spawn term)
  , ((xmModShift, xK_h),      sendMessage $ Swap L) -- vi-style swap windows
  , ((xmModShift, xK_j),      sendMessage $ Swap D)
  , ((xmModShift, xK_k),      sendMessage $ Swap U)
  , ((xmModShift, xK_l),      sendMessage $ Swap R)
  , ((xmMod, xK_h),           sendMessage $ Go L) -- vi-style move focus
  , ((xmMod, xK_j),           sendMessage $ Go D)
  , ((xmMod, xK_k),           sendMessage $ Go U)
  , ((xmMod, xK_l),           sendMessage $ Go R)
  , ((xmMod, xK_t),           sendMessage ToggleStruts)
  , ((xmModShift, xK_t),      withFocused $ windows . W.sink)
  , ((alt, xK_Tab),           windows W.focusUp) -- cycle window focus
  , ((altShift, xK_Tab),      windows W.focusDown)
  , ((xmModShift, xK_comma),  sendMessage Shrink) -- "<" == shrink
  , ((xmModShift, xK_period), sendMessage Expand) -- ">" == grow
    -- manage dynamic workspaces
  , ((xmModShift, xK_v),      selectWorkspace def)
  , ((xmMod, xK_m),           withWorkspace def (windows . W.shift))
  , ((xmModShift, xK_m),      withWorkspace def (windows . copy))
  , ((xmModShift, xK_r),      DW.renameWorkspace def)
  , ((xmMod, xK_a),           appendWorkspacePrompt def)
    -- magnify commands
  , ((xmModCtrl, xK_plus),    sendMessage MagnifyMore)
  , ((xmModCtrl, xK_minus),   sendMessage MagnifyLess)
  , ((xmModCtrl, xK_m),       sendMessage Toggle)
    -- minimize
    -- ((xmMod, xK_m), withFocused minimizeWindow)
    -- maximize
    -- ((xmModAlt, xK_m), withFocused (sendMessage . maximizeRestore))
    -- ((xmModShift, xK_m), withLastMinimized maximizeWindowAndFocus)
    -- system
  , ((xmModShift, xK_x),      exit)
  , ((xmMod, xK_q),           kill)
  , ((xmModShift, xK_c),      reloadConfig)
     -- grid select
  , ((xmModShift, xK_w),      windowMenu)
  , ((xmMod, xK_b),           bringSelected def)
  , ((xmMod, xK_g),           goToSelected def)
  , ((xmModShift, xK_o),      gridSelectOpener)
     -- terminals
  , ((xmModShift, xK_Return), runInBash "'cf ~'")
  , ((xmModAlt, xK_Return),   runInBash "'rcd'")
  , ((altShift, xK_Return),   runInBash "'cf /'")
  , ((xmMod, xK_d),           spawn dmenu_run)
  , ((xmMod, xK_o),           defaultCommands >>= runCommand)
  , ((xmMod, xK_c),           editConfig) -- change to dotfiles gridselect
    -- TODO Debug these
  , ((0, xK_Print),           unGrab *> spawn scrot)
  , ((0, xK_Insert),          pasteSelection)
  ]
  ++ -- switch ws between screens
  [ ((m .|. xmMod, k), screenWorkspace sc >>= flip whenJust (windows . f))
         | (k, sc) <- zip [xK_bracketleft, xK_bracketright] [0 ..]
         , (f, m) <- [(W.view, 0), (W.shift, shift)]
  ]
  where
    dmenu_args = ["-b", "-h", "20"]
    dmenu_run = "dmenu_run " ++ unlines dmenu_args ++ " -p 'Yes, Master?'"
    runInBash prog = runInTerm "" ("bash -is eval " ++ prog)
    editConfig = runInTerm "" (nvim ++ " " ++ configFile)
    reloadConfig = spawn "xmonad --recompile; xmonad --restart"

    configFile = "~/.config/xmonad/xmonad.hs"
    scrotFile = "~/pictures/screnshots/%Y-%m-%d-%T-screenshot.png"

    scrot = "sleep 0.2; scrot -sf -q 100 -t 25 " ++ scrotFile




xmStartupHook :: X ()
xmStartupHook = do
  -- start some nice programs
  spawnOnce "xrandr --output DP1 --primary --output HDMI1 --left-of DP1"
  spawnOnce "picom"
  spawnOnce "~/.fehbg"
  spawnOnce "pasystray"
  spawnOnce "blueman-applet"
  spawnOnce "nm-applet --sm-disable --indicator"
  spawnOnce "~/.config/polybar/launch.sh"
  spawnOnce "xscreensaver -no-splash"

-- Note that each layout is separated by ||| which denotes layout choice.

xmLayoutHook =
  mouseResize
    . windowArrange
    . windowNavigation
    . smartBorders
    . avoidStruts
    . magnifier
    $ noBorders Full -- The available layouts start here
      ||| BSP.emptyBSP
      ||| spiral (6 / 7)

xmIcons :: Query [String]
xmIcons =
  composeAll
    [ className =? "Firefox" --> appIcon "\xE745",
      className =? "Spotify" --> appIcon "\xF1BC"
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

xmPolybarLogHook :: D.Client -> X ()
xmPolybarLogHook dbus = xmLogHook <+> dynamicLogWithPP (polybarHook dbus)

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s
        | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
        | otherwise = mempty
      green = "#b8bb26"
      darkgreen = "#98971a"
      red = "#fb4934"
      darkred = "#cc241d"
      orange = "#fabd2f"
      blue = "#2E9AFE"
      gray = "#7F7F7F"
      purple = "#8e7dc3"
      aqua = "#8ec07c"
   in def
        { ppOutput = dbusOutput dbus,
          ppCurrent = wrapper blue,
          ppVisible = wrapper gray,
          ppUrgent = wrapper orange,
          ppHidden = wrapper gray,
          ppWsSep = "",
          ppSep = " : ",
          ppTitle = shorten 100 . wrapper purple
        }

-- create dbus client for connection
makeDbusClient :: IO D.Client
makeDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log") opts
  return dbus
  where
    opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath = D.objectPath_ "/org/xmonad/Log"
      iname = D.interfaceName_ "org.xmonad.Log"
      mname = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body = [D.toVariant $ UTF8.decodeString str]
   in D.emit dbus $ signal {D.signalBody = body}

------------------------------------------------------------------------------

xmWs :: [String]
xmWs = map show [1..9]

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

main' :: D.Client -> IO ()
main' dbus =
  xmonad
    . modal [xmExitMode, xmLaunchMode, xmWorkspaceMode, floatMode 5]
    . ewmhFullscreen
    . ewmh
    . docks
    $ def
      { terminal = term
      , borderWidth = xmBW
      , modMask = xmMod
      , workspaces = xmWs
      , normalBorderColor = xmNBC
      , focusedBorderColor = xmFBC
      , layoutHook = xmLayoutHook
      , logHook = xmPolybarLogHook dbus
      , manageHook = xmManageHook <+> manageHook def
      , handleEventHook = xmEventHook
      , startupHook = xmStartupHook
      , focusFollowsMouse = xmFocusFollowsMouse
      , clickJustFocuses = xmClickJustFocuses
        -- , mouseBindings = xmMouseMap
      }
      `removeKeys` rmKeyPairs
      `additionalKeys` xmKeyMaps

main :: IO ()
main = makeDbusClient >>= main'
