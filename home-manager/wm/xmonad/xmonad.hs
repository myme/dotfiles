{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
import           Control.Monad (void, when)
import           Data.List (sortOn)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Monoid (First(getFirst, First))
import qualified FontAwesome as Fa
import           System.Environment (lookupEnv)
import           System.Exit
import           Text.Read (readMaybe)
import qualified Variables as Vars
import           XMonad hiding (Color)
import           XMonad.Actions.CopyWindow (copyToAll, kill1, killAllOtherCopies)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.Submap
import           XMonad.Actions.SwapWorkspaces (swapTo)
import           XMonad.Hooks.DynamicLog hiding (wrap)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import qualified XMonad.Layout.BoringWindows as BW
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import qualified XMonad.Layout.ThreeColumns as Three
import           XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import           XMonad.Util.Dmenu
import           XMonad.Util.EZConfig (mkKeymap, additionalKeysP, mkNamedKeymap)
import           XMonad.Util.NamedActions (noName, addName, submapName, NamedAction, addDescrKeys', showKm)
import           XMonad.Util.Run
import           XMonad.Actions.Warp (warpToScreen, warpToWindow)

data Color = BgDark
           | BgLight
           | FgDark
           | FgLight
           | BrightGreen
           | DimGreen
           | DimBlue
           | Blue
           | Red
           | Orange
           | Yellow
           | Green
           | Purple

-- | Theme: Dracula
instance Show Color where
  show BgDark      = "#282a36"
  show BgLight     = "#3b4252"
  show FgDark      = "#e5e9f0"
  show FgLight     = "#f8f8f2"
  show BrightGreen = "#5af78e"
  show DimGreen    = "#1ef956"
  show DimBlue     = "#4d5b86"
  show Blue        = "#bd93f9"
  show Red         = "#ff5555"
  show Orange      = "#d08770"
  show Yellow      = "#f1fa8c"
  show Green       = "#50fa7b"
  show Purple      = "#ff79c6"

data Btn = BLeft | BMiddle | BRight | BUp | BDown | BLeftDbl | BMiddleDbl | BRightDbl
  deriving Enum

btnIdx :: Btn -> Int
btnIdx = (1+) . fromEnum

data Fmt = Fg Color
         | Bg Color
         | Ul Color
         | A Btn String

confirm :: String -> X () -> X ()
confirm msg action = do
  response <- menuArgs "rofi" ["-dmenu", "-p", msg] ["No", "Yes"]
  when ("Yes" `L.isPrefixOf` response) action

-- | Keybindings
myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf@XConfig { XMonad.terminal = term } = mkNamedKeymap conf (
  -- Quit + kill
  [("M-S-q", addName "Quit XMonad" $ confirm "Really quit?" $ io exitSuccess)
  ,("M-q",   addName "Reload XMonad" $ spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  ,("M-S-w", addName "Close window" kill1)
  -- Suspend/Lock
  ,("C-M1-l", submapName $ mkNamedKeymap conf
     [("l", addName "Lock session" $ spawn "loginctl lock-session")
     ,("h", addName "Hibernate" $ spawn "systemctl hibernate")
     ,("s", addName "Suspend" $ spawn "systemctl suspend")
     ])
  -- Layout
  ,("M-<Space>",   addName "Next layout" $ sendMessage NextLayout)
  ,("M-S-<Space>", addName "Reset layout" $ setLayout $ XMonad.layoutHook conf)
  ,("M-r",         addName "Refresh XMonad layout" refresh)
  -- Master area
  ,("M-S-m", addName "Swap master"  $ windows W.swapMaster)
  ,("M-S-h", addName "Resize left"  $ sendMessage Shrink)
  ,("M-S-l", addName "Resize right" $ sendMessage Expand)
  ,("M-S-j", addName "Resize up"    $ sendMessage MirrorShrink)
  ,("M-S-k", addName "Resize down"  $ sendMessage MirrorExpand)
  ,("M-,",   addName "Increase master windows" $ sendMessage (IncMasterN 1))
  ,("M-.",   addName "Decrease master windows" $ sendMessage (IncMasterN (-1)))
  -- Float + Tiling + Sticky
  ,("M-t",   addName "Make focused window tiling" $ withFocused $ windows . W.sink)
  ,("M-S-e", addName "Copy window to all workspaces" $ windows copyToAll)
  ,("M-S-r", addName "Kill all other window copies" killAllOtherCopies)
  -- Focus + Swap
  ,("M-m",   addName "Focus master window" $ windows W.focusMaster)
  ,("M-j",   addName "Focus next window" BW.focusDown)
  ,("M-k",   addName "Focus previous window" BW.focusUp)
  ,("M-S-f", addName "Swap focused with next" $ windows W.swapDown)
  ,("M-S-b", addName "Swap focused with previous" $ windows W.swapUp)
  -- Spaces
  ,("M-=",   addName "Increase window spacing" $ incWindowSpacing 5)
  ,("M--",   addName "Decrease window spacing" $ decWindowSpacing 5)
  ,("M-S-=", addName "Increase screen spacing" $ incScreenSpacing 5)
  ,("M-S--", addName "Decrease screen spacing" $ decScreenSpacing 5)
  ,("M-w",   addName "Toggle smart spacing" toggleSmartSpacing)
  -- Terminal
  ,("M-<Return>", addName "Spawn tmux terminal" $ spawn $ term <> " -e tmux")
  ,("M-S-<Return>", addName "Spawn terminal" $ spawn term)
  -- Launcher
  ,("M-d",     addName "Launch application" $ spawn "rofi -show drun -show-icons")
  ,("M-S-d",   addName "Launch command" $ spawn "rofi -show combi -combi-modi run,drun")
  ,("M-s",     addName "Launch SSH to host" $ spawn "rofi -show ssh")
  ,("M-S-s",   addName "Launch mosh to host" $ spawn "rofi -show ssh -ssh-command '{terminal} -e mosh {host}'")
  ,("M-<Tab>", addName "Select window" $ spawn "rofi -show window -show-icons")
  ,("M-x",     addName "Nixon command" $ spawn "nixon run")
  ,("M-S-x",   addName "Nixon project command" $ spawn "nixon project")
  ,("M-e",     addName "Rofimoji (Emoji picker)" $ spawn "rofimoji")
  -- Mouse warp
  ,("M-S-.", addName "Warp mouse to screen" $ warpToWindow 0.5 0.5)
  -- Struts...
  ,("M-b", addName "Toggle struts (statusbar)" $ sendMessage $ ToggleStrut D)
    -- Screenshots
  ,("<Print>",   addName "Capture screenshot" $ spawn "flameshot full -c")
  ,("S-<Print>", addName "Launch screenshot app" $ spawn "flameshot gui")
  -- Workspaces cycling
  ,("M-p",   addName "Move to next workspace" $ moveTo Prev NonEmptyWS)
  ,("M-n",   addName "Move to previous workspace" $ moveTo Next NonEmptyWS)
  ,("M-S-p", addName "Swap workspace with next" $ swapTo Prev)
  ,("M-S-n", addName "Swap workspace with previous" $ swapTo Next)
  ,("M-c",   addName "Select first empty workspace" $ moveTo Next EmptyWS)
  ,("M-S-c", addName "Move window to next empty workspace" $ shiftTo Next EmptyWS)
  -- Sublayouts
  , ("M-C-h", addName "Tab group windows left" $ sendMessage $ pushGroup L)
  , ("M-C-l", addName "Tab group windows right" $ sendMessage $ pushGroup R)
  , ("M-C-k", addName "Tab group windows up" $ sendMessage $ pushGroup U)
  , ("M-C-j", addName "Tab group windows down" $ sendMessage $ pushGroup D)
  , ("M-C-,", addName "Focus previous tab" $ onGroup W.focusUp')
  , ("M-C-.", addName "Focus next tab" $ onGroup W.focusDown')
  , ("M-C-m", addName "Tab group all" $ withFocused (sendMessage . MergeAll))
  , ("M-C-u", addName "Tab ungroup window" $ withFocused (sendMessage . UnMerge))
  , ("M-C-/", addName "Tab ungroup all" $ withFocused (sendMessage . UnMergeAll))
  -- , ("M-C-c", addName "Move tab group to next empty workspace" $ onGroup (shiftTo Next EmptyWS))
  -- Audio controls
  ,("<XF86AudioRaiseVolume>", addName "Audio volume up" $ spawn "amixer set Master 1%+")
  ,("<XF86AudioLowerVolume>", addName "Audio volume down" $ spawn "amixer set Master 1%-")
  ,("<XF86AudioMute>", addName "Audio volume mute" $ spawn "amixer set Master toggle")
  ,("<XF86AudioMicMute>", addName "Audio mic mute" $ spawn "amixer set Capture toggle")
  ] ++
  -- Workspaces navigation
  -- M-[1..9]   => Switch to workspace N
  -- M-S-[1..9] => Move client to workspace N
  [(m <> [k], addName (d " " <> [k]) $ windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'] <> ['0'])
    , (d, f, m) <- [(("Switch to workspace" <>), W.greedyView, "M-"), (("Move to workspace" <>), W.shift, "M-S-")]]
  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  (
    let withScreen screenId f = do
          warpToScreen screenId 0.5 0.5
          void $ fmap (windows . f) <$> screenWorkspace screenId
    in [("M-" <> m <> k, addName (d <> " " <> show (s + 1)) $ withScreen s f)
      | (k, s) <- zip ["u", "i", "o"] ([0..] :: [ScreenId])
      , (d, f, m) <- [("Switch to screen", W.view, ""), ("Move to screen", W.shift, "S-")]]
  ))

-- | Match against start of Query
(=?^) :: Eq a => Query [a] -> [a] -> Query Bool
(=?^) q x = L.isPrefixOf x <$> q

-- | Match against end of Query
(=?$) :: Eq a => Query [a] -> [a] -> Query Bool
(=?$) q x = L.isSuffixOf x <$> q

-- | To float or not to float
myManageHook :: ManageHook
myManageHook = composeAll
  [ isNothing <$> transientTo        --> doF W.swapDown -- Insert new windows *after* the focused window
  , className =?  "Barrier"          --> doFloat
  , className =?  "Conky"            --> doFloat
  , className =?^ "Gimp"             --> doFloat
  , className =?  "Gnome-calculator" --> doFloat
  , className =?^ "davmail"          --> doFloat
  , className =?  "openconnect-sso"  --> doFloat
  , className =?  "plasmashell"      --> doIgnore
  , className =?  "Plasma"           --> doIgnore
  ]

myLog :: FilePath -> PP
myLog logfile = def
  { ppOutput = appendFile logfile . (<> "\n")
  }

fmt :: [Fmt] -> String -> String
fmt []     txt = txt
fmt (f:fs) txt = fmt fs (wrap f txt)
  where
    wrap (Fg color)  t = concat ["%{F", show color, "}", t, "%{F-}"]
    wrap (Bg color)  t = concat ["%{B", show color, "}", t, "%{B-}"]
    wrap (Ul color)  t = concat ["%{u", show color, "}%{+u}", t, "%{-u}"]
    wrap (A btn cmd) t = concat ["%{A", show $ btnIdx btn, ":", cmd, ":}", t, "%{A}"]

xdo :: String -> String
xdo = ("xdotool key " <>)

-- | Add application icons to workspace list
logWorkspaces :: FilePath -> X ()
logWorkspaces logfile = do
  layout <- description . W.layout . W.workspace . W.current . windowset <$> get
  ws <- sortOn (readInt . W.tag) . W.workspaces . windowset <$> get
  wsIcons <- mapM workspaceIcons ws
  focused <- W.tag . W.workspace . W.current . windowset <$> get
  let focusedOrNonEmpty (tag, icons) = tag == focused || not (null icons)
      workspaces = unwords $ map (formatWS focused) $ filter focusedOrNonEmpty wsIcons
      wsStr = unwords [formatLayout layout, addScroll workspaces]
  io $ appendFile logfile (wsStr <> "\n")
  where readInt = readMaybe :: String -> Maybe Int
        addScroll = fmt [A BUp $ xdo "super+n", A BDown $ xdo "super+p"]

formatLayout :: String -> String
formatLayout = fmt [A BLeft $ xdo "super+space", Ul DimGreen] . pad . layoutIcon

formatWS :: String -> (String, [String]) -> String
formatWS focused (tag, cls) =
  let key = if tag == "10" then "0" else tag
  in addClick key $ addFocus $ pad $ unwords $ tag : cls
  where addFocus = applyIf (tag == focused) $ fmt [Bg BgLight, Ul Purple]
        addClick key = fmt [A BLeft (xdo $ "super+" <> key)]
        applyIf p f x | p = f x | otherwise = x

-- | Icons for current layout configuration
layoutIcon :: String -> String
layoutIcon desc | "Full" `L.isInfixOf` desc = Fa.icon Fa.WindowMaximize
                | "Three" `L.isInfixOf` desc = Fa.icon Fa.Columns
                | "Grid" `L.isInfixOf` desc = Fa.icon Fa.Th
                | "Mirror" `L.isInfixOf` desc = Fa.icon Fa.ChevronDown
                | "Tall" `L.isInfixOf` desc = if "ReflectX" `L.isInfixOf` desc
                  then Fa.icon Fa.ChevronLeft
                  else Fa.icon Fa.ChevronRight
                | otherwise = Fa.icon Fa.Question

workspaceIcons :: W.Workspace WorkspaceId l Window  -> X (String, [String])
workspaceIcons workspace = case W.stack workspace of
  Nothing -> pure (tag, [])
  Just stack -> do
    let wins = W.integrate stack
    cls <- mapM (fmap Fa.icon <$> appIcon) wins
    pure (tag, cls)
  where tag = W.tag workspace

-- | Window class/title mapping to icon
appIcon :: Window -> X Fa.FontAwesome
appIcon = runQuery $ fromMaybe Fa.WindowMaximize . getFirst <$> composeAll
  [ className =?  "Alacritty"        ~~> Fa.Terminal
  , className =?  "Bitwarden"        ~~> Fa.Shield
  , className =?^ "Chromium"         ~~> Fa.Chrome
  , className =?  "Code"             ~~> Fa.Code
  , className =?  "discord"          ~~> Fa.Comment
  , className =?  "Emacs"            ~~> Fa.Code
  , className =?  "Evince"           ~~> Fa.FilePdfO
  , className =?^ "firefox"          ~~> Fa.Firefox
  , className =?^ "Gimp"             ~~> Fa.PaintBrush
  , className =?  "Gnome-calculator" ~~> Fa.Calculator
  , className =?$ "Nautilus"         ~~> Fa.FolderOpen
  , className =?  "openshot"         ~~> Fa.VideoCamera
  , className =?  "Peek"             ~~> Fa.Camera
  , className =?  "qutebrowser"      ~~> Fa.Compass
  , className =?  "Seahorse"         ~~> Fa.Lock
  , className =?  "Signal"           ~~> Fa.Comment
  , className =?  "Slack"            ~~> Fa.Slack
  , className =?  "Spotify"          ~~> Fa.Spotify
  , title     =?  "st"               ~~> Fa.Terminal
  , className =?  "vlc"              ~~> Fa.YoutubePlay
  , className =?  "webex"            ~~> Fa.Comment
  ]
  where
    (~~>) q i = q --> pure (First $ Just i)
    infix 0 ~~>

myLayout = BW.boringWindows
         $ tall ||| full ||| reflect ||| three ||| grid
  where tall = smartBorders
             $ avoidStruts
             $ configurableNavigation noNavigateBorders
             $ addTabs shrinkText tabTheme
             $ subLayout [] Simplest
             $ spacingRaw Vars.smartBorder (Border spc spc spc spc) True (Border spc spc spc spc) True
             $ ResizableTall 1 (3/100) (1/2) []
        full = noBorders
             $ avoidStruts
               Full
        reflect = reflectHoriz tall
        three = smartBorders
              $ avoidStruts
              $ configurableNavigation noNavigateBorders
              $ addTabs shrinkText tabTheme
              $ subLayout [] Simplest
              $ spacingRaw Vars.smartBorder (Border spc spc spc spc) True (Border spc spc spc spc) True
              $ Three.ThreeCol 1 (3/100) (1/3)
        grid = smartBorders
             $ avoidStruts
             $ addTabs shrinkText tabTheme
             $ subLayout [] Simplest
             $ spacingRaw True (Border 0 0 0 0) True (Border 5 5 5 5) True
               Grid
        spc = Vars.spaces
        tabTheme = def
                 { fontName            = Vars.fontName
                 , activeColor         = show Blue
                 , inactiveColor       = show BgDark
                 , activeBorderColor   = show BgDark
                 , inactiveBorderColor = show BgDark
                 , activeTextColor     = show BgDark
                 , inactiveTextColor   = show FgDark
                 }

rofiBindings :: [((KeyMask, KeySym), NamedAction)] -> X ()
rofiBindings bindings = do
  handle <- spawnPipe "rofi -dmenu -i"
  liftIO $ hPutStrLn handle (unlines $ showKm bindings)

main :: IO ()
main = do
  logDir <- fromMaybe "/tmp" <$> lookupEnv "XDG_RUNTIME_DIR"
  let workspaceLog = logDir <> "/xmonad.log"
  safeSpawn "mkfifo" [workspaceLog]
  xmonad
    $ ewmh
    $ docks
    $ addDescrKeys' ((mod4Mask, xK_F1), rofiBindings) myKeys
    $ def
    { borderWidth        = 2
    , handleEventHook    = handleEventHook def <+> fullscreenEventHook
    , manageHook         = myManageHook <+> manageHook def
    , modMask            = mod4Mask
    , terminal           = "alacritty"
    , normalBorderColor  = show BgLight
    , focusedBorderColor = show Blue
    , layoutHook         = myLayout
    , logHook            = logWorkspaces workspaceLog
    , workspaces         = map show [1 .. 10 :: Int]
    }
