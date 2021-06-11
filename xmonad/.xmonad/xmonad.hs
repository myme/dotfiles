import           Control.Monad (when)
import           Data.List (sortOn)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (First(getFirst, First))
import qualified FontAwesome as Fa
import           System.Environment (lookupEnv)
import           System.Exit
import           Text.Read (readMaybe)
import           XMonad hiding (Color)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.Submap
import           XMonad.Actions.SwapWorkspaces (swapTo)
import           XMonad.Hooks.DynamicLog hiding (wrap)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import qualified XMonad.Layout.BoringWindows as BW
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import qualified XMonad.Layout.ThreeColumns as Three
import           XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import           XMonad.Util.Dmenu
import           XMonad.Util.EZConfig (mkKeymap, additionalKeysP)
import           XMonad.Util.Run

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
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.terminal = term } = mkKeymap conf (
  -- Quit + kill
  [("M-S-q", confirm "Really quit?" $ io exitSuccess)
  ,("M-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  ,("M-S-w", kill)
  -- Suspend/Lock
  ,("C-M1-l", submap $ mkKeymap conf
     [("l", spawn "loginctl lock-session")
     ,("s", spawn "systemctl suspend")
     ])
  -- Layout
  ,("M-<Space>", sendMessage NextLayout)
  ,("M-S-<Space>", setLayout $ XMonad.layoutHook conf)
  ,("M-r", refresh)
  -- Master area
  ,("M-S-m", windows W.swapMaster)
  ,("M-S-h", sendMessage Shrink)
  ,("M-S-l", sendMessage Expand)
  ,("M-S-j", sendMessage MirrorShrink)
  ,("M-S-k", sendMessage MirrorExpand)
  ,("M-,", sendMessage (IncMasterN 1))
  ,("M-.", sendMessage (IncMasterN (-1)))
  -- Float + Tiling
  ,("M-t", withFocused $ windows . W.sink)
  -- Focus + Swap
  ,("M-m", windows W.focusMaster)
  ,("M-j", BW.focusDown)
  ,("M-k", BW.focusUp)
  ,("M-S-f", windows W.swapDown)
  ,("M-S-b", windows W.swapUp)
  -- Spaces
  ,("M-=", incWindowSpacing 5)
  ,("M--", decWindowSpacing 5)
  ,("M-S-=", incScreenSpacing 5)
  ,("M-S--", decScreenSpacing 5)
  -- Terminal
  ,("M-<Return>", spawn $ term <> " -e tmux")
  ,("M-S-<Return>", spawn term)
  -- Launcher
  ,("M-d", spawn "rofi -show drun -show-icons")
  ,("M-S-d", spawn "rofi -show combi -combi-modi run,drun")
  ,("M-s", spawn "rofi -show ssh")
  ,("M-S-s", spawn "rofi -show ssh -ssh-command '{terminal} -e mosh {host}'")
  ,("M-<Tab>", spawn "rofi -show window -show-icons")
  ,("M-x", spawn "nixon project")
  ,("M-S-x", spawn "nixon run")
  ,("M-e", spawn "rofimoji")
  -- Struts...
  ,("M-b", sendMessage $ ToggleStrut D)
    -- Screenshots
  ,("<Print>", spawn "flameshot full -c")
  ,("S-<Print>", spawn "flameshot gui")
  -- Workspaces cycling
  ,("M-p", moveTo Prev NonEmptyWS)
  ,("M-n", moveTo Next NonEmptyWS)
  ,("M-S-p", swapTo Prev)
  ,("M-S-n", swapTo Next)
  ,("M-c", moveTo Next EmptyWS)
  ,("M-S-c", shiftTo Next EmptyWS)
  -- Sublayouts
  , ("M-C-h", sendMessage $ pushGroup L)
  , ("M-C-l", sendMessage $ pushGroup R)
  , ("M-C-k", sendMessage $ pushGroup U)
  , ("M-C-j", sendMessage $ pushGroup D)
  , ("M-C-,", onGroup W.focusUp')
  , ("M-C-.", onGroup W.focusDown')
  , ("M-C-m", withFocused (sendMessage . MergeAll))
  , ("M-C-u", withFocused (sendMessage . UnMerge))
  , ("M-C-/", withFocused (sendMessage . UnMergeAll))
  -- Audio controls
  ,("<XF86AudioRaiseVolume>", spawn "amixer set Master 1%+")
  ,("<XF86AudioLowerVolume>", spawn "amixer set Master 1%-")
  ,("<XF86AudioMute>", spawn "amixer set Master toggle")
  ] ++
  -- Workspaces navigation
  -- M-[1..9]   => Switch to workspace N
  -- M-S-[1..9] => Move client to workspace N
  [(m <> [k], windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'] <> ['0'])
    , (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]]
  )

-- | Match against start of Query
(=?^) :: Eq a => Query [a] -> [a] -> Query Bool
(=?^) q x = L.isPrefixOf x <$> q

-- | Match against end of Query
(=?$) :: Eq a => Query [a] -> [a] -> Query Bool
(=?$) q x = L.isSuffixOf x <$> q

-- | To float or not to float
myManageHook :: ManageHook
myManageHook = composeAll
  [ doF W.swapDown -- Insert new windows *after* the focused window
  , className =?  "Barrier"          --> doFloat
  , className =?^ "Gimp"             --> doFloat
  , className =?  "Gnome-calculator" --> doFloat
  , className =?^ "davmail"          --> doFloat
  , className =?  "openconnect-sso"  --> doFloat
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
    wrap (Ul color)  t = concat ["%{u", show color, "}", t, "%{-u}"]
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
                | "Tall" `L.isInfixOf` desc = Fa.icon Fa.ChevronRight
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
  , className =?  "Emacs"            ~~> Fa.Code
  , className =?  "Evince"           ~~> Fa.FilePdfO
  , className =?  "Firefox"          ~~> Fa.Firefox
  , className =?^ "Gimp"             ~~> Fa.PaintBrush
  , className =?  "Gnome-calculator" ~~> Fa.Calculator
  , className =?$ "Nautilus"         ~~> Fa.FolderOpen
  , className =?  "openshot"         ~~> Fa.VideoCamera
  , className =?  "Peek"             ~~> Fa.Camera
  , className =?  "qutebrowser"      ~~> Fa.Compass
  , className =?  "Seahorse"         ~~> Fa.Lock
  , className =?  "Spotify"          ~~> Fa.Spotify
  , title     =?  "st"               ~~> Fa.Terminal
  , className =?  "vlc"              ~~> Fa.YoutubePlay
  , className =?  "webex"            ~~> Fa.Comment
  ]
  where
    (~~>) q i = q --> pure (First $ Just i)
    infix 0 ~~>

myLayout = BW.boringWindows
         $ tall ||| three ||| full ||| grid
  where tall = smartBorders
             $ avoidStruts
             $ configurableNavigation noNavigateBorders
             $ addTabs shrinkText tabTheme
             $ subLayout [] Simplest
             $ spacingRaw False (Border 20 20 20 20) True (Border 20 20 20 20) True
             $ ResizableTall 1 (3/100) (1/2) []
        three = smartBorders
              $ avoidStruts
              $ configurableNavigation noNavigateBorders
              $ addTabs shrinkText tabTheme
              $ subLayout [] Simplest
              $ spacingRaw False (Border 20 20 20 20) True (Border 20 20 20 20) True
              $ Three.ThreeCol 1 (3/100) (1/3)
        full = noBorders
             $ avoidStruts
               Full
        grid = smartBorders
             $ avoidStruts
             $ addTabs shrinkText tabTheme
             $ subLayout [] Simplest
             $ spacingRaw True (Border 0 0 0 0) True (Border 5 5 5 5) True
               Grid
        tabTheme = def
                 { fontName            = "xft:Dejavu Sans Mono for Powerline:regular:size=12:antialias=true:hinting=true"
                 , activeColor         = show Blue
                 , inactiveColor       = show BgDark
                 , activeBorderColor   = show BgDark
                 , inactiveBorderColor = show BgDark
                 , activeTextColor     = show BgDark
                 , inactiveTextColor   = show FgDark
                 }

main :: IO ()
main = do
  logDir <- fromMaybe "/tmp" <$> lookupEnv "XDG_RUNTIME_DIR"
  let workspaceLog = logDir <> "/xmonad.log"
  safeSpawn "mkfifo" [workspaceLog]
  xmonad $ ewmh $ docks def
    { borderWidth        = 2
    , handleEventHook    = handleEventHook def <+> fullscreenEventHook
    , manageHook         = myManageHook <+> manageHook def
    , modMask            = mod4Mask
    , terminal           = "alacritty"
    , normalBorderColor  = show BgLight
    , focusedBorderColor = show Blue
    , layoutHook         = myLayout
    , logHook            = logWorkspaces workspaceLog
    , keys               = myKeys
    , workspaces         = map show [1 .. 10 :: Int]
    }
