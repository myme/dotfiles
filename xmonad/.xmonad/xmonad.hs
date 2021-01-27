import           Control.Monad (when)
import           Data.List (sortOn)
import qualified Data.List as L
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
import           XMonad.Hooks.DynamicLog hiding (wrap)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import           XMonad.Util.Dmenu
import           XMonad.Util.EZConfig (mkKeymap, additionalKeysP)
import           XMonad.Util.Run

data Color = BgDark
           | BgLight
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
myKeys :: XConfig l -> XConfig l
myKeys conf@XConfig { XMonad.terminal = term } = additionalKeysP conf
  -- Quit + kill
  [("M-S-q", confirm "Really quit?" $ io exitSuccess)
  ,("M-w", kill)
  -- Suspend/Lock
  ,("C-M1-l", submap $ mkKeymap conf
     [("l", spawn "loginctl lock-session")
     ,("s", spawn "systemctl suspend")
     ])
  -- Layout
  ,("M-S-m", windows W.swapMaster)
  -- Terminal
  ,("M-<Return>", spawn $ term <> " -e tmux")
  ,("M-S-<Return>", spawn $ XMonad.terminal conf)
  -- Launcher
  ,("M-d", spawn "rofi -show drun")
  ,("M-S-d", spawn "rofi -show combi -combi-modi run,drun")
  ,("M-S-s", spawn "rofi -show ssh")
  ,("M-<Tab>", spawn "rofi -show window")
  ,("M-x", spawn "nixon project")
  ,("M-S-x", spawn "nixon run")
  -- Struts...
  ,("M-S-b", sendMessage $ ToggleStrut D)
    -- Screenshots
  ,("<Print>", spawn "flameshot full -c")
  ,("S-<Print>", spawn "flameshot gui")
  -- Workspaces cycling
  ,("M-p", moveTo Prev NonEmptyWS)
  ,("M-n", moveTo Next NonEmptyWS)
  ,("M-c", moveTo Next EmptyWS)
  ,("M-S-c", shiftTo Next EmptyWS)
  -- Workspaces navigation
  ,("M-1", withNthWorkspace W.greedyView 0)
  ,("M-2", withNthWorkspace W.greedyView 1)
  ,("M-3", withNthWorkspace W.greedyView 2)
  ,("M-4", withNthWorkspace W.greedyView 3)
  ,("M-5", withNthWorkspace W.greedyView 4)
  ,("M-6", withNthWorkspace W.greedyView 5)
  ,("M-7", withNthWorkspace W.greedyView 6)
  ,("M-8", withNthWorkspace W.greedyView 7)
  ,("M-9", withNthWorkspace W.greedyView 8)
  ,("M-0", withNthWorkspace W.greedyView 9)
  ]

-- | Match against start of Query
(=?^) :: Eq a => Query [a] -> [a] -> Query Bool
(=?^) q x = L.isPrefixOf x <$> q

-- | Match against end of Query
(=?$) :: Eq a => Query [a] -> [a] -> Query Bool
(=?$) q x = L.isSuffixOf x <$> q

-- | To float or not to float
myManageHook :: ManageHook
myManageHook = composeAll
  [ className =?  "Barrier"          --> doFloat
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
  let label = pad $ if null cls then tag else unwords (tag : cls)
      key = if tag == "10" then "0" else tag
      addClick = fmt [A BLeft (xdo $ "super+" <> key)]
      addFocus = applyIf (tag == focused) $ fmt [Bg BgLight, Ul Purple]
  in addClick $ addFocus label
  where applyIf p f x | p = f x | otherwise = x

-- | Icons for current layout configuration
layoutIcon :: String -> String
layoutIcon desc | "Full" `L.isInfixOf` desc = Fa.icon Fa.WindowMaximize
                | "Grid" `L.isInfixOf` desc = Fa.icon Fa.Th
                | "Mirror" `L.isInfixOf` desc = Fa.icon Fa.ChevronDown
                | otherwise = Fa.icon Fa.ChevronRight

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
  ]
  where
    (~~>) q i = q --> pure (First $ Just i)
    infix 0 ~~>

myLayout = smartBorders
         $ avoidStruts
         $ spacingRaw True (Border 0 0 0 0) True (Border 5 5 5 5) True baseLayouts
  where baseLayouts = layoutHook def ||| Grid

main :: IO ()
main = do
  logDir <- fromMaybe "/tmp" <$> lookupEnv "XDG_RUNTIME_DIR"
  let workspaceLog = logDir <> "/xmonad.log"
  safeSpawn "mkfifo" [workspaceLog]
  xmonad $ myKeys $ ewmh $ docks def
    { borderWidth        = 2
    , handleEventHook    = handleEventHook def <+> fullscreenEventHook
    , manageHook         = myManageHook <+> manageHook def
    , modMask            = mod4Mask
    , terminal           = "alacritty"
    , normalBorderColor  = "#4c566a"
    , focusedBorderColor = "#bd93f9"
    , layoutHook         = myLayout
    , logHook            = logWorkspaces workspaceLog
    , workspaces         = map show [1 .. 10 :: Int]
    }
