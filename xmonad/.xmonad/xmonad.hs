import           Control.Monad (when)
import qualified Data.List as L
import qualified Data.Map as M
import qualified FontAwesome as Fa
import           System.Exit
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.Submap
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Grid
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import           XMonad.Util.Dmenu


confirm msg action = do
  response <- menuArgs "rofi" ["-dmenu", "-p", msg] ["No", "Yes"]
  when ("Yes" `L.isPrefixOf` response) action


myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.union (M.fromList bindings) (keys def $ conf)
  where bindings = [((modm,               xK_Return), spawn $ XMonad.terminal conf)
                   ,((modm .|. shiftMask, xK_Return), windows W.swapMaster)
                   ,((modm,               xK_d     ), spawn "rofi -show drun")
                   ,((modm .|. shiftMask, xK_d     ), spawn "rofi -show combi -combi-modi run,drun")
                   -- ,((modm .|. shiftMask, xK_s),   spawn "rofi -show ssh")
                   -- ,((modm,               xK_Tab), spawn "rofi -show window")
                   ,((modm,               xK_x), spawn "nixon project")
                   ,((modm .|. shiftMask, xK_x), spawn "nixon run")

                   -- Navigation
                   ,((modm, xK_h), windowGo L True)
                   ,((modm, xK_j), windowGo D True)
                   ,((modm, xK_k), windowGo U True)
                   ,((modm, xK_l), windowGo R True)

                   -- Resize
                   ,((modm .|. shiftMask, xK_h), sendMessage Shrink)
                   ,((modm .|. shiftMask, xK_l), sendMessage Expand)

                   -- Struts...
                   ,((modm .|. shiftMask, xK_b), sendMessage $ ToggleStrut D)

                   -- Quit
                   ,((modm .|. shiftMask, xK_q), confirm "Really quit?" $ io exitSuccess)

                   -- Lock screen
                   ,((controlMask .|. mod1Mask, xK_l), submap $ M.fromList
                      [((0, xK_l), spawn "i3lock -c 000000")
                      ,((0, xK_s), spawn "systemctl suspend")
                      ])

                   -- Workspaces
                   ,((modm .|. shiftMask, xK_equal), addWorkspacePrompt def)
                   ,((modm, xK_BackSpace), removeWorkspace)
                   ,((modm, xK_p), prevWS)
                   ,((modm, xK_n), nextWS)
                   ]
                   ++
                   zip (zip (repeat modm) numberKeys) (map (withNthWorkspace W.greedyView) [0..])
                   -- ++
                   -- zip (zip (repeat modm) [xK_1..xK_9]) (map (addWorkspace . show) [1..])
        numberKeys = [xK_1..xK_9] ++ [xK_0]


myManageHook = composeAll
  [ className =? "Gimp"                  --> doFloat
  , L.isPrefixOf "davmail" <$> className --> doFloat
  , className =? "openconnect-sso"       --> doFloat
  ]


myLayout = avoidStruts
         $ spacingRaw False (Border 0 0 0 0) True (Border 5 5 5 5) True
         $ (layoutHook def ||| Grid)


main :: IO ()
main = xmonad $ ewmh $ docks def
  { borderWidth        = 2
  , handleEventHook    = handleEventHook def <+> fullscreenEventHook
  , manageHook         = myManageHook <+> manageHook def
  , modMask            = mod4Mask
  , terminal           = "alacritty"
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#61afef"
  , layoutHook         = myLayout
  , keys               = myKeys
  , workspaces         = ["1 " <> Fa.icon Fa.Code
                         ,"2 " <> Fa.icon Fa.Terminal
                         ,"3 " <> Fa.icon Fa.Firefox
                         -- ,"4 " <> Fa.icon Fa.Spotify
                         ]
  }
