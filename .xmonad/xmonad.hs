--Base
import Data.Monoid
import Data.Tree
import XMonad

--Hooks
import XMonad.Hooks.ManageDocks(avoidStruts, ToggleStruts(..), manageDocks, docksEventHook)
import XMonad.Hooks.EwmhDesktops
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.SetWMName

--Utilities
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
-- import System.Exit
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Hooks.FadeInactive
import qualified XMonad.StackSet as W
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)

--Layout
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle ((??), EOT (EOT), mkToggle, single)
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)

--Actions
import XMonad.Actions.MouseResize
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow(kill1)
import XMonad.Actions.WithAll(killAll, sinkAll)


--Variables

myTerminal :: String
myTerminal="alacritty"
myBorderWidth =2
myNormalBorderColor = "#1D2330"
myFocusedBorderColor ="#e1acff"
myKeybindings =[
                    --Menu Rofi
                    ("M-S-m", spawn "rofi -show drun -show-icons"),
                    ("M-S-n", spawn "rofi -show window -show-icons"),
                    ("M-<Return>", spawn myTerminal),
                    ("<XF86Search>", spawn "firefox-bin"),
                    ("M-h", sendMessage Expand),
                    ("M-l", sendMessage Shrink),
                    ("M-w", kill1),
                    ("M-S-w", killAll),
                    ("M-j", windows W.focusDown),
                    ("M-k", windows W.focusUp),
                    ("M-<Tab>", sendMessage NextLayout),
                    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
                    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
                    ("M-.", nextScreen),
                    ("M-,", prevScreen),
                    ("M-S-j", windows W.swapDown),
                    ("M-S-k", windows W.swapUp),
                    ("M-S-q", io exitSuccess)
               ]


--WorkkSpaces
myWorkspaces :: [String]
myWorkspaces = ["www", "dev", "term", "ref", "sys", "fs", "img", "vid", "misc"]


-- mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- -- Single window with no gaps
-- mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Layouts definition
tall = renamed [Replace "tall"]
    $ limitWindows 12
    -- $ mySpacing 4
    $ ResizableTall 1 (3 / 100) (1 / 2) []

monocle = renamed [Replace "monocle"] $ limitWindows 20 Full

grid = renamed [Replace "grid"]
    $ limitWindows 12
    -- $ mySpacing 4
    $ mkToggle (single MIRROR)
    $ Grid (16 / 10)

threeCol = renamed [Replace "threeCol"]
    $ limitWindows 7
    -- $ mySpacing' 4
    $ ThreeCol 1 (3 / 100) (1 / 3)

floats = renamed [Replace "floats"] $ limitWindows 20 simplestFloat

-- Layout hook

myLayoutHook = avoidStruts 
    $ smartBorders
    $ mouseResize
    $ windowArrange
    $ T.toggleLayouts floats
    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = 
        noBorders monocle
        ||| tall
        ||| threeCol
        ||| grid


myStartupHook :: X ()
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom &"
    spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 16 &" 
    setWMName "LG3D"



myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.95

main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 /home/js/.config/xmobar/xmobarrc0"
    xmonad $ ewmh def
        {
            manageHook =(isFullscreen --> doFullFloat) <+> manageDocks,
            handleEventHook = handleEventHook def <+> fullscreenEventHook <+> docksEventHook,
            terminal = myTerminal,
            borderWidth = myBorderWidth,
            normalBorderColor = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,
            startupHook = myStartupHook,
            workspaces = myWorkspaces,
            layoutHook = myLayoutHook,
            logHook =myLogHook <+> workspaceHistoryHook <+> dynamicLogWithPP xmobarPP 
                {
                    ppOutput = \x -> hPutStrLn xmproc0 x,
                    ppCurrent =xmobarColor "#98be65" "" . wrap "[" "]",
                    ppVisible = xmobarColor "#98be65" "",
                    ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "",
                    ppHiddenNoWindows = xmobarColor "#c792ea" "",
                    ppTitle = xmobarColor "#b3afc2" "" . shorten 60,
                    ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
                }

        }
        `additionalKeysP` myKeybindings

