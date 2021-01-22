import XMonad
import System.IO
import System.Exit

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks(ToggleStruts(..), avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers(isFullscreen, doFullFloat)
import XMonad.Util.EZConfig
import XMonad.Operations(kill)
import XMonad.Actions.WithAll(sinkAll, killAll)
import XMonad.Actions.CycleWS(nextScreen, prevScreen)
import XMonad.StackSet(sink)
import qualified XMonad.StackSet as W
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.WorkspaceHistory(workspaceHistoryHook)
import XMonad.Actions.MouseResize

import XMonad.Layout.Renamed
import XMonad.Layout.ToggleLayouts as T(ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowNavigation
import XMonad.Layout.MultiToggle((??), EOT (EOT), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances(StdTransformers(MIRROR, NBFULL, NOBORDERS)) 
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger(WindowArrangerMsg(..), windowArrange)
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Spacing
import XMonad.Layout.Simplest
import XMonad.Layout.Magnifier
import XMonad.Layout.SubLayouts
import XMonad.Layout.ShowWName
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral



myTerminal = "alacritty"
myNormalBorder = "#1D2330"
myFocusBorder = "#e1acff"
myBorderWidth = 2
myModMask = mod4Mask


myStartupHook = do
    -- spawnOnce "nitrogen --restore &"
    spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
    spawnOnce "variety &"
    setWMName "LG3D"

myManageHook = composeAll
    [className =? "nitrogen" --> doFloat,
     title =? "Telegram Desktop" --> doFloat,
     className =? "com.spotify.Client" --> doFloat
    ]
--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Layouts definition

tall = renamed [Replace "tall"]
    $ windowNavigation
    $ limitWindows 12
    $ mySpacing 4
    $ ResizableTall 1 (3 / 100) (1 / 2) []

monocle = renamed [Replace "monocle"] $ limitWindows 20 Full

grid = renamed [Replace "grid"]
    $ windowNavigation
    $ limitWindows 12
    $ mySpacing 4
    $ mkToggle (single MIRROR)
    $ Grid (16 / 10)

threeCol = renamed [Replace "threeCol"]
    $ limitWindows 7
    $ mySpacing' 4
    $ ThreeCol 1 (3 / 100) (1 / 3)

threeRow = renamed [Replace "threeRow"]
    $ windowNavigation
    $ limitWindows 15
    $ mySpacing 4
    $ Mirror
    $ ThreeCol 1 (3/100)(1/2)

spirals = renamed [Replace "spirals"]
    $ windowNavigation
    $ mySpacing 8
    $ spiral (6/7)

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
        tall
        |||noBorders monocle
        ||| threeCol
        ||| grid
        ||| threeRow
        ||| floats
        ||| spirals

myKeyBinding = 
    [
        --Menu
        ("M-S-m", spawn "rofi -show drun"),
        ("M-S-n", spawn "rofi -show window"),

        --Restart and Recompile
        ("M-C-r", spawn "xmonad --recompile"),
        ("M-S-r", spawn "xmonad --restart"),
        --Monitor
        ("M-.", nextScreen),
        ("M-,", prevScreen),
        --General Functions
        ("M-S-f", withFocused $windows . W.sink),
        ("M-w", kill), ("M-S-w", killAll),
        --Move Windows
        ("M-j", windows W.focusDown),
        ("M-k", windows W.focusUp),
        ("M-S-j", windows W.swapDown),
        ("M-S-k", windows W.swapUp),
        ("M-h", sendMessage Shrink),
        ("M-l", sendMessage Expand),
        ("M-S-h", sendMessage MirrorShrink),
        ("M-S-l", sendMessage MirrorExpand),
        ("M-<Tab>", sendMessage NextLayout),
        ("M-<Space>", sendMessage(T.Toggle "floats")),

        --Volumen
        ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
        ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
        --Application
        ("<XF86Search>", spawn "firefox-bin"),
        ("M-S-p", spawn "pcmanfm"),
        ("M-v", spawn "pavucontrol"),
        ("M-<Return>", spawn myTerminal),
        ("M-S-<Return>", spawn "kitty")
     
    ]

myWorkspace= ["www","file","term","code","set","soc","music"]
-- myWorkspace= ["","","","","","","ﱘ"]

main :: IO ()
main = do

    xmobarMonitor <- spawnPipe "xmobar -x 0 ~/.config/xmobar/screen0"
    xmonad $ ewmh  def{   
        manageHook = (isFullscreen --> doFullFloat) <+> manageDocks <+> myManageHook <+> insertPosition End Newer,  
        handleEventHook = docksEventHook,
        startupHook = myStartupHook,
        workspaces = myWorkspace,
        modMask = myModMask,
        borderWidth=myBorderWidth,
        terminal = myTerminal,
        layoutHook = myLayoutHook,
        focusedBorderColor = myFocusBorder,
        normalBorderColor = myNormalBorder,
        logHook =workspaceHistoryHook <+> dynamicLogWithPP xmobarPP{
            ppOutput = \x -> hPutStrLn xmobarMonitor x,
            ppCurrent = xmobarColor "#98be65" "" . wrap "[""]",
            ppVisible = xmobarColor "#98be65" "",
            ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "",
            ppHiddenNoWindows = xmobarColor "#c792ea" "",
            ppTitle = xmobarColor "#b3afc2" "" . shorten 80
        }
    }`additionalKeysP` myKeyBinding

