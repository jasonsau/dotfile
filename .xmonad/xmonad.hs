import XMonad
import System.IO
import System.Exit

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks(ToggleStruts(..), avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers(isFullscreen, doFullFloat, doCenterFloat)
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
import XMonad.Layout.Tabbed


myTerminal = "alacritty"
myNormalBorder = "#1D2330"
myFocusBorder = "#e1acff"
myBorderWidth = 2
myModMask = mod4Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myTabTheme = def { fontName            = "xft:Mononoki Nerd Font"
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }
myStartupHook = do
    -- spawnOnce "nitrogen --restore &"
    spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
    -- spawnOnce "variety &"
    spawnOnce "double-screen.sh &"
    spawnOnce "nitrogen --restore &"
    setWMName "LG3D"

myManageHook = composeAll . concat $
    [ [className =? c --> doCenterFloat | c <- myCFloats]
    ]
    where
    myCFloats =  ["Arandr", "Spotify" , "Virtualbox", "Telegram"]

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
-- mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Layouts definition

tall = renamed [Replace "tall"]
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 12
    $ mySpacing 4
    $ ResizableTall 1 (3/100) (1/2) []

magnify = renamed  [Replace "magnify"]
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ magnifier
    $ limitWindows 12
    $ mySpacing 8
    $ ResizableTall 1 (3/100) (1/2) []

monocle = renamed [Replace "monocle"] 
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    -- $ subLayout [] (smartBorders Simplest)
    $ mySpacing 4
    $ limitWindows 20 Full

grid = renamed [Replace "grid"]
    $ windowNavigation
    $ limitWindows 12
    $ mySpacing 4
    $ mkToggle (single MIRROR)
    $ Grid (16 / 10)

threeCol = renamed [Replace "threeCol"]
    $ limitWindows 7
    $ mySpacing 4
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
tabs = renamed [Replace "tabs"]
    $ tabbed shrinkText myTabTheme

floats = renamed [Replace "floats"] $ limitWindows 20 simplestFloat

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font             = "xft:Mononoki Nerd Font:bold:size=60",
      swn_fade             = 1.0,
      swn_bgcolor          = "#1c1f24",
      swn_color            = "#ffffff"
    }


myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

-- Layout hook

myLayoutHook = avoidStruts 
    $ mouseResize
    $ windowArrange
    $ T.toggleLayouts floats
    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = tall
        ||| magnify
        ||| noBorders monocle
        ||| threeCol
        ||| grid
        ||| threeRow
        ||| floats
        ||| spirals
        ||| tabs

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
        -- ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
        -- ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
        ("M-<Up>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
        ("M-<Down>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
        --Application
        ("<XF86Search>", spawn "firefox-bin"),
        ("M-S-p", spawn "pcmanfm"),
        ("M-v", spawn "pavucontrol"),
        ("M-<Return>", spawn myTerminal),
        ("M-S-<Return>", spawn "kitty")
     
    ]

-- myWorkspace= ["www","file","term","code","set","soc","music"]
--       謹        (ﱘ
-- f108
myWorkspace= ["\xe745 ", "\xe5fe ", "\xe795 ", "\xf121 ", "\xf8c5 ", "\xf108 ", "\xe217 ", "\xf001 "]

main :: IO ()
main = do

    xmobarMonitor <- spawnPipe "xmobar -x 0 ~/.config/xmobar/screen0"
    xmobarMonitor1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/screen1"
    xmonad $ ewmh  def{   
        manageHook = (isFullscreen --> doFullFloat) <+>  myManageHook <+> manageDocks <+> insertPosition End Newer,  
        handleEventHook = docksEventHook,
        startupHook = myStartupHook,
        workspaces = myWorkspace,
        modMask = myModMask,
        borderWidth=myBorderWidth,
        terminal = myTerminal,
        layoutHook = showWName' myShowWNameTheme $ myLayoutHook,
        focusedBorderColor = myFocusBorder,
        normalBorderColor = myNormalBorder,
        logHook =workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP{
            ppOutput = \x -> hPutStrLn xmobarMonitor x >> hPutStrLn xmobarMonitor1 x,
            ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]",
            ppVisible = xmobarColor "#98be65" "",
            ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "",
            ppHiddenNoWindows = xmobarColor "#c792ea" "",
            ppTitle = xmobarColor "#b3afc2" "" . shorten 80,
            ppSep = "<fc=#666666> | </fc>",
            ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!",
            ppExtras = [windowCount],
            ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        }
    }`additionalKeysP` myKeyBinding

