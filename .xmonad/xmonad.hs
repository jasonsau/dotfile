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
import XMonad.Hooks.ManageDocks
import XMonad.Actions.MouseResize
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)


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


import XMonad.Util.NamedActions

import Data.Monoid
import Data.Char (isSpace, toUpper)
import Data.Maybe (isJust)


myTerminal :: String
myTerminal = "alacritty"

myNormalBorder :: String
myNormalBorder = "#1D2330"

myFocusBorder :: String
myFocusBorder = "#e1acff"

myBorderWidth :: Dimension
myBorderWidth = 2

myModMask :: KeyMask
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
                 

myStartupHook :: X ()
myStartupHook = do

    spawnOnce "nitrogen --restore &"

    spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"

    spawnOnce "/home/js/.local/bin/autostart.sh &"
    setWMName "LG3D"


myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)

myManageHook = composeAll . concat $
    [ [className =? c --> doCenterFloat | c <- myCFloats]
    ]
    where
    myCFloats =  ["Arandr", "Spotify" , "Virtualbox", "TelegramDesktop"]

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
-- mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Layouts definition

tall = renamed [Replace "tall"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 12
    $ mySpacing 4
    $ ResizableTall 1 (3/100) (1/2) []

-- magnifier = renamed  [Replace "magnify"]
--    $ smartBorders
--    $ windowNavigation
--    $ addTabs shrinkText myTabTheme
--    $ subLayout [] (smartBorders Simplest)
--    $ limitWindows 12
--    $ mySpacing 8
--    $ ResizableTall 1 (3/100) (1/2) []

monocle = renamed [Replace "monocle"] 
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ mySpacing 4
    $ limitWindows 20 
    $ Full

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

floats = renamed [Replace "floats"] 
    $ smartBorders
    $ simplestFloat

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
    myDefaultLayout = withBorder myBorderWidth tall
        ||| noBorders monocle
        ||| threeCol
        ||| grid
        ||| threeRow
        ||| floats
        ||| spirals
        ||| tabs



showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  --hPutStr h (unlines $ showKm x) -- showKM adds ">>" before subtitles
  hPutStr h (unlines $ showKmSimple x) -- showKmSimple doesn't add ">>" to subtitles
  hClose h
  return ()


subtitle' :: String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper
                      $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)

    where 
        sep = replicate (6 + length x) '-'

myKeys :: XConfig  l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c = 

    let subKeys str ks = subtitle' str : mkNamedKeymap c ks in
    subKeys "Xmonad keys"
    [ ("M-S-m", addName "Run Rofi" $ spawn "rofi -show drun"),
      ("M-S-n", addName "Run Rofi Window" $ spawn "rofi -show window"),

        --Restart and Recompile
        ("M-C-r", addName "Recompile" $ spawn "xmonad --recompile"),
        ("M-S-r", addName "Restart" $ spawn "xmonad --restart"),
        --Monitor
        ("M-.", addName "Next Monitor" $ nextScreen),
        ("M-,", addName "Prev Monitor" $ prevScreen),
        --General Functions
        ("M-S-f", addName "Focus" $ withFocused $windows . W.sink),
        ("M-w", addName "Kill" $ kill), 
        ("M-S-w", addName "Kill All" $ killAll),
        --Move Windows
        ("M-j", addName "Focus Down" $ windows W.focusDown),
        ("M-k", addName "Focus Up" $ windows W.focusUp),
        ("M-S-j", addName "Swap Down" $ windows W.swapDown),
        ("M-S-k", addName "Swap Up" $  windows W.swapUp),
        ("M-h", addName "Shrink" $ sendMessage Shrink),
        ("M-l", addName "Expand" $ sendMessage Expand),
        ("M-S-h", addName "MirrorShink" $ sendMessage MirrorShrink),
        ("M-S-l", addName "MirrorExpand" $ sendMessage MirrorExpand),
        ("M-<Tab>", addName "NextLayout" $ sendMessage NextLayout),
        ("M-<Space>", addName "Float" $ sendMessage(T.Toggle "floats")),

        --Volumen
        ("<XF86AudioRaiseVolume>", addName "Volumen Up" $ spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
        ("<XF86AudioLowerVolume>", addName "Volumen Down" $ spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
        --Application
        ("<XF86Search>", addName "Open Firefox" $ spawn "firefox-bin"),
        ("M-S-p", addName "Open File Explorer" $ spawn "pcmanfm"),
        ("M-v", addName "Open PavuControl" $ spawn "pavucontrol"),
        ("M-<Return>", addName "Open Terminal" $ spawn myTerminal),
        ("M-S-<Return>", addName "Open Kitty" $ spawn "kitty"),
        ("M-S-s", addName "Open ScreenShot" $ spawn "spectacle -r")
     
    ]
         where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
               nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))


-- myWorkspace= ["www","file","term","code","set","soc","music"]
--       謹        (ﱘ
-- f108
myWorkspace= ["\xe745  ", "\xe5fe  ", "\xe795  ", "\xf121  ", "\xf8c5  ", "\xf108  ", "\xe217  ", "\xf001 "]

main :: IO ()
main = do
    xmobarMonitor <- spawnPipe "xmobar -x 0 ~/.config/xmobar/screen0"
    xmobarMonitor1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/screen1"
    xmonad  $ ewmh $ myKeys $ docks $ def{   
        manageHook =  myManageHook <+> manageDocks,  
--        handleEventHook = docks,
        terminal=myTerminal,
        startupHook = myStartupHook,
        workspaces = myWorkspace,
        modMask = myModMask,
        borderWidth=myBorderWidth,
        layoutHook = showWName' myShowWNameTheme $ myLayoutHook,
        focusedBorderColor = myFocusBorder,
        normalBorderColor = myNormalBorder,
        logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP{
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
    }
