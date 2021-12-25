{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Main where

import System.Exit
import System.IO
import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Run(spawnPipe)
import qualified Data.Map                   as M
import System.IO
import qualified XMonad.Actions.Search      as S
import qualified XMonad.Actions.Submap      as SM
import qualified XMonad.Layout.WorkspaceDir as WD
import qualified XMonad.Prompt              as P
import qualified XMonad.Prompt.Input        as P
import qualified XMonad.Prompt.XMonad       as P
import qualified XMonad.Util.Dzen as DZEN

import XMonad.Actions.CycleWS

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.Layout.SimpleDecoration as SD

import qualified XMonad.Layout.DecorationMadness as DM
import qualified XMonad.Layout.NoFrillsDecoration as NFD

import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.MultiToggle
import XMonad.Layout.Decoration
import XMonad.Util.Font (Align(AlignRight))



import Data.Tree
import XMonad.Actions.TreeSelect
import XMonad.Hooks.WorkspaceHistory
import qualified XMonad.StackSet as W


main :: IO ()
main =
  xmonad =<< statusBar "xmobar" xmobarConf toggleStrutsKey conf
  -- xmonad conf

  where
    conf =
      gnomeConfig
        { modMask    = modm
        , terminal   = alacritty
        , layoutHook = myLayout
        , manageHook = myManageHook <+> manageHook desktopConfig
        , normalBorderColor  = blue light -- note, blue is same for both..
        , focusedBorderColor = magenta light -- note, magenta is same for both..
        , borderWidth = 4
        }
        `additionalKeysP` ezKeyBindings
        `additionalKeys`  keyBindings

    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_f )

xmobarConf = def { ppCurrent = xmobarColor (cyan light) "" . wrap "[" "]"
                 , ppTitle   = xmobarColor (cyan light)  "" . shorten 40
                 , ppVisible = wrap "(" ")" -- xinerama (multiscreen) only
                 , ppUrgent  = xmobarColor (orange light) ""
                 }


alacritty = "LIBGL_ALWAYS_SOFTWARE=1 alacritty"

modm :: KeyMask
modm = mod1Mask -- Use the "Alt" key for the mod key


-- key bindings for use with ez config tool
ezKeyBindings :: [(String, X ())]
ezKeyBindings =
    [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))
    , ("M-S-l", spawn "dm-tool lock")
    , ("M-p",   shellPrompt myXPConfig)
    ]

spawnBtm :: X ()
spawnBtm = spawn $ alacritty ++ " -e btm --battery"

spawnCDown :: X ()
spawnCDown = do
    input <- P.inputPromptWithCompl myXPConfig "timer for: " P.historyCompletion
    case input of
      Just s  -> spawn $ alacritty ++ " -e cdown -c cyan " ++ s
      Nothing -> pure ()


actionTree :: X ()
actionTree = promote
           [ Node (TSNode "PROGRAMS" "Sets screen brightness" $ promote brightness) brightness
           , Node (TSNode "SCREEN" "Sets screen brightness" $ promote brightness) brightness
           , Node (TSNode "ADMIN"  "system config/control" $ promote admin) admin
           ]
  where
    promote = treeselectAction def
    admin =
      [ Node (TSNode "SHUTDOWN" "power off the system" (spawn "shutdown")) []
      ]
    programs =
      [ Node (TSNode "BTM" "system info dashboard" spawnBtm) []
      , Node (TSNode "CDOWN" "spawn a countdown timer" spawnCDown)  []
      ]
    brightness =
      [ Node (TSNode "Bright" "FULL POWER!!"            (spawn "rpi-backlight max")) []
      -- TODO: only supports min/max
      -- , Node (TSNode "Normal" "Normal Brightness (50%)" (spawn "rpi-backlight min "))  []
      , Node (TSNode "Dim"    "Quite dark"              (spawn "rpi-backlight min"))  []
      ]



-- key bindings using actual key symbols and masks
keyBindings :: [((KeyMask, KeySym), X ())]
keyBindings =
    -- TODO: update stack LTS, latest version has custom titles here
    -- TODO: try runSelectedAction from GSConfig
    -- TODO: nvm, tree select is perfect, XMonad.Actions.TreeSelect
    [ ((modm, xK_s)
      -- TODO: transparent background, may require compositing wm or whatever - if not, eh
      , actionTree
      -- , P.xmonadPromptC [ ("btm", spawnBtm)
      --                   , ("cdown", spawnCDown)
      --                   ]
      --                   myXPConfig
      )
    , ((modm, xK_x), sendMessage $ Toggle NBFULL) -- toggle fullscreen for focus
    , ((modm .|. shiftMask, xK_Right), shiftNextScreen)
    , ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
    -- launch a terminal (MOD + RETURN)
    , ((modm, xK_Return), spawn alacritty)
    -- close focused window  (MOD + C)
    , ((modm, xK_c), confirmPrompt myXPConfig "kill window" kill)
    ]



--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig :: P.XPConfig
myXPConfig = def
  { P.position          = P.CenteredAt 0.25 0.5
  , P.alwaysHighlight   = True
  , P.promptBorderWidth = 4
  , P.borderColor       = magenta light
  , P.height            = 60
  , P.bgColor           = base0 light
  , P.fgColor           = base3 light
  , P.bgHLight          = base1 light
  , P.fgHLight          = base2 light
  , P.font              = "xft:monospace:size=20"
  }

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook  = composeOne
  [ className =? "Pidgin" -?> doFloat
  , className =? "XCalc"  -?> doFloat
  , className =? "mpv"    -?> doFloat
  , isDialog              -?> doCenterFloat
  , isFullscreen          -?> doFullFloat --todo: revisit mb?
    -- Move transient windows to their parent:
  , transience
  ]


type HexColor = String
data Solarized
  = Solarized
  { base0   :: HexColor
  , base1   :: HexColor
  , base2   :: HexColor
  , base3   :: HexColor
  , yellow  :: HexColor
  , orange  :: HexColor
  , red     :: HexColor
  , magenta :: HexColor
  , violet  :: HexColor
  , blue    :: HexColor
  , cyan    :: HexColor
  , green   :: HexColor
  }

-- solarized dark
dark :: Solarized
dark
  = Solarized
  { base3   = "#002b36"
  , base2   = "#073642"
  , base1   = "#586e75"
  , base0   = "#657b83"
  , yellow  = "#b58900"
  , orange  = "#cb4b16"
  , red     = "#dc322f"
  , magenta = "#d33682"
  , violet  = "#6c71c4"
  , blue    = "#268bd2"
  , cyan    = "#2aa198"
  , green   = "#859900"
  }

light :: Solarized
light
  = Solarized
  { base0   = "#839496"
  , base1   = "#93a1a1"
  , base2   = "#eee8d5"
  , base3   = "#fdf6e3"
  , yellow  = "#b58900"
  , orange  = "#cb4b16"
  , red     = "#d30102"
  , magenta = "#d33682"
  , violet  = "#6c71c4"
  , blue    = "#268bd2"
  , cyan    = "#2aa198"
  , green   = "#859900"
  }



windowDecorationTheme :: Solarized -> Theme
windowDecorationTheme s
  = def { SD.fontName          = "-misc-fixed-*-*-*-*-24-*-*-*-*-*-*-*"

        , SD.decoHeight        = 32
        , SD.decoWidth         = maxBound

        , SD.activeTextColor   = magenta s
        , SD.inactiveTextColor = blue s
        , SD.urgentTextColor   = orange s

        , SD.activeColor       = base3 s
        , SD.inactiveColor     = base2 s
        , SD.urgentColor       = base3 s

        , activeBorderColor    = magenta s
        , inactiveBorderColor  = blue s
        , urgentBorderColor    = orange s

        , activeBorderWidth    = 3
        , inactiveBorderWidth  = 3
        , urgentBorderWidth    = 3
        }


myLayout = WD.workspaceDir "~"
         . smartBorders
         . avoidStruts
         -- single-elem hlists of toggle transformer options (non-exclusive transforms)
         . mkToggle (single NBFULL)
         $ ThreeColMid 1 (3/100) (1/2)
       ||| Tall 1 (3/100) (1/2)
       ||| Mirror (Tall 1 (3/100) (1/2))
       ||| XMonad.Layout.Spiral.spiral (6/7)
