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
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Run(spawnPipe)
import qualified Data.Map                   as M
import System.IO
import qualified XMonad.Actions.Search      as S
import qualified XMonad.Actions.Submap      as SM
import qualified XMonad.Layout.WorkspaceDir as WD
import qualified XMonad.Prompt              as P
import qualified XMonad.Prompt.AppendFile   as AP
import qualified XMonad.Util.Dzen as DZEN

import XMonad.Actions.CycleWS

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.Layout.SimpleDecoration as SD

import qualified XMonad.Layout.DecorationMadness as DM
import qualified XMonad.Layout.NoFrillsDecoration as NFD

import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.MultiToggle
import XMonad.Layout.Decoration
import XMonad.Layout.Reflect -- haha yes
import XMonad.Util.Font (Align(AlignRight))



import Data.Tree
import XMonad.Actions.TreeSelect
import XMonad.Hooks.WorkspaceHistory
import qualified XMonad.StackSet as W


main :: IO ()
main =
  xmonad =<< statusBar "xmobar" xmobarConf toggleStrutsKey conf

  where
    conf =
      gnomeConfig
        { modMask    = modm
        -- , terminal   = "gnome-terminal --hide-menubar --profile SolarizedDark"
        , terminal   = "alacritty"
        , layoutHook = myLayout
        , manageHook = myManageHook <+> manageHook desktopConfig
        , normalBorderColor  = blue light -- note, blue is same for both..
        , focusedBorderColor = magenta light -- note, magenta is same for both..
        , borderWidth = 4
        }
        `additionalKeysP` ezKeyBindings
        `additionalKeys`  keyBindings

    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

xmobarConf = def { ppCurrent = xmobarColor (cyan light) "" . wrap "[" "]"
                 , ppTitle   = xmobarColor (cyan light)  "" . shorten 40
                 , ppVisible = wrap "(" ")" -- xinerama (multiscreen) only
                 , ppUrgent  = xmobarColor (orange light) ""
                 }


modm :: KeyMask
modm = mod4Mask -- Use the "Win" key for the mod key

-- key bindings for use with ez config tool
ezKeyBindings :: [(String, X ())]
ezKeyBindings =
    [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))
    , ("M-S-l", spawn "dm-tool lock")
    , ("M-p",   shellPrompt myXPConfig)
    , ("M-S-w", WD.changeDir myXPConfig)
    , ("M-a",   spawn "touch /home/pk/test1") --doesn't work (write op seems to fail)
    ]


-- key bindings using actual key symbols and masks
keyBindings :: [((KeyMask, KeySym), X ())]
keyBindings =
    [ ((modm, xK_s), SM.submap $ searchEngineMap $ S.promptSearch P.def)
    , ((modm, xK_r), sendMessage $ Toggle REFLECTY)
    , ((modm, xK_x), sendMessage $ Toggle NBFULL) -- toggle fullscreen for focus
    , ((modm, xK_b), sendMessage ToggleStruts) -- toggle xmobar
    , ((modm .|. shiftMask, xK_b), sendMessage $ Toggle $ HYPERTABBAR_DARK) -- toggle window decoration
    , ((modm .|. shiftMask, xK_s), SM.submap $ searchEngineMap $ S.selectSearch)
    , ((modm .|. shiftMask, xK_t), SM.submap $ themeSelect)
    -- will break if I don't run this to fix perms every time I restart:
    -- note to self: was breaking due to script being in ~/.local/bin instead of /bin
    -- sudo chmod a=rw /sys/class/backlight/intel_backlight/brightness
    , ((0, xF86XK_MonBrightnessUp), spawn "bright +")
    , ((0, xF86XK_MonBrightnessDown), spawn "bright -")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 5%+")
    , ((0, xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle")
    , ((modm .|. shiftMask, xK_Right), shiftNextScreen)
    , ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
    ]


-- idea: 2-stage prompt, first provides search engine list with autocomplete,
-- second prompts for search query
-- look into it here: https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Prompt.html
searchEngineMap method = M.fromList $
      [ ((0, xK_g), method S.google)
      , ((0, xK_h), method S.hoogle)
      , ((0, xK_w), method S.wikipedia)
      ]

-- note: this is janky, need to make them mutually exclusive..
-- Q: do I need to drop multitoggle and just DIY from primitives of next layer? MAYBE
themeSelect = M.fromList $
      [ ((0, xK_1), sendMessage $ Toggle $ HYPERTABBAR_DARK) -- toggle window decoration
      , ((0, xK_2), sendMessage $ Toggle $ HYPERTABBAR_LIGHT) -- toggle window decoration
      ]

-- idea: prompt to append to workflowy todo list

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig :: P.XPConfig
myXPConfig = def
  { P.position          = P.Top
  , P.alwaysHighlight   = True
  , P.promptBorderWidth = 0
  , P.font              = "xft:monospace:size=9"
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
         . mkToggle (single REFLECTY)
         . mkToggle (single NBFULL)
         . mkToggle (HYPERTABBAR_LIGHT ?? HYPERTABBAR_DARK ?? EOT)
         $ ThreeColMid 1 (3/100) (1/2)
       ||| Tall 1 (3/100) (1/2)
       ||| Mirror (Tall 1 (3/100) (1/2))
       ||| XMonad.Layout.Spiral.spiral (6/7)

data HyperDecoration a = Hyper deriving (Show, Read)

instance Eq a => DecorationStyle HyperDecoration a where
    describeDeco _ = "Hyper"
    shrink Hyper (Rectangle _ _ _ dh) r@(Rectangle x y w h) =
        Rectangle x (y + fi dh) w (h - dh)
    pureDecoration Hyper wh ht _ s _ (w, Rectangle x y wid _) =
        if isInStack s w
        then Just $ Rectangle x y nwh ht
        else Nothing
            where nwh = min wid wh

hyperDeco :: (Eq a, Shrinker s) => s -> Theme
           -> l a -> ModifiedLayout (Decoration HyperDecoration s) l a
hyperDeco s c = decoration s c Hyper

data HyperTabBar = HYPERTABBAR_LIGHT | HYPERTABBAR_DARK deriving (Read, Show, Eq, Typeable)
instance Transformer HyperTabBar Window where
    transform HYPERTABBAR_DARK x k = k (hyperDeco shrinkText (windowDecorationTheme dark) x) (\(ModifiedLayout _  x') -> x')

    transform HYPERTABBAR_LIGHT x k = k (hyperDeco shrinkText (windowDecorationTheme light) x) (\(ModifiedLayout _  x') -> x')
