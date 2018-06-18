module Main where

import System.Exit
import System.IO
import XMonad
import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageHelpers (isFullscreen,doFullFloat)
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Run(spawnPipe)
--import XMonad.Actions.Volume
import qualified Data.Map                   as M
import System.IO
import qualified XMonad.Actions.Search      as S
import qualified XMonad.Actions.Submap      as SM
import qualified XMonad.Layout.WorkspaceDir as WD
import qualified XMonad.Prompt              as P
import qualified XMonad.Prompt.AppendFile   as AP
import qualified XMonad.Util.Dzen as DZEN


import Graphics.X11.ExtraTypes.XF86

main :: IO ()
main = do
  --todo: don't hardcode xmobar path :(, mb use nix?
  xmproc <- spawnPipe "xmobar --bottom ~/.xmonad/xmobarrc"

  -- Start xmonad using the main desktop configuration with a few
  -- simple overrides:
  xmonad $ gnomeConfig
    { modMask    = modm
    , terminal   = "gnome-terminal"
    , layoutHook = smartBorders $ avoidStruts myLayout
    , manageHook = myManageHook <+> manageHook desktopConfig
    , logHook    = dynamicLogWithPP $ xmobarPP {
            ppOutput  = hPutStrLn xmproc
          , ppTitle   = xmobarColor xmobarTitleColor "" . shorten 200
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep     = "   "
      }
    }

    `additionalKeysP` ezKeyBindings
    `additionalKeys`  keyBindings

modm :: KeyMask
modm = mod4Mask -- Use the "Win" key for the mod key

-- key bindings for use with ez config tool
ezKeyBindings :: [(String, X ())]
ezKeyBindings =
    [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))
    , ("M-S-l", spawn "dm-tool lock")
    , ("M-p",   shellPrompt myXPConfig)
    , ("M-S-w", WD.changeDir myXPConfig)
    , ("M-a",   AP.appendFilePrompt myXPConfig "~/todo") --doesn't work (write op seems to fail)
    ]


-- key bindings using actual key symbols and masks
keyBindings :: [((KeyMask, KeySym), X ())]
keyBindings =
    [ ((modm, xK_s), SM.submap $ searchEngineMap $ S.promptSearch P.def)
    , ((modm .|. shiftMask, xK_s), SM.submap $ searchEngineMap $ S.selectSearch)
    , ((0, xF86XK_AudioLowerVolume), spawn "volume -")
    , ((0, xF86XK_AudioRaiseVolume), spawn "volume +")
    ]


-- idea: 2-stage prompt, first provides search engine list with autocomplete,
-- second prompts for search query
-- look into it here: https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Prompt.html
searchEngineMap method = M.fromList $
      [ ((0, xK_g), method S.google)
      , ((0, xK_h), method S.hoogle)
      , ((0, xK_w), method S.wikipedia)
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


xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"


myLayout = WD.workspaceDir "~" $ avoidStruts (
    ThreeColMid 1 (3/100) (1/2) |||
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    Full ) |||
---    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

