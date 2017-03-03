--------------------------------------------------------------------------------
import System.Exit
import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)

import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers (isFullscreen,doFullFloat)
import XMonad.Layout.Gaps
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import System.IO
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
--import XMonad.Actions.Volume
import qualified XMonad.Util.Dzen as DZEN
import System.IO

--------------------------------------------------------------------------------
main = do
  --todo: don't hardcode xmobar path :(, mb use nix?
  xmproc <- spawnPipe "~/.cabal/bin/xmobar ~/.xmonad/xmobarrc"

  -- Start xmonad using the main desktop configuration with a few
  -- simple overrides:
  xmonad $ desktopConfig
    { modMask    = mod4Mask -- Use the "Win" key for the mod key
    , terminal = "gnome-terminal"
    , layoutHook = smartBorders $ avoidStruts myLayout
    , manageHook = myManageHook <+> manageHook desktopConfig
    , logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = "   "
      }
    }

    `additionalKeysP` -- Add some extra key bindings:
      [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))
      , ("M-p",   shellPrompt myXPConfig)
      ]

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:monospace:size=9"
  }

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook = composeOne
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


{-
-- nice pretty centered alert
alert :: Show a => a -> X ()
alert = dzenConfig centered . show
centered =
        onCurr (center 150 66)
    >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> addArgs ["-fg", "#80c0ff"]
    >=> addArgs ["-bg", "#000040"]
-}

myLayout = avoidStruts (
    ThreeColMid 1 (3/100) (1/2) |||
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    Full ) |||
---    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

