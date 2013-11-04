--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
 
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import System.IO
import XMonad.Actions.CycleWS
import XMonad.Hooks.FadeInactive
import Control.Monad (when)

import XMonad.Layout.TwoPane
--import XMonad.Layout.Combo
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
--import XMonad.Layout.Accordion
import XMonad.Layout.BryanSingleScreen
import XMonad.Layout.BryanAccordion
import XMonad.Layout.ShrinkExpandFlipper
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.List(isInfixOf)
 
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "gnome-terminal --hide-menubar"
 
-- Width of the window border in pixels.
-- THICK BORDER so it's always obvious to me which window I'm looking at!
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
--myNumlockMask   = 0
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myBorderWidth   = 0
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- move to a different workspace and follow it
viewShift i = W.view i . W.shift i

-- only allow viewing of workspaces not already displayed
viewHidden :: WorkspaceId -> X ()
viewHidden w = do
  ws <- gets windowset
  when (w `notElem` (map (W.tag . W.workspace) $ W.current ws : W.visible ws)) (windows $ W.view w)

upStack :: X()
upStack = do
    ws <- gets windowset
    let l = W.layout $ (W.workspace . W.current) ws
        d = description l
        super = "MasterPlusSublayout"
        sub = "Tabbed"
    if super `isInfixOf` d
        then if sub `isInfixOf` d
                then sendMessage Down
                    else sendMessage Up
            else if sub `isInfixOf` d
                then windows W.focusDown
                    else windows W.focusUp

downStack :: X()
downStack = do
    ws <- gets windowset
    let l = W.layout $ (W.workspace . W.current) ws
        d = description l
        super = "MasterPlusSublayout"
        sub = "Tabbed"
    if super `isInfixOf` d
        then if sub `isInfixOf` d
                then sendMessage Up
                    else sendMessage Down
            else if sub `isInfixOf` d
                then windows W.focusUp
                    else windows W.focusDown
        
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#77bb77"
myFocusedBorderColor = "#ff0000"
 
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm .|. controlMask, xK_t), spawn $ XMonad.terminal conf)

    -- launch firefox
    , ((modm .|. shiftMask, xK_f), spawn "firefox")

    -- launch thunderbird
    , ((modm .|. shiftMask, xK_t), spawn "thunderbird")

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- movie view
    , ((modm,               xK_v     ), spawn "~/.movie_toggle")
 
    -- toggle fading of un-focused windows
    , ((modm,               xK_f     ), spawn "~/.xmonad_fade")

    -- launch gmrun
    --, ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window 
    , ((modm,               xK_c     ), kill)
    , ((shiftMask .|. controlMask, xK_q), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    --, ((modm,               xK_k     ), windows W.focusDown)
    , ((modm,               xK_j     ), downStack )
    , ((modm,               xK_Down     ), downStack )
 
    -- Move focus to the previous window
    --, ((modm,               xK_j     ), windows W.focusUp  )
    , ((modm,               xK_k     ), upStack  )
    , ((modm,               xK_Up     ), upStack  )
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp  )
 
    -- Move focus to the master window
    --, ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Swap the two screens and keep focus on the current window
    , ((modm,               xK_w     ), swapNextScreen >> nextScreen      )
 
    -- Toggle the previous workspace
    , ((modm,               xK_u     ), toggleWS            )

    -- Toggle focus between master and slave
    , ((modm,               xK_m     ), windows W.focusMaster )

    , ((modm .|. shiftMask, xK_m     ), sendMessage $ MainSlave )

    -- Swap the master and slave windows
    , ((modm,               xK_s     ), sendMessage SwapMasterAndSlave)

    -- Shrink the master area
    , ((modm,               xK_Next     ), sendMessage $ Shrink)
 
    -- Expand the master area
    , ((modm,               xK_Prior     ), sendMessage $ Expand)

    -- Swap focus between master and slave (sublayout) panes
    , ((modm,               xK_l     ), sendMessage $ SlaveFocus)
    , ((modm,               xK_Right     ), sendMessage $ SlaveFocus)
    , ((modm,               xK_h     ), sendMessage $ MasterFocus)
    , ((modm,               xK_Left     ), sendMessage $ MasterFocus)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Print Screen
    , ((myModMask , xK_Print), spawn "gnome-screenshot")
    , ((myModMask .|. shiftMask, xK_Print), spawn "gnome-screenshot -a")
 
    -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
    -- , ((modm , xK_b ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)

    -- Lock the screen
    , ((modm .|. controlMask, xK_BackSpace     ), spawn "xscreensaver-command -lock")

    -- Suspend
    , ((modm .|. controlMask, xK_Prior     ), spawn "xscreensaver-command -lock && pmi action suspend")

    -- Hibernate
    , ((modm .|. controlMask, xK_Next     ), spawn "xscreensaver-command -lock && pmi action hibernate")

    -- Shutdown
    , ((modm .|. controlMask, xK_End     ), spawn "dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop")

    -- Restart
    , ((modm .|. controlMask, xK_Home     ), spawn "dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart")
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(viewHidden, 0), (windows . W.shift, shiftMask)]]
	   -- Greedy view (switching the screens when pressing workspace that's
	   -- already on the screen) was too confusing!
        --, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    -- XF86AudioMute
    [ ((modm , 0xffc7 ), spawn "~/.amixer_muter")

    -- XF86AudioLowerVolume
    , ((modm , 0xffc8 ), spawn "amixer set Master 5%-")

    -- XF86AudioRaiseVolume
    , ((modm , 0xffc9 ), spawn "amixer set Master 5%+")
    ]
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- shift-control-button1, Set the window to floating mode and move by dragging
    [ ((shiftMask .|. controlMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button1, Raise the window to the top of the stack
    , ((modMask, button1), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--


bryanTwoPane = flippedShrinkExpand (TwoPane 0.03 0.5)

myLayout = bryanLayout (bryanTwoPane) (bryanAccordion) ||| bryanLayout (bryanTwoPane) (simpleTabbed) ||| simpleTabbed
 
------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Thunderbird"    --> doShift "8"
    , className =? "Rhythmbox"      --> doShift "8"
    , className =? "Update-manager" --> doShift "8"
    --, className =? "Update-manager" --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
 
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
--  where fadeAmount = 0.5
  where fadeAmount = 1

------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawn "~/.xmonad_startups"
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main = do
	xmproc <- spawnPipe "xmobar"
	xmonad $ defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        --numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = avoidStruts $ myLayout,
        manageHook         = manageDocks <+> myManageHook,
        logHook            = myLogHook <+> dynamicLogWithPP xmobarPP
	   					{ ppOutput = hPutStrLn xmproc,
						  ppTitle = shorten 0
						},
        startupHook        = myStartupHook
}
