RANDOMIZE TIMER

CONST True = 1
CONST False = 0

'Key Definitions
Action_Quit$ = "q"
Action_Help$ = "?"

Action_Aim$ = "a"
Action_EjectMag$ = "e"
Action_PullTrigger$ = "f"
Action_PullSlide$ = "p"
Action_SlideRelease$ = "r"
Action_Safety$ = "s"
Action_ChamberCheck$ = "c"
Action_MagSwap$ = "m"
Action_GetLooseRound$ = "g"
Action_LoadRound$ = "z"
Action_UnloadRound$ = "Z"
Action_Look$ = "l"
Action_DetailedLook$ = "L"
Action_MoveNorth$ = CHR$(0) + "H"
Action_MoveSouth$ = CHR$(0) + "P"
Action_MoveEast$ = CHR$(0) + "M"
Action_MoveWest$ = CHR$(0) + "K"

DIM Quit AS _UNSIGNED _BIT
Quit = False

TYPE RoomDef
    LooseRounds AS INTEGER
    ExitN AS INTEGER
    ExitS AS INTEGER
    ExitE AS INTEGER
    ExitW AS INTEGER
    Description AS INTEGER
END TYPE

TYPE PlayerDef
    MagInHand AS INTEGER
    LooseRounds AS INTEGER
    XPos AS INTEGER
    YPos AS INTEGER
    WeaponRaised AS INTEGER 
    AccuracyFactor AS SINGLE 'Decimal percentage
END TYPE

TYPE MagDef
    RoundsInMag AS INTEGER
END TYPE

TYPE PistolDef
    MagInserted AS INTEGER
    RoundInChamber AS INTEGER
    SlideLocked AS INTEGER
    SafetyOn AS INTEGER
    TriggerPulled AS INTEGER
END TYPE

DIM Player AS PlayerDef
DIM Pistol1911 AS PistolDef
DIM Mag1911(1 TO 3) AS INTEGER
DIM Room(1 TO 10, 1 TO 10) AS RoomDef

Mag1911.RoundsInMag(1) = CINT(7 * RND)
Mag1911.RoundsInMag(2) = CINT(7 * RND)
Mag1911.RoundsInMag(3) = CINT(7 * RND)
Pistol1911.RoundInChamber = CINT(RND)
Pistol1911.MagInserted = CINT(2 * RND + 1)
Pistol1911.SafetyOn = CINT(RND)
Pistol1911.SlideLocked = CINT(RND)

FOR ix = 1 TO 10
    FOR iy = 1 TO 10
        IF (RND < .5) THEN Room(ix, iy).LooseRounds = INT(5 * RND)
        DO
            Room(ix, iy).ExitN = 1
            Room(ix, iy).ExitS = 1
            Room(ix, iy).ExitE = 1
            Room(ix, iy).ExitW = 1
        LOOP UNTIL (Room(ix, iy).ExitN + Room(ix, iy).ExitS + Room(ix, iy).ExitE + Room(ix, iy).ExitW) >= 1
    NEXT
NEXT


Player.LooseRounds = INT(10 * RND)
Player.XPos = CINT(9 * RND + 1)
Player.YPos = CINT(9 * RND + 1)



'Main loop

GOSUB Look
DO
    _LIMIT 20

    GOSUB GetInput
    GOSUB DoAction

LOOP WHILE Quit = False
SYSTEM 'Exit immediately




'====MAIN SUBROUTINES===


PrintHelp:
CLS
PRINT Action_Quit$, "Quit"
PRINT Action_Help$, "Help"
PRINT
PRINT Action_EjectMag$, "Eject/Insert magazine"
PRINT Action_PullTrigger$, "Pull trigger"
PRINT Action_PullSlide$, "Pull slide"
PRINT Action_SlideRelease$, "Slide Release"
PRINT Action_Safety$, "Toggle safety"
PRINT Action_ChamberCheck$, "Press check chamber"
PRINT Action_MagSwap$, "Swap magazines (if mag in hand)"
PRINT Action_GetLooseRound$, "Grab loose rounds from floor"
PRINT Action_LoadRound$, "Load a round (if mag in hand)"
PRINT Action_UnloadRound$, "Unload a round (if mag in hand)"
PRINT Action_Look$, "Quick glance around"
PRINT Action_DetailedLook$, "Look closely at something"
PRINT Action_MoveNorth$, "Move North"
PRINT Action_MoveSouth$, "Move South"
PRINT Action_MoveEast$, "Move East"
PRINT Action_MoveWest$, "Move West"
PRINT
PRINT "Press any key to continue..."
SLEEP
CLS
_KEYCLEAR
RETURN



GetInput:
K$ = "": _KEYCLEAR
DO
    _LIMIT 20
    K$ = INKEY$
LOOP WHILE K$ = ""
_KEYCLEAR
Row = CSRLIN: Col = POS(0)
LOCATE 1, 78: PRINT K$
LOCATE Row, Col
RETURN



Look:
PRINT "Your weapon is a .45 1911. "
PRINT "The safety is ";: IF Pistol1911.SafetyOn = True THEN PRINT "ON. " ELSE PRINT "OFF. "
PRINT "The slide is ";: IF Pistol1911.SlideLocked = True THEN PRINT "back. " ELSE PRINT "forward. "
PRINT "There is ";: IF Pistol1911.MagInserted <> 0 THEN PRINT "a magazine inserted. " ELSE PRINT "no magazine inserted. "
IF (Room(Player.XPos, Player.YPos).LooseRounds > 0) THEN PRINT "There are"; Room(Player.XPos, Player.YPos).LooseRounds; "loose rounds on the floor."
PRINT "You have";Player.LooseRounds;"loose round(s) on your person."
RETURN



DoAction:
SELECT CASE K$

    CASE Action_PullTrigger$ 'Pull Trigger
        GOSUB PullTrigger

    CASE Action_PullSlide$ 'Pull Slide
        GOSUB PullSlide

    CASE Action_SlideRelease$ 'Slide release
        GOSUB SlideRelease

    CASE Action_EjectMag$ 'Eject/Insert Mag
        GOSUB EjectMag

    CASE Action_Safety$ 'Toggle Safety
        GOSUB ToggleSafety

    CASE Action_ChamberCheck$ 'Chamber press check
        GOSUB ChamberCheck

    CASE Action_MagSwap$ 'Swap another magazine while holding.
        GOSUB MagSwap

    CASE Action_GetLooseRound$ 'GetLooseRound
        GOSUB GetLooseRound

    CASE Action_LoadRound$ 'Load a round into the in-hand magazine
        GOSUB LoadRound
        
    CASE Action_UnloadRound$ 'Unload round from in-hand magazine to inventory
        GOSUB UnloadRound
        
    CASE Action_Aim$ 'Aim at a target to increase accuracy
        GOSUB Aim

    CASE Action_MoveNorth$ 'Move north 1 room
        IF (Room(Player.XPos, Player.YPos).ExitN = 1) AND Player.YPos > 1 THEN
            Player.YPos = Player.YPos - 1
            PRINT "You move north into the next room."
        ELSE
            PRINT "No exit that way. "
        END IF

    CASE Action_MoveSouth$ 'Move south 1 room
        IF (Room(Player.XPos, Player.YPos).ExitS = 1) AND Player.YPos < 10 THEN
            Player.YPos = Player.YPos + 1
            PRINT "You move south into the next room."
        ELSE
            PRINT "No exit that way. "
        END IF

    CASE Action_MoveEast$ 'Move east 1 room
        IF (Room(Player.XPos, Player.YPos).ExitE = 1) AND Player.XPos < 10 THEN
            Player.XPos = Player.XPos + 1
            PRINT "You move east into the next room."
        ELSE
            PRINT "No exit that way. "
        END IF

    CASE Action_MoveWest$ 'Move west 1 room
        IF (Room(Player.XPos, Player.YPos).ExitW = 1) AND Player.XPos > 1 THEN
            Player.XPos = Player.XPos - 1
            PRINT "You move west into the next room."
        ELSE
            PRINT "No exit that way. "
        END IF
        
    CASE Action_Look$
        GOSUB Look        

    CASE Action_Quit$ 'Quit
        Quit = True
        
    CASE Action_Help$
        GOSUB PrintHelp

END SELECT
RETURN




'===WEAPON SUBROUTINES===
CycleAction:
Player.AccuracyFactor = 0
IF (Pistol1911.RoundInChamber = 1) THEN
    Pistol1911.RoundInChamber = 0
    Pistol1911.SlideLocked = 0
    IF (Pistol1911.TriggerPulled = 1) THEN
        PRINT "An empty shell ejects. ";
        Pistol1911.TriggerPulled = 0
    ELSE
        PRINT "A round ejects. ";
        Room(Player.XPos, Player.YPos).LooseRounds = Room(Player.XPos, Player.YPos).LooseRounds + 1
    END IF
END IF
IF (Pistol1911.MagInserted <> 0) THEN
    IF (Mag1911.RoundsInMag(Pistol1911.MagInserted) > 0) THEN
        Pistol1911.RoundInChamber = 1
        Mag1911.RoundsInMag(Pistol1911.MagInserted) = Mag1911.RoundsInMag(Pistol1911.MagInserted) - 1
        Pistol1911.SlideLocked = 0
    ELSE
        Pistol1911.SlideLocked = 1
    END IF
ELSE
    Pistol1911.SlideLocked = 0
END IF
RETURN



EjectMag:
Player.AccuracyFactor = 0
IF (Pistol1911.MagInserted <> 0) THEN
    Player.MagInHand = Pistol1911.MagInserted
    Pistol1911.MagInserted = 0
    PRINT "You eject the magazine. There are"; Mag1911.RoundsInMag(Player.MagInHand); "round(s) loaded."
ELSEIF (Pistol1911.MagInserted = 0) THEN
    Pistol1911.MagInserted = Player.MagInHand
    Player.MagInHand = 0
    PRINT "You re-insert the magazine."
END IF
RETURN



ToggleSafety:
Player.AccuracyFactor = Player.AccuracyFactor / 2
PRINT "You toggle the thumb safety ";
Pistol1911.SafetyOn = 1 - Pistol1911.SafetyOn
IF Pistol1911.SafetyOn = 1 THEN PRINT "on." ELSE PRINT "off."
RETURN



MagSwap:
IF (Player.MagInHand = 0) THEN RETURN
PRINT "Which magazine to grab? (";
FOR i = 1 TO 3
    IF i <> Player.MagInHand THEN PRINT i;
NEXT
PRINT ")"
DO: GOSUB GetInput: LOOP UNTIL (VAL(K$) <> Player.MagInHand) AND ((VAL(K$) = 1) OR (VAL(K$) = 2) OR (VAL(K$) = 3))
Player.MagInHand = VAL(K$)
PRINT "You swap magazine"; Player.MagInHand; "for magazine "; K$; ". There are"; Mag1911.RoundsInMag(Player.MagInHand); "round(s) loaded."
RETURN



UnloadRound:
IF (Player.MagInHand <> 0) AND (Mag1911.RoundsInMag(Player.MagInHand) > 0) THEN
    Mag1911.RoundsInMag(Player.MagInHand) = Mag1911.RoundsInMag(Player.MagInHand) - 1
    Player.LooseRounds = Player.LooseRounds + 1
    PRINT "You unload a round from the magazine."; Mag1911.RoundsInMag(Player.MagInHand); "rounds still loaded. "
ELSE
    PRINT "The magazine is empty. "
END IF
RETURN



LoadRound:
IF (Player.MagInHand <> 0) AND (Player.LooseRounds > 0) THEN
    IF (Mag1911.RoundsInMag(Player.MagInHand) < 8) THEN
        Mag1911.RoundsInMag(Player.MagInHand) = Mag1911.RoundsInMag(Player.MagInHand) + 1
        Player.LooseRounds = Player.LooseRounds - 1
        PRINT "You load a round into the magazine."; Mag1911.RoundsInMag(Player.MagInHand); "rounds now loaded. "
    ELSE
        PRINT "The magazine is full. "
    END IF
END IF
RETURN



PullTrigger:
Player.AccuracyFactor = Player.AccuracyFactor / 2
IF (Pistol1911.RoundInChamber = 1) AND (Pistol1911.SafetyOn = 0) THEN
    Pistol1911.TriggerPulled = 1
    PRINT "BANG! ";
    GOSUB CycleAction
    IF (Pistol1911.SlideLocked = 1) THEN PRINT "The slide locks back." ELSE PRINT
ELSE
    PRINT "Nothing happens."
END IF
RETURN



ChamberCheck:
Player.AccuracyFactor = 0
IF (Pistol1911.SlideLocked = False) THEN PRINT "You pull the slide partially back. "; ELSE PRINT "The slide is locked back. ";
IF Pistol1911.RoundInChamber = 1 THEN
    PRINT "There is a round in the chamber."
ELSE
    PRINT "There is no round in the chamber."
END IF
RETURN



GetLooseRound:
Player.AccuracyFactor = 0
IF (Room(Player.XPos, Player.YPos).LooseRounds > 0) THEN
    Room(Player.XPos, Player.YPos).LooseRounds = Room(Player.XPos, Player.YPos).LooseRounds - 1
    Player.LooseRounds = Player.LooseRounds + 1
    PRINT "You pick up a loose round from the floor."
END IF
RETURN



PullSlide:
Player.AccuracyFactor = 0
PRINT "You pull the slide back. ";
GOSUB CycleAction
IF (Pistol1911.SlideLocked = 1) THEN
    PRINT "The slide stays locked in place."
ELSE
    PRINT "The slide snaps forward."
END IF
RETURN



SlideRelease:
Player.AccuracyFactor = Player.AccuracyFactor / 2
IF (Pistol1911.SlideLocked = 1) THEN
    PRINT "You toggle the release and the slide snaps forward. "
    Pistol1911.SlideLocked = 0
    IF (Pistol1911.MagInserted<>0 AND Pistol1911.RoundInChamber = 0 AND Mag1911.RoundsInMag(Pistol1911.MagInserted) > 0) THEN
        Pistol1911.RoundInChamber = 1
        Mag1911.RoundsInMag(Pistol1911.MagInserted) = Mag1911.RoundsInMag(Pistol1911.MagInserted) - 1
    END IF
END IF
RETURN



Aim:
IF Player.WeaponRaised = 0 THEN
    Player.WeaponRaised = 1
    PRINT "You raise the pistol to an aiming position. ";
END IF
Player.AccuracyFactor = Player.AccuracyFactor + ((1 - Player.AccuracyFactor) / 2)
SELECT CASE Player.AccuracyFactor
    CASE > 0 
        PRINT ""; Player.AccuracyFactor
END SELECT    
RETURN
