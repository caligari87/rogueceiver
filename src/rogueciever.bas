RANDOMIZE TIMER

CONST True = 1
CONST False = 0

'Key Definitions
Action_EjectMag$ = "E"
Action_PullTrigger$ = "F"

DIM Quit AS _UNSIGNED _BIT

Quit = False

TYPE RoomDef
    LooseRounds AS INTEGER
    ExitN AS INTEGER
    ExitS AS INTEGER
    ExitE AS INTEGER
    ExitW AS INTEGER
	Description as INTEGER
END TYPE

TYPE PlayerDef
    MagInHand AS INTEGER
    LooseRounds AS INTEGER
    XPos AS INTEGER
    YPos AS INTEGER
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

DO
    _LIMIT 20

    'GOSUB PrintOptions
    GOSUB GetInput
    GOSUB DoAction

LOOP WHILE Quit = False
SYSTEM 'Exit immediately




'====MAIN SUBROUTINES===


PrintOptions:
VIEW PRINT 20 TO 25
CLS
LOCATE 20, 1
COLOR 0, 7: PRINT "F";: COLOR 7, 0: PRINT "ire  ";
COLOR 0, 7: PRINT "P";: COLOR 7, 0: PRINT "ull slide  ";
COLOR 0, 7: PRINT "E";: COLOR 7, 0: IF (Pistol1911.MagInserted <> 0) THEN PRINT "ject mag  "; ELSE PRINT " Insert mag  ";
COLOR 0, 7: PRINT "S";: COLOR 7, 0: PRINT "afety  ";
COLOR 0, 7: PRINT "C";: COLOR 7, 0: PRINT "hamber check  ";
COLOR 0, 7: PRINT "M";: COLOR 7, 0: PRINT "ag swap  "
COLOR 0, 7: PRINT "G";: COLOR 7, 0: PRINT "et loose rounds  ";
COLOR 0, 7: PRINT "L";: COLOR 7, 0: PRINT "oad round  ";
COLOR 0, 7: PRINT "U";: COLOR 7, 0: PRINT "nload round  "

PRINT
COLOR 0, 7: PRINT "Q";: COLOR 7, 0: PRINT "uit"
COLOR 7

VIEW PRINT 1 TO 10
CLS
LOCATE 1, 1
PRINT Room(Player.XPos, Player.YPos).Description
PRINT "Your weapon is a .45 1911. "
PRINT "The safety is ";: IF Pistol1911.SafetyOn = True THEN PRINT "ON. " ELSE PRINT "OFF. "
PRINT "The slide is ";: IF Pistol1911.SlideLocked = True THEN PRINT "back. " ELSE PRINT "forward. "
PRINT "There is ";: IF Pistol1911.MagInserted <> 0 THEN PRINT "a magazine inserted. " ELSE PRINT "no magazine inserted. "
IF (Player.MagInHand <> 0) THEN PRINT "You hold magazine"; Player.MagInHand; "with"; Mag1911.RoundsInMag(Player.MagInHand); "round(s) loaded. You have"; Player.LooseRounds; "spare rounds to load."
IF (Room(Player.XPos, Player.YPos).LooseRounds > 0) THEN PRINT "There are"; Room(Player.XPos, Player.YPos).LooseRounds; "loose rounds on the floor."

VIEW PRINT 11 TO 15
LOCATE 15, 1
RETURN



GetInput:
K$ = "": DO: LOOP UNTIL INKEY$ = ""
DO
    _LIMIT 20
    K$ = UCASE$(INKEY$)
LOOP WHILE K$ = ""
RETURN



DoAction:
SELECT CASE K$

    CASE Action_PullTrigger$ 'Pull Trigger
        IF (Pistol1911.RoundInChamber = 1) AND (Pistol1911.SafetyOn = 0) THEN
            Pistol1911.TriggerPulled = 1
            PRINT "BANG! ";
            GOSUB CycleAction
            IF (Pistol1911.SlideLocked = 1) THEN PRINT "The slide locks back." ELSE PRINT
        ELSE
            PRINT "Nothing happens."
        END IF

    CASE "P" 'Pull Slide
        PRINT "You pull the slide back. ";
        GOSUB CycleAction
        IF (Pistol1911.SlideLocked = 1) THEN
            PRINT "The slide stays locked in place."
        ELSE
            PRINT "The slide snaps forward."
        END IF

    CASE Action_EjectMag$ 'Eject/Insert Mag
        GOSUB EjectMag

    CASE "S" 'Toggle Safety
        GOSUB ToggleSafety

    CASE "C" 'Chamber press check
        IF (Pistol1911.SlideLocked = False) THEN PRINT "You pull the slide partially back. "; ELSE PRINT "The slide is locked back. ";
        IF Pistol1911.RoundInChamber = 1 THEN
            PRINT "There is a round in the chamber."
        ELSE
            PRINT "There is no round in the chamber."
        END IF

    CASE "M" 'Swap another magazine while holding.
        IF (Player.MagInHand <> 0) THEN GOSUB MagSwap

    CASE "G" 'GetLooseRounds
        IF (Room(Player.XPos, Player.YPos).LooseRounds > 0) THEN
            Room(Player.XPos, Player.YPos).LooseRounds = Room(Player.XPos, Player.YPos).LooseRounds - 1
            Player.LooseRounds = Player.LooseRounds + 1
            PRINT "You pick up a loose round from the floor."
        END IF

    CASE "L" 'Load a round into the in-hand magazine
        IF (Player.MagInHand <> 0) AND (Player.LooseRounds > 0) THEN
            IF (Mag1911.RoundsInMag(Player.MagInHand) < 8) THEN
                Mag1911.RoundsInMag(Player.MagInHand) = Mag1911.RoundsInMag(Player.MagInHand) + 1
                Player.LooseRounds = Player.LooseRounds - 1
                PRINT "You load a round into the magazine. "
            ELSE
                PRINT "The magazine is full. "
            END IF
        END IF
    CASE "U" 'Unload round from in-hand magazine to inventory
        IF (Player.MagInHand <> 0) AND (Mag1911.RoundsInMag(Player.MagInHand) > 0) THEN
            Mag1911.RoundsInMag(Player.MagInHand) = Mag1911.RoundsInMag(Player.MagInHand) - 1
            Player.LooseRounds = Player.LooseRounds + 1
            PRINT "You unload a round from the magazine. "
        ELSE
            PRINT "The magazine is empty. "
        END IF

    CASE CHR$(0) + "H" 'Move north 1 room
        IF (Room(Player.XPos, Player.YPos).ExitN = 1) AND Player.YPos > 1 THEN
            Player.YPos = Player.YPos - 1
            PRINT "You move north into the next room."
        ELSE
            PRINT "No exit that way. "
        END IF

    CASE CHR$(0) + "P" 'Move south 1 room
        IF (Room(Player.XPos, Player.YPos).ExitS = 1) AND Player.YPos < 10 THEN
            Player.YPos = Player.YPos + 1
            PRINT "You move south into the next room."
        ELSE
            PRINT "No exit that way. "
        END IF

    CASE CHR$(0) + "M" 'Move east 1 room
        IF (Room(Player.XPos, Player.YPos).ExitE = 1) AND Player.XPos < 10 THEN
            Player.XPos = Player.XPos + 1
            PRINT "You move east into the next room."
        ELSE
            PRINT "No exit that way. "
        END IF

    CASE CHR$(0) + "K" 'Move west 1 room
        IF (Room(Player.XPos, Player.YPos).ExitW = 1) AND Player.XPos > 1 THEN
            Player.XPos = Player.XPos - 1
            PRINT "You move west into the next room."
        ELSE
            PRINT "No exit that way. "
        END IF


    CASE "Q" 'Quit
        Quit = True

END SELECT
RETURN




'===WEAPON SUBROUTINES===
CycleAction:
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
IF (Pistol1911.MagInserted <> 0) THEN
    Player.MagInHand = Pistol1911.MagInserted
    Pistol1911.MagInserted = 0
    PRINT "You eject the magazine."
ELSEIF (Pistol1911.MagInserted = 0) THEN
    Pistol1911.MagInserted = Player.MagInHand
    Player.MagInHand = 0
    PRINT "You re-insert the magazine."
END IF
RETURN

ToggleSafety:
PRINT "You toggle the thumb safety."
Pistol1911.SafetyOn = 1 - Pistol1911.SafetyOn
RETURN



MagSwap:
PRINT "Which magazine to grab? (";
FOR i = 1 TO 3
    IF i <> Player.MagInHand THEN PRINT i;
NEXT
PRINT ")"
DO: GOSUB GetInput: LOOP UNTIL (VAL(K$) <> Player.MagInHand) AND ((VAL(K$) = 1) OR (VAL(K$) = 2) OR (VAL(K$) = 3))
PRINT "You swap magazine"; Player.MagInHand; "for magazine "; K$; "."
Player.MagInHand = VAL(K$)
RETURN
