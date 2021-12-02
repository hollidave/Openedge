 
/*------------------------------------------------------------------------
    File        : OthelloBot
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DHolliday
    Created     : Fri Apr 24 07:26:32 BST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{OthelloJSON.i}

DEFINE INPUT PARAMETER gicRequestID    AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttGame NO-UNDO 
    FIELD gameGUID         AS CHARACTER 
    FIELD startDateTime    AS DATETIME.

DEFINE TEMP-TABLE ttBoard   NO-UNDO
    FIELD board    AS INTEGER EXTENT 64 FORMAT 9
    FIELD gameGUID    AS CHARACTER 
    FIELD whitePlayer    AS CHARACTER 
    FIELD blackPlayer    AS CHARACTER 
    FIELD whiteCounters    AS INTEGER  
    FIELD blackCounters    AS INTEGER 
    FIELD startDateTime    AS DATETIME 
    FIELD gameLengthInSeconds    AS INTEGER 
    FIELD lastMoveWhite    AS CHARACTER 
    FIELD lastMoveBlack    AS CHARACTER 
    FIELD whiteSecondsRemaining    AS INTEGER 
    FIELD blackSecondsRemaining    AS INTEGER 
    FIELD nextMove    AS CHARACTER 
    FIELD turnsSoFar    AS INTEGER
    FIELD validMoves    AS LOGICAL EXTENT 64
    FIELD winner        AS CHARACTER.
 
 DEFINE TEMP-TABLE ttPosition NO-UNDO
    FIELD iRow  AS INTEGER 
    FIELD iColumn AS INTEGER 
    FIELD iValue AS INTEGER
INDEX idxAsc iRow iColumn
INDEX idxDesc iRow DESC iColumn DESC.
    
 DEFINE TEMP-TABLE ttMove NO-UNDO
    FIELD gameGUID  AS CHARACTER 
    FIELD iColumn   AS INTEGER 
    FIELD iRow      AS INTEGER 
    FIELD iCounters AS INTEGER 
    FIELD cCounters AS CHARACTER 
    FIELD iValue    AS INTEGER.
    
 DEFINE TEMP-TABLE ttRandomMove NO-UNDO 
    FIELD iColumn   AS INTEGER 
    FIELD iRow      AS INTEGER 
    FIELD iCounters AS INTEGER 
    FIELD cCounters AS CHARACTER
    FIELD iNumber   AS INTEGER.

 DEFINE TEMP-TABLE ttBestMove NO-UNDO 
    FIELD gameGUID  AS CHARACTER LABEL "gameGUID"
    FIELD iColumn   AS INTEGER LABEL "x"
    FIELD iRow      AS INTEGER LABEL "y" 
    FIELD iMove     AS INTEGER LABEL "moveNumber".
    
DEFINE VARIABLE giBestY              AS INTEGER     NO-UNDO.
DEFINE VARIABLE giBestX              AS INTEGER     NO-UNDO.
DEFINE VARIABLE gcBestCounters      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE giStopPoint     AS INTEGER   NO-UNDO.
DEFINE VARIABLE gcStopType      AS CHARACTER NO-UNDO.

DEFINE VARIABLE giPlayer        AS INTEGER  NO-UNDO.
DEFINE VARIABLE gcPlayer        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE gcPlayerName    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE giOpponent      AS INTEGER  NO-UNDO.
DEFINE VARIABLE giMoveTime      AS INTEGER  NO-UNDO.
 
DEFINE VARIABLE gcMoveLocation  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcGameGUID      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE oJson           AS JsonObject  NO-UNDO.
DEFINE VARIABLE oArray          AS JsonArray   NO-UNDO.

DEFINE VARIABLE giTurns         AS INTEGER      NO-UNDO.
DEFINE VARIABLE glLoser         AS LOGICAL      NO-UNDO.

DEFINE STREAM strOutput.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fnSquare RETURNS LOGICAL 
    (INPUT iCheckRow AS INTEGER,
     INPUT iCheckCol AS INTEGER,
     INPUT iActualRow AS INTEGER,
     INPUT iActualCol AS INTEGER,
     INPUT iCorner AS INTEGER) FORWARD.


/* ***************************  Main Block  *************************** */

RUN prMain.

/* **********************  Internal Procedures  *********************** */

PROCEDURE prAscending:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE VARIABLE iCheck      AS INTEGER      NO-UNDO.
DEFINE VARIABLE iCounters   AS INTEGER      NO-UNDO.
DEFINE VARIABLE iCounters1  AS INTEGER      NO-UNDO.
DEFINE VARIABLE cCounters   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCounters1   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iDiagonal   AS INTEGER      NO-UNDO.
DEFINE VARIABLE lStop       AS LOGICAL      NO-UNDO.
DEFINE VARIABLE lStop1      AS LOGICAL      NO-UNDO.
DEFINE VARIABLE lStop2      AS LOGICAL      NO-UNDO.

ASSIGN giStopPoint = 8.

FOR EACH ttPosition USE-INDEX idxAsc:
                  
    IF ttPosition.iValue <> 0 THEN
        NEXT.  
           
    ASSIGN iCounters    = 0
           lStop1       = FALSE
           lStop2       = FALSE
           iDiagonal    = 0
           gcStopType   = "Column".
             
    CHECKLOOP:
    DO iCheck = ttPosition.iColumn + 1 TO 8: 

        RUN prCheckNext (INPUT ttPosition.iRow, INPUT iCheck, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters, OUTPUT lStop).
    
        IF lStop THEN
        LEAVE CHECKLOOP.
    END. 
    RUN prCreateCounter(ttPosition.iRow,  ttPosition.iColumn, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters).

    ASSIGN gcStopType = "Row".
    
    CHECKLOOP2:
    DO iCheck = ttPosition.iRow + 1 TO 8:
        RUN prCheckNext (INPUT iCheck, INPUT ttPosition.iColumn, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters, OUTPUT lStop).
   
        IF lStop THEN
        LEAVE CHECKLOOP2. 
    END.
    RUN prCreateCounter(ttPosition.iRow,  ttPosition.iColumn, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters).

    ASSIGN gcStopType = "Both".
  
    CHECKLOOP3:
    DO iCheck = ttPosition.iRow + 1 TO 8:
        ASSIGN iDiagonal = iDiagonal + 1.
        
        IF lStop1 = FALSE THEN
            RUN prCheckNext (INPUT iCheck, INPUT ttPosition.iColumn - iDiagonal, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters, OUTPUT lStop1).
        
        IF lStop2 = FALSE THEN
            RUN prCheckNext (INPUT iCheck, INPUT ttPosition.iColumn + iDiagonal, INPUT-OUTPUT iCounters1, INPUT-OUTPUT cCounters1, OUTPUT lStop2).

        IF lStop1 AND lStop2 THEN
            LEAVE CHECKLOOP3. 
   
    END.
    
    RUN prCreateCounter(ttPosition.iRow,  ttPosition.iColumn, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters).
    RUN prCreateCounter(ttPosition.iRow,  ttPosition.iColumn, INPUT-OUTPUT iCounters1, INPUT-OUTPUT cCounters1).
    
END.

END PROCEDURE.

PROCEDURE prBestMove:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iiTurns         AS INTEGER  NO-UNDO.

DEFINE VARIABLE iBestMove       AS INTEGER  NO-UNDO. 
DEFINE VARIABLE iBestValue      AS INTEGER  NO-UNDO. 
DEFINE VARIABLE iRandom         AS INTEGER  NO-UNDO. 
DEFINE VARIABLE iPick           AS INTEGER  NO-UNDO. 

PUT STREAM strOutput UNFORMATTED "Possible Moves:" SKIP.
       
FOR EACH ttMove:
    
    //X and C Squares
    //IF Corner already taken then safe to take
    
    IF fnSquare(ttMove.iRow, ttMove.iColumn, 2,2, 1) OR 
       fnSquare(ttMove.iRow, ttMove.iColumn, 7,7, 64) OR 
       fnSquare(ttMove.iRow, ttMove.iColumn, 2,7, 8) OR
       fnSquare(ttMove.iRow, ttMove.iColumn, 7,2, 57) OR 
       
       fnSquare(ttMove.iRow, ttMove.iColumn, 2,1, 1) OR 
       fnSquare(ttMove.iRow, ttMove.iColumn, 1,2, 1) OR    
            
       fnSquare(ttMove.iRow, ttMove.iColumn, 2,8, 8) OR 
       fnSquare(ttMove.iRow, ttMove.iColumn, 8,2, 57) OR         
       
       fnSquare(ttMove.iRow, ttMove.iColumn, 1,7, 1) OR
       fnSquare(ttMove.iRow, ttMove.iColumn, 7,1, 57) OR         
       
       fnSquare(ttMove.iRow, ttMove.iColumn, 7,8, 64) OR 
       fnSquare(ttMove.iRow, ttMove.iColumn, 8,7, 64) THEN
      
        ASSIGN ttMove.iValue = 0.
        
    PUT STREAM strOutput UNFORMATTED "X=" ttMove.iColumn ", Y=" ttMove.iRow ", Counters Win=" ttMove.cCounters ", Position Value=" ttMove.iValue SKIP.
END.

FOR EACH ttMove:


    IF ttMove.iValue > iBestValue THEN DO: 
        
        ASSIGN giBestY = ttMove.iRow
               giBestX = ttMove.iColumn
               iBestValue = ttMove.iValue 
               gcBestCounters = ttMove.cCounters.
    END.
END.
        
//Lowest number of counters
IF iiTurns < 30 THEN DO:
    ASSIGN iBestMove = 64. 
     
    FOR EACH ttMove WHERE ttMove.iValue = iBestValue:    
    
        IF ttMove.iCounters < iBestMove THEN DO: 
        
            ASSIGN giBestY = ttMove.iRow
                   giBestX = ttMove.iColumn
                   iBestMove = ttMove.iCounters
                   gcBestCounters = ttMove.cCounters.
        END.
    END.
 END.

ELSE DO: 
    FOR EACH ttMove WHERE ttMove.iValue = iBestValue:    
         
        IF ttMove.iCounters > iBestMove THEN DO: 
            
            ASSIGN giBestY = ttMove.iRow
                   giBestX = ttMove.iColumn
                   iBestMove = ttMove.iCounters
                   gcBestCounters = ttMove.cCounters.
        END.
    END.
END.
 
//Random the best moves
FOR EACH ttMove WHERE ttMove.iValue = iBestValue AND 
                      ttMove.iCounters = iBestMove:    
    CREATE ttRandomMove.
    BUFFER-COPY ttMove TO ttRandomMove.
    ASSIGN iRandom = iRandom + 1
           ttRandomMove.iNumber = iRandom.
END.
 
IF iRandom > 1 THEN DO:
    ASSIGN iPick = RANDOM(1, iRandom). 
    FIND ttRandomMove WHERE ttRandomMove.iNumber = iPick NO-ERROR.
    IF AVAILABLE ttRandomMove THEN
        ASSIGN giBestX = ttRandomMove.iColumn
               giBestY = ttRandomMove.iRow.
END.
EMPTY TEMP-TABLE ttRandomMove.

ASSIGN gcBestCounters = gcBestCounters + "|" +  STRING(giBestX) + "," + STRING(giBestY). 
         
END PROCEDURE.

PROCEDURE prBuildGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE VARIABLE iY          AS INTEGER  NO-UNDO.
DEFINE VARIABLE iX          AS INTEGER  NO-UNDO.
 
FIND ttBoard NO-ERROR.
IF AVAILABLE ttBoard THEN DO: 
    DO iX = 1 TO 8:
        
        DO iY = 1 TO 8: 
            
            CREATE ttPosition.
            ASSIGN ttPosition.iColumn = iX
                   ttPosition.iRow = iY
                   ttPosition.iValue = ttBoard.board[(ttPosition.iRow - 1) * 8 + ttPosition.iColumn].
                   
        END.
    END.
END.
 
PUT STREAM strOutput UNFORMATTED "Starting Board:" SKIP. 

FOR EACH ttPosition:
    
    PUT STREAM strOutput UNFORMATTED ttPosition.iValue.
    
    IF ttPosition.iColumn = 8 THEN
        PUT STREAM strOutput UNFORMATTED SKIP.
    ELSE 
        PUT STREAM strOutput UNFORMATTED ",".
    
END. 

END PROCEDURE.

PROCEDURE prCheckForFile:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icFile      AS CHARACTER    NO-UNDO.
DEFINE OUTPUT PARAMETER olFound    AS LOGICAL      NO-UNDO.
 
DEFINE VARIABLE iChecks AS INTEGER NO-UNDO. 
 
CHECKLOOP:
REPEAT ON STOP UNDO, RETRY:   
    
    IF SEARCH(icFile) <> ? THEN DO:
        ASSIGN olFound = TRUE.
        LEAVE CHECKLOOP.
    END.
 
    PAUSE 5 NO-MESSAGE.
    
    ASSIGN iChecks = iChecks + 1.
    
    IF iChecks > 6 THEN
        LEAVE CHECKLOOP.
    
END. 


END PROCEDURE.

PROCEDURE prCheckForInvite:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER icFile      AS CHARACTER    NO-UNDO.
DEFINE OUTPUT PARAMETER olFound    AS LOGICAL      NO-UNDO.
 
DEFINE VARIABLE iChecks AS INTEGER NO-UNDO. 
 
CHECKLOOP:
REPEAT ON STOP UNDO, RETRY:   
    
    IF SEARCH(icFile) <> ? THEN DO:
        ASSIGN olFound = TRUE.
        LEAVE CHECKLOOP.
    END.
    
    PAUSE 5 NO-MESSAGE.
        
    ASSIGN iChecks = iChecks + 1.
    
    IF iChecks > 30 THEN
        LEAVE CHECKLOOP.
    
END. 

END PROCEDURE.

PROCEDURE prCheckNext:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iiRow        AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER iiColumn     AS INTEGER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioiCounter   AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iocCounters  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER olLeave     AS LOGICAL  NO-UNDO.

DEFINE BUFFER bttPosition FOR ttPosition.

    FIND bttPosition WHERE 
         bttPosition.iRow = iiRow AND 
         bttPosition.iColumn = iiColumn NO-ERROR. 
    IF AVAILABLE bttPosition THEN DO: 
        IF bttPosition.iValue = giOpponent THEN   
            ASSIGN ioiCounter = ioiCounter + 1
                   iocCounters = iocCounters + STRING(iiColumn) + "," + STRING(iiRow) + "|". 
        ELSE IF bttPosition.iValue = giPlayer THEN
            ASSIGN olLeave = TRUE.
        ELSE 
            ASSIGN ioiCounter = 0
                   olLeave = TRUE. 

    END.
    IF olLeave THEN
        RETURN.

    IF (iiRow = giStopPoint AND LOOKUP(gcStopType, "Row,Both") <> 0) OR (iiColumn = giStopPoint AND LOOKUP(gcStopType, "Column,Both") <> 0) THEN
        ASSIGN ioiCounter = 0. //No player counter found

END PROCEDURE.

PROCEDURE prCreateCounter:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iiRow                AS INTEGER      NO-UNDO.
DEFINE INPUT PARAMETER iiColumn             AS INTEGER      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioiCounters   AS INTEGER      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iocCounters   AS CHARACTER    NO-UNDO.

ASSIGN iocCounters = TRIM(iocCounters, "|").

    IF ioiCounters > 0 THEN DO:
        
        FIND ttMove WHERE 
             ttMove.iRow = iiRow AND 
             ttMove.iColumn = iiColumn NO-ERROR.
        IF NOT AVAILABLE ttMove THEN DO:
            CREATE ttMove.
            ASSIGN ttMove.iRow = iiRow
                   ttMove.iColumn = iiColumn.
                   
            IF (iiRow = 1 AND iiColumn = 1) OR (iiRow = 1 AND iiColumn = 8) OR (iiRow = 8 AND iiColumn = 1) OR (iiRow = 8 AND iiColumn = 1) THEN
                ttMove.iValue = 3.
            ELSE IF (iiRow = 1 OR iiColumn = 1 OR iiRow = 8 OR  iiColumn = 8) THEN
                ttMove.iValue = 2.
            ELSE 
                ttMove.iValue = 1.
        END.
        ASSIGN ttMove.iCounters = ttMove.iCounters + ioiCounters
               ttMove.cCounters = ttMove.cCounters + "," + iocCounters
               ttMove.cCounters = TRIM(ttMove.cCounters, ",").
    END. 
    
    ASSIGN ioiCounters = 0
           iocCounters = "".

END PROCEDURE.

PROCEDURE prDescending:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE VARIABLE iCheck      AS INTEGER      NO-UNDO.
DEFINE VARIABLE iCounters   AS INTEGER      NO-UNDO.
DEFINE VARIABLE iCounters1   AS INTEGER      NO-UNDO.
DEFINE VARIABLE iDiagonal   AS INTEGER      NO-UNDO.
DEFINE VARIABLE lStop       AS LOGICAL      NO-UNDO.
DEFINE VARIABLE lStop1      AS LOGICAL      NO-UNDO.
DEFINE VARIABLE lStop2      AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cCounters   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cCounters1   AS CHARACTER    NO-UNDO.

ASSIGN giStopPoint = 1.

FOR EACH ttPosition USE-INDEX idxDesc:
                  
    IF ttPosition.iValue <> 0 THEN
        NEXT. 
         
    ASSIGN iCounters    = 0
           lStop1       = FALSE 
           lStop2       = FALSE
           iDiagonal    = 0
           gcStopType   = "Column".
             
    CHECKLOOP:
    DO iCheck = ttPosition.iColumn - 1 TO 1 BY -1: 

        RUN prCheckNext (INPUT ttPosition.iRow, INPUT iCheck, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters, OUTPUT lStop).
    
        IF lStop THEN
        LEAVE CHECKLOOP.
        
    END. 
    RUN prCreateCounter(ttPosition.iRow,  ttPosition.iColumn, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters).
    
    ASSIGN gcStopType = "Row".
           
    CHECKLOOP2:
    DO iCheck = ttPosition.iRow - 1 TO 1 BY -1:
        RUN prCheckNext (INPUT iCheck, INPUT ttPosition.iColumn, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters, OUTPUT lStop).
   
        IF lStop THEN
        LEAVE CHECKLOOP2. 
    END.
    RUN prCreateCounter(ttPosition.iRow,  ttPosition.iColumn, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters).
 
    ASSIGN gcStopType = "Both".
    
    CHECKLOOP3:
    DO iCheck = ttPosition.iRow - 1 TO 1 BY -1:
        ASSIGN iDiagonal = iDiagonal + 1.
                
        IF lStop1 = FALSE THEN
            RUN prCheckNext (INPUT iCheck, INPUT ttPosition.iColumn - iDiagonal, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters, OUTPUT lStop1).
        
        IF lStop2 = FALSE THEN
            RUN prCheckNext (INPUT iCheck, INPUT ttPosition.iColumn + iDiagonal, INPUT-OUTPUT iCounters1, INPUT-OUTPUT cCounters1, OUTPUT lStop2).

        IF lStop1 AND lStop2 THEN
            LEAVE CHECKLOOP3. 
    END.
    RUN prCreateCounter(ttPosition.iRow,  ttPosition.iColumn, INPUT-OUTPUT iCounters, INPUT-OUTPUT cCounters).
    RUN prCreateCounter(ttPosition.iRow,  ttPosition.iColumn, INPUT-OUTPUT iCounters1, INPUT-OUTPUT cCounters1).
END.

END PROCEDURE.

PROCEDURE prMain:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileOut    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRetOK      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFound      AS LOGICAL   NO-UNDO.

DEFINE VARIABLE httBoard    AS HANDLE    NO-UNDO.
DEFINE VARIABLE httBestMove AS HANDLE    NO-UNDO.
DEFINE VARIABLE httGame     AS HANDLE    NO-UNDO.
 
 
ASSIGN cSourceType = "file" 
       cReadMode   = "empty".

RUN prCheckForInvite (INPUT "D:\Othello\V1\invite-" + gicRequestID + ".json", OUTPUT lFound).
IF lFound = FALSE THEN DO:
    MESSAGE "Timeout waiting for invite"
    VIEW-AS ALERT-BOX.
    RETURN. 
END.
 
OUTPUT STREAM strOutput TO VALUE( "D:\dholliday\Othello\100-Random\Log-" + gicRequestID + ".log" ).

CREATE ttGame.
ASSIGN httGame = TEMP-TABLE ttGame:HANDLE
       lRetOK = httGame:READ-JSON(cSourceType, "D:\Othello\V1\invite-" + gicRequestID + ".json", cReadMode). 
    
IF lRetOk THEN DO:
    FIND ttGame NO-ERROR.
    IF AVAILABLE ttGame THEN
        ASSIGN gcGameGUID = ttGame.gameGUID.
END. 

IF gcGameGUID = "" THEN DO:
    MESSAGE "Invalid Game ID"
    VIEW-AS ALERT-BOX.
    RETURN. 
END.

PUT STREAM strOutput UNFORMATTED "Game Started".

ASSIGN gcPlayerName = "David".

ASSIGN cFile       = "D:\Othello\V1\game-" + gcGameGUID + "\game-" + gcGameGUID + ".json"
       cFileOut    = "D:\Othello\V1\game-" + gcGameGUID + "\move-" + gicRequestID + ".json". 
     
MOVELOOP:  
REPEAT:
     
    PAUSE 0.5 NO-MESSAGE.
    
    RUN prCheckForFile (INPUT cFile, OUTPUT lFound).
    
    IF lFound = FALSE THEN DO:
        MESSAGE "Move Timeout"
        VIEW-AS ALERT-BOX.
    END. 
    
    EMPTY TEMP-TABLE ttBoard.
    CREATE ttBoard.
      
    ASSIGN httBoard = TEMP-TABLE ttBoard:HANDLE.
    ASSIGN cSourceType = "file" 
           cReadMode   = "empty".
    
    lRetOK = httBoard:READ-JSON(cSourceType, cFile, cReadMode).   
    
    FIND ttBoard NO-ERROR.
    IF NOT AVAILABLE ttBoard THEN NEXT. 
    
    IF gcPlayer = "" THEN DO:
        IF ttBoard.whitePlayer = gcPlayerName THEN
            ASSIGN giPlayer = 1
                   gcPlayer = "white" 
                   giOpponent = 2.
        ELSE         
            ASSIGN giPlayer = 2
                   gcPlayer = "black" 
                   giOpponent = 1.
                   
        PUT STREAM strOutput UNFORMATTED "Colour = " gcPlayer.
    END.
     
    IF ttBoard.winner <> "" THEN DO:
        IF ttBoard.winner = gcPlayerName THEN DO:
            PUT STREAM strOutput UNFORMATTED "Yay. I Won" SKIP 
                                                "Colour: " gcPlayer SKIP 
                                                "Black: " ttBoard.blackCounters SKIP 
                                                "White: " ttBoard.whiteCounters SKIP 
                                                "Game ID: " gcGameGUID SKIP
                                                "Moves: " ttBoard.turnsSoFar SKIP.
        END.
        ELSE IF ttBoard.winner = "Server-Non-Random" THEN DO:
            PUT STREAM strOutput UNFORMATTED "Boo, I Lost" SKIP 
                                                "Colour: " gcPlayer SKIP 
                                                "Black: " ttBoard.blackCounters SKIP 
                                                "White: " ttBoard.whiteCounters SKIP 
                                                "Game ID: " gcGameGUID SKIP
                                                "Moves: " ttBoard.turnsSoFar SKIP.
        END.
        ELSE DO:
            PUT STREAM strOutput UNFORMATTED "DRAW" SKIP 
                                                "Colour: " gcPlayer SKIP 
                                                "Black: " ttBoard.blackCounters SKIP 
                                                "White: " ttBoard.whiteCounters SKIP 
                                                "Game ID: " gcGameGUID SKIP
                                                "Moves: " ttBoard.turnsSoFar SKIP.
        END.
        LEAVE MOVELOOP.
    END.
        
    IF ttBoard.nextMove <> gcPlayer THEN 
        NEXT.    
        
    IF ttBoard.turnsSoFar = giTurns AND ttBoard.turnsSoFar <> 0 THEN DO:
        PAUSE 5 NO-MESSAGE.
        NEXT. 
    END.
        
    ASSIGN giMoveTime = ETIME
           giTurns = ttBoard.turnsSoFar.
        
    PUT STREAM strOutput UNFORMATTED "Turn = " ttBoard.turnsSoFar + 1 SKIP.
    
    RUN prBuildGrid.
    RUN prScanGrid.
    
    RUN prBestMove (ttBoard.turnsSoFar).
    
    IF glLoser = TRUE THEN DO:
        PUT STREAM strOutput UNFORMATTED "Boo, I Lost" SKIP 
                                                "Colour: " gcPlayer SKIP 
                                                "Black: " ttBoard.blackCounters SKIP 
                                                "White: " ttBoard.whiteCounters SKIP 
                                                "Game ID: " gcGameGUID SKIP
                                                "Moves: " ttBoard.turnsSoFar SKIP.
        MESSAGE "I Lost"
            VIEW-AS ALERT-BOX.
            LEAVE MOVELOOP.
    END.
    
    RUN prRedrawGrid.   
    
    FIND ttBestMove NO-ERROR.
    RUN prGenerateJSON("ttBestMove", cFileOut). 
    
    RUN prReset.
    
END.
END PROCEDURE.

OUTPUT STREAM strOutput CLOSE.

PROCEDURE prRedrawGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCounters   AS INTEGER  NO-UNDO.
DEFINE VARIABLE iY          AS INTEGER  NO-UNDO.
DEFINE VARIABLE iX          AS INTEGER  NO-UNDO.

FIND ttBoard NO-ERROR.
IF AVAILABLE ttBoard THEN DO: 
    DO iCounters = 1 TO NUM-ENTRIES(gcBestCounters, "|"):
        
        FIND ttPosition WHERE ttPosition.iColumn = INTEGER(ENTRY(1, ENTRY(iCounters, gcBestCounters, "|"))) AND
                              ttPosition.iRow = INTEGER(ENTRY(2, ENTRY(iCounters, gcBestCounters, "|"))) NO-ERROR.
        IF AVAILABLE ttPosition THEN DO:
            ASSIGN ttPosition.iValue = giPlayer
                   ttBoard.board[(ttPosition.iRow - 1) * 8 + ttPosition.iColumn] = ttPosition.iValue.
        END.
    END.   
    
     
    ASSIGN giMoveTime = (ETIME - giMoveTime)
           giMoveTime = giMoveTime / 1000.
           
    CASE giPlayer:
        WHEN 1 THEN
            ASSIGN ttBoard.whiteSecondsRemaining = ttBoard.whiteSecondsRemaining - giMoveTime
                   ttBoard.whitecounters = ttBoard.blackCounters + NUM-ENTRIES(gcBestCounters, "|")
                   ttBoard.nextMove = "Black"
                   ttBoard.lastMoveWhite = STRING(giBestX) + "," + STRING(giBestY).
        OTHERWISE  
            ASSIGN ttBoard.blackSecondsRemaining = ttBoard.blackSecondsRemaining - giMoveTime
                   ttBoard.blackCounters = ttBoard.blackCounters + NUM-ENTRIES(gcBestCounters, "|")
                   ttBoard.nextMove = "White"
                   ttBoard.lastMoveBlack = STRING(giBestX) + "," + STRING(giBestY).

    END CASE.

    ASSIGN ttBoard.gameLengthInSeconds = ttBoard.gameLengthInSeconds + giMoveTime
           ttBoard.turnsSoFar = ttBoard.turnsSoFar + 1.
    
    CREATE ttBestMove.
    ASSIGN ttBestMove.gameGUID = gcGameGUID
           ttBestMove.iColumn = giBestX 
           ttBestMove.iRow = giBestY
           ttBestMove.iMove = ttBoard.turnsSoFar.
    
    PUT STREAM strOutput UNFORMATTED "Best Move:" SKIP.
    PUT STREAM strOutput UNFORMATTED "X=" giBestX ", Y=" giBestY ", Counters Win=" gcBestCounters SKIP.
    PUT STREAM strOutput UNFORMATTED "New Board:" SKIP.
     
    DO iCounters = 1 TO 64:
        
        PUT STREAM strOutput UNFORMATTED ttBoard.board[iCounters].
    
        IF iCounters MOD 8 = 0 THEN
            PUT STREAM strOutput UNFORMATTED SKIP.
        ELSE 
            PUT STREAM strOutput UNFORMATTED ",". 
    END.
END. 
 
END PROCEDURE.

PROCEDURE prReset:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE ttMove.
EMPTY TEMP-TABLE ttBestMove. 
EMPTY TEMP-TABLE ttPosition. 

ASSIGN giBestY = 0
       giBestX = 0
       gcBestCounters = "".
   
END PROCEDURE.

PROCEDURE prScanGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

RUN prAscending.
RUN prDescending.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fnSquare RETURNS LOGICAL 
    ( INPUT iActualRow AS INTEGER, INPUT iActualCol AS INTEGER, INPUT iCheckRow AS INTEGER, INPUT iCheckCol AS INTEGER, INPUT iCorner AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE lSkip   AS LOGICAL  NO-UNDO.
    
    IF iCheckRow = iActualRow AND 
       iCheckCol = iActualCol THEN
        ASSIGN lSkip = TRUE. 

    FIND ttBoard NO-ERROR.
    IF AVAILABLE ttBoard AND ttBoard.board[iCorner] = giPlayer THEN 
        lSkip = FALSE.
    
    RETURN lSkip.
        
END FUNCTION.

  
