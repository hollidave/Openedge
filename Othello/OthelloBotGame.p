
{OthelloJSON.i}

DEFINE VARIABLE cLocation   AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE  ttGame 
    FIELD cGUID AS CHARACTER LABEL "requestGUID"
    FIELD cName AS CHARACTER LABEL "playerName"
    FIELD cOpponent AS CHARACTER LABEL "opponentName"
    FIELD iMin AS INTEGER LABEL "minimumGameLengthInSeconds"
    FIELD iMax AS INTEGER LABEL "maximumGameLengthInSeconds" 
INDEX idxGUID cGUID.

ASSIGN cLocation = "D:\Othello\V1\". 

RUN prStartGame.
 



/* **********************  Internal Procedures  *********************** */


PROCEDURE prCreateGame:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icGUID   AS CHARACTER    NO-UNDO.

    CREATE ttGame.
    ASSIGN ttGame.cGUID = icGUID
           ttGame.cName = "David-Bot"
           ttGame.cOpponent = "Server-Test"
           ttGame.iMin = 1
           ttGame.iMax = 999999.

END PROCEDURE.

PROCEDURE prStartGame:

    DEFINE VARIABLE cFile       AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE lRetOk      AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE httGame     AS HANDLE       NO-UNDO.
    DEFINE VARIABLE cGUID       AS CHARACTER    NO-UNDO.

    ASSIGN cGUID = GUID(GENERATE-UUID).

    RUN prCreateGame (INPUT cGUID).
     
    cFile = cLocation + "request-" + cGUID + ".json". 
    RUN prGenerateJSON("ttGame", cFile).  
 MESSAGE "here"
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RUN VALUE("D:\dholliday\CaseflowOE\CaseflowOE 20.08\ApplicationCode\util\OthelloBot.p") (INPUT cGUID).

    EMPTY TEMP-TABLE ttGame.

END PROCEDURE. 




