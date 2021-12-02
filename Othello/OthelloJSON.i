
/*------------------------------------------------------------------------
    File        : GenerateJSON.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DHolliday
    Created     : Tue May 05 11:22:25 BST 2020
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Json.ObjectModel.*.  

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE prGenerateJSON:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER icTable  AS CHARACTER    NO-UNDO. 
DEFINE INPUT PARAMETER icFile   AS CHARACTER    NO-UNDO. 

DEFINE VARIABLE hBuffer     AS HANDLE       NO-UNDO. 
DEFINE VARIABLE hQuery      AS HANDLE       NO-UNDO.
DEFINE VARIABLE lQueryOk    AS LOGICAL      NO-UNDO. 
DEFINE VARIABLE iFields     AS INTEGER      NO-UNDO.   
DEFINE VARIABLE cQuery      AS CHARACTER    NO-UNDO. 
    
DEFINE VARIABLE jSon_string AS LONGCHAR                             NO-UNDO. 
DEFINE VARIABLE oJSON       AS Progress.Json.ObjectModel.JsonObject. 
  
ASSIGN cQuery = "FOR EACH " + STRING(icTable).
    
CREATE QUERY hQuery.   
CREATE BUFFER hBuffer FOR TABLE icTable.    
hQuery:ADD-BUFFER(hBuffer).  
ASSIGN lQueryOk = hQuery:QUERY-PREPARE(cQuery). 
  
IF lQueryOk THEN DO:
    oJSON = NEW JsonObject(). 

    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST(NO-LOCK). 
    DO WHILE NOT hQuery:QUERY-OFF-END:  
        
        DO iFields = 1 TO hBuffer:NUM-FIELDS:
            oJSON:add(hBuffer:BUFFER-FIELD(iFields):LABEL ,hBuffer:BUFFER-FIELD(iFields):BUFFER-VALUE).  
        END.
        
        hQuery:GET-NEXT(). 
    END.
END.  

/* Write out the JsonObject as a json file */
 
oJSON:WriteFile(icFile, TRUE). 
oJSON:Write(jSon_string,TRUE). 

FINALLY:
    DELETE OBJECT oJSON NO-ERROR.
 
    hQuery:QUERY-CLOSE() NO-ERROR.
    DELETE OBJECT hQuery NO-ERROR.
    DELETE OBJECT hBuffer NO-ERROR.
     
 END FINALLY.
 
END PROCEDURE.

