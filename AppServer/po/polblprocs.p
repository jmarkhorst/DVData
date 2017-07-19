
/*------------------------------------------------------------------------
    File        : polblprocs.p
    Purpose     : programs for polable processes

    Syntax      :

    Description : PO Label Procs programs	

    Author(s)   : John Markhorst
    Created     : Mon Jan 11 13:24:21 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.  

/* ********************  Preprocessor Definitions  ******************** */
{po/polbllist.i} 

DEF INPUT PARAMETER ip-process AS CHARACTER.
DEF INPUT-OUTPUT PARAMETER DATASET FOR dspolist.
DEF INPUT PARAMETER filter AS CHARACTER.
DEF STREAM out1. 
DEF VAR v-dtc        AS CHAR NO-UNDO.
DEF VAR op-directory AS CHAR NO-UNDO.

/* ***************************  Main Block  *************************** */
ASSIGN 
            v-dtc = STRING(NOW)
            v-dtc = REPLACE(v-dtc,'/','')
            v-dtc = REPLACE(v-dtc,' ','-') 
            v-dtc = REPLACE(v-dtc,':','')
            v-dtc = SUBSTRING(v-dtc,1,R-INDEX(v-dtc,'-') - 1)
            op-directory = "/WebSites/WebRest/Logs/"
            /*  "C:\workspace\TestLogs\" */
            .   
OUTPUT stream out1 to value(op-directory + "polblprocs-" + v-dtc + ".txt").


/*    for debugging propath issue.                                                                             */
/*PUT STREAM out1 UNFORMATTED "Propath: " PROPATH SKIP.                            */
/*PUT STREAM out1 UNFORMATTED "============================" SKIP                  */
/*          "PATH:" OS-GETENV("path") SKIP                                         */
/*          "============================" SKIP .                                  */
/*PUT STREAM out1 UNFORMATTED "In polblprocs.p program **** " SKIP.                */
/*PUT STREAM out1 UNFORMATTED "gd-lblprnt-csv.r =" SEARCH("gd-lblprnt-csv.r") SKIP.*/
/*PUT STREAM out1 UNFORMATTED "Input Parameters:" SKIP                             */
/*        "ip-process:" ip-process SKIP                                            */
/*        "filter:" filter SKIP .                                                  */


CASE ip-process:
    WHEN 'SubmitPoLblList' THEN
        DO: 
            PUT STREAM out1 UNFORMATTED 'submit' SKIP.
            RUN SubmitPoLblList.
            PUT STREAM out1 UNFORMATTED 'submit-out' SKIP. 
        END.
    WHEN 'create' THEN 
        DO:
            
        END.
END CASE.
OUTPUT stream out1 close.

/* **********************  Internal Procedures  *********************** */

PROCEDURE SubmitPoLblList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR op-file-name AS CHAR NO-UNDO.
    DEF VAR op-error     AS LOG  NO-UNDO.
    DEF VAR v-template-file AS CHAR NO-UNDO.
    DEF VAR v-template-name AS CHAR NO-UNDO.
    DEF VAR v-template-dir AS CHAR NO-UNDO.
    DEF VAR v-template-type AS CHAR NO-UNDO.
    DEF VAR v-label-size AS CHAR NO-UNDO.
    DEF VAR v-outfile AS CHAR NO-UNDO.
    
    v-template-dir = "templates/".
    /*v-template-dir = "\\erp-sgtest\Test\Pc\obj11\docx\templates\".*/
    
    IF v-label-size = ''  THEN v-label-size = 'Avery5164'. /*'3x5'*/ 
    v-template-type = 'Inv'.
    
    
    
    FOR EACH ttpolist NO-LOCK
        WHERE ttpolist.PrintPo = YES 
        :
        EXPORT STREAM out1  DELIMITER ','  ttpolist .
        
        IF CAN-FIND(FIRST po-header NO-LOCK 
            WHERE po-header.SYSTEM-ID = 'DV' 
            AND po-header.po-num = ttpolist.PoNum
            AND po-header.po-type = "Special Order"
            ) THEN v-template-type = 'GD'.
        
        RUN create-gd-csv1.p (INPUT ttpolist.PoNum,
                     OUTPUT op-file-name,
                     OUTPUT op-error).
                     
        PUT STREAM out1 UNFORMATTED 'run- ' op-file-name ' : ' op-error SKIP.      
        
        v-label-size  = TRIM(ttpolist.lblSize) .
        
        v-template-name = v-template-dir + v-template-type + "Label" + v-label-size + ".dfw" .
        v-template-file = SEARCH(v-template-name).
        
        PUT STREAM out1 UNFORMATTED  SKIP SKIP SKIP 'run gd-lblprnt-csv.p: ' SKIP
        "opfilename:" op-file-name SKIP 
        'templatename:' v-template-name SKIP 
        'search: ' SEARCH(v-template-name) SKIP
        'gd-lblprnt-csv.p: ' SEARCH('gd-lblprnt-csv.p') SKIP 
        'gd-lblprnt-csv.r: ' SEARCH('gd-lblprnt-csv.r') SKIP
        " running **********" SKIP.
        
        
        RUN gd-lblprnt-csv.p (INPUT op-file-name
                             ,INPUT v-template-file
                             ,OUTPUT v-outfile).
                             
        PUT STREAM out1 UNFORMATTED 
            'v-outfile: ' v-outfile SKIP
            "============================" SKIP .                        
        
    END.  

END PROCEDURE.

