/*------------------------------------------------------------------------
    File        : readdircontents.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Markhorst
    Created     : Thu Dec 28 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE ttpopdflabels
    FIELD VendorCode  AS CHAR 
    FIELD PoNum       AS CHAR     FORMAT 'x(15)'
    FIELD Csvlink     AS CHAR     FORMAT 'x(50)'
    FIELD DocXlink    AS CHAR     FORMAT 'x(50)'
    FIELD CSVmodDate  AS DATETIME
    FIELD DocXmodDate AS DATETIME
    FIELD listseq     AS INT
    INDEX idx1 IS PRIMARY ponum listseq
    .
DEFINE DATASET dspdflabels FOR ttpopdflabels.

/* ***************************  Definitions  ************************** */
DEF STREAM in1.

RUN load-dir.
RUN display_pdflabels.


PROCEDURE load-dir:

    DEF VAR ip-supplier-code AS CHAR . 
    DEF VAR v-file           AS CHAR     FORMAT 'x(50)' .
    DEF VAR op-directory     AS CHAR     NO-UNDO.
    DEF VAR v-seq            AS INT      NO-UNDO.
    DEF VAR v-vendor         AS CHAR     NO-UNDO.
    DEF VAR v-ponum          AS CHAR     FORMAT 'x(15)' NO-UNDO.
    DEF VAR v-fileext        AS CHAR     NO-UNDO.
    DEF VAR v-fdatetime      AS DATETIME NO-UNDO.
    DEF VAR v-char           AS CHAR     NO-UNDO.



    ip-supplier-code = '900'.
    op-directory = "\\vweb2\WebSites\DVC\Labels\" + trim(ip-supplier-code) + '\'.

    /*op-directory = 'S:\DVC\Labels\900\'.*/

    INPUT STREAM in1 FROM OS-DIR(op-directory) NO-ATTR-LIST. 
    REPEAT:
        IMPORT STREAM in1 v-file.
        FILE-INFO:FILE-NAME = op-directory + v-file.
        IF NOT FILE-INFO:FILE-TYPE BEGINS "F" THEN NEXT.
        /*IF NOT FILE-INFO:FILE-MOD-DATE = TODAY THEN NEXT.*/
        ASSIGN 
            v-vendor  = SUBSTRING(v-file,1,INDEX(v-file,'_') - 1 )
            v-ponum   = SUBSTRING(v-file,INDEX(v-file,'_') + 1,11)
            v-fileext = SUBSTRING(v-file,INDEX(v-file,'.') + 1)
            .
        
        v-char = STRING(DATE(FILE-INFO:FILE-MOD-DATE)) + " " + string(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS").
        v-fdatetime = DATETIME(v-char).

        /*        IF v-ponum = "fl000026871" THEN*/
/*        DISPLAY v-file                                */
/*            v-char FORMAT 'x(20)'                     */
/*            v-fdatetime                               */
/*            /*INDEX(v-file,'.')*/                     */
/*            v-fileext                                 */
/*            v-vendor                                  */
/*            v-ponum                                   */
/*            STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")*/
/*            DATE(FILE-INFO:FILE-MOD-DATE)             */
/*            FILE-INFO:FULL-PATHNAME                   */
/*            FILE-INFO:PATHNAME                        */
/*            FILE-INFO:FILE-TYPE                       */
/*            WITH WIDTH 200                            */
/*            /*VIEW-AS ALERT-BOX*/                     */
/*            .                                         */
     
        FIND FIRST ttpopdflabels EXCLUSIVE-LOCK
            WHERE ttpopdflabels.ponum = v-ponum
            NO-ERROR.
     
        IF NOT AVAILABLE ttpopdflabels THEN 
        DO: 
            CREATE ttpopdflabels.
            ASSIGN 
                v-seq                  = v-seq + 1
                ttpopdflabels.listseq    = v-seq
                ttpopdflabels.vendorcode = v-vendor
                ttpopdflabels.ponum      = v-ponum
                .    
        END.
    
        IF v-fileext = 'docx' THEN 
        DO:
            IF ttpopdflabels.DocXmodDate < v-fdatetime 
                OR ttpopdflabels.DocXmodDate = ?
                THEN
                ASSIGN 
                    ttpopdflabels.DocXlink    = v-vendor + '/' + v-file
                    ttpopdflabels.DocXmodDate = v-fdatetime.
        END.
        IF v-fileext = 'csv' THEN
        DO:
            IF ttpopdflabels.CSVmodDate < v-fdatetime 
                OR ttpopdflabels.CSVmodDate = ?
                THEN
                ASSIGN 
                    ttpopdflabels.csvlink    = v-vendor + '/' + v-file
                    ttpopdflabels.CSVmodDate = v-fdatetime.
                .
        END.
        
     
        
        
    END.  

    INPUT STREAM in1 CLOSE.
END PROCEDURE.

PROCEDURE display_pdflabels:

    FOR EACH ttpopdflabels NO-LOCK
/*        WHERE ponum = "fl000026871"*/
        :
        DISPLAY ttpopdflabels. 
    END. 

END PROCEDURE. 


