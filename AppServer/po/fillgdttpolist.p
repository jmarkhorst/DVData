
/*------------------------------------------------------------------------
    File        : fillgdttpolist.p
    Purpose     : 

    Syntax      :

    Description : Fillttpolist for uploaded gd label files

    Author(s)   : John
    Created     : Wed May 25 14:29:15 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.
{polbllist.i}
DEFINE INPUT PARAMETER filter AS CHARACTER. 
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dspolist. 

DEFINE STREAM in1.
DEFINE STREAM out1.

/* ********************  Preprocessor Definitions  ******************** */
DEFINE VARIABLE x              AS INTEGER        NO-UNDO.
DEFINE VARIABLE v-dtc          AS CHARACTER      NO-UNDO.
DEFINE VARIABLE v-vendor-code  AS CHARACTER      NO-UNDO.
DEFINE VARIABLE v-lblsize      AS CHARACTER      NO-UNDO.
DEFINE VARIABLE v-sysid        AS CHARACTER INITIAL "DV" NO-UNDO.
DEFINE VARIABLE op-directory   AS CHARACTER      NO-UNDO.
    
EMPTY TEMP-TABLE ttpolist.

ASSIGN 
    /*op-directory = "\\vweb2\WebSites\DVC\Labels\" */
    op-directory = IF OPSYS = 'unix' THEN "/WebSites/DVC/Labels/"
       ELSE "\\vweb2\WebSites\DVC\Labels\" 
       .

/* ***************************  Main Block  *************************** */

RUN getDTfilestamp.
IF filter NE ? THEN
DO:
    /*    PUT STREAM out1 UNFORMATTED         "run parcefilter : " filter SKIP.*/
    RUN ParceFilter.     
END.
FIND FIRST suppname NO-LOCK 
    WHERE suppname.system-id = v-sysid 
    AND suppname.supplier-code = v-vendor-code
    .

/*PUT STREAM out1 UNFORMATTED  "Suppname avail:" AVAILABLE(suppname) SKIP.*/
IF AVAILABLE(suppname) THEN
DO:
    op-directory = op-directory + trim(suppname.supplier-code) + '/GD/'.
    RUN getFilesList.  
END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE getDTfilestamp:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

        
    ASSIGN 
        v-dtc = STRING(NOW)
        v-dtc = REPLACE(v-dtc,'//','')
        v-dtc = REPLACE(v-dtc,' ','-') 
        v-dtc = REPLACE(v-dtc,':','')
        v-dtc = SUBSTRING(v-dtc,1,R-INDEX(v-dtc,'-') - 1)
        . 

END PROCEDURE.

PROCEDURE getFilesList:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /* code from readdircontects.p */

    DEFINE VARIABLE ip-supplier-code AS CHARACTER . 
    DEFINE VARIABLE v-file           AS CHARACTER     FORMAT 'x(50)' .
    
    DEFINE VARIABLE v-seq            AS INTEGER      NO-UNDO.
    DEFINE VARIABLE v-vendor         AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE v-ponum          AS CHARACTER     FORMAT 'x(15)' NO-UNDO.
    DEFINE VARIABLE v-fileext        AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE v-fdatetime      AS DATETIME NO-UNDO.
    DEFINE VARIABLE v-char           AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE v-bpo            AS INTEGER      NO-UNDO.
    DEFINE VARIABLE v-epo            AS INTEGER      NO-UNDO.


    ip-supplier-code = suppname.supplier-code.
    
/*    op-directory = "\\vweb2\WebSites\DVC\Labels\" + trim(ip-supplier-code) + '\'.*/
    
    FILE-INFO:FILE-NAME = op-directory.
    IF FILE-INFO:FILE-TYPE = ? THEN 
    DO:
       OS-CREATE-DIR value(op-directory).
    END.

    FILE-INFO:FILE-NAME = op-directory.
    IF FILE-INFO:FILE-TYPE = ? THEN RETURN.  
        
    
    INPUT STREAM in1 FROM OS-DIR(op-directory) NO-ATTR-LIST. 
    REPEAT:
        IMPORT STREAM in1 v-file.
        FILE-INFO:FILE-NAME = op-directory + v-file.
        IF NOT FILE-INFO:FILE-TYPE BEGINS "F" THEN NEXT.
        /*IF NOT FILE-INFO:FILE-MOD-DATE = TODAY THEN NEXT.*/
        ASSIGN 
            v-bpo     = INDEX(v-file,'_') + 1
            v-epo     = INDEX(v-file,'-') 
            v-vendor  = SUBSTRING(v-file,1,INDEX(v-file,'_') - 1 )
            v-ponum   = SUBSTRING(v-file,v-bpo,v-epo - v-bpo) 
            /*SUBSTRING(v-file,INDEX(v-file,'_') + 1, INDEX(v-file,'-' - 1 - INDEX(v-file,'_') + 1 )  /*11*/)*/
            v-fileext = SUBSTRING(v-file,INDEX(v-file,'.') + 1)
            .
        
        v-char = STRING(DATE(FILE-INFO:FILE-MOD-DATE)) + " " + string(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS").
        v-fdatetime = DATETIME(v-char).
        
        /*        PUT STREAM out1 UNFORMATTED v-file " : ".*/
        FIND FIRST ttpolist EXCLUSIVE-LOCK
            WHERE ttpolist.ponum = v-ponum
            NO-ERROR.
     
        IF NOT AVAILABLE ttpolist THEN NEXT.
        /*        PUT STREAM out1 UNFORMATTED "Found "  v-fileext " | " v-fdatetime.*/
            
        IF v-fileext = 'docx' THEN 
        DO:
            IF ttpolist.DocXDate < v-fdatetime 
                OR ttpolist.DocXDate = ?
                THEN
                ASSIGN 
                    ttpolist.DocXfile = v-vendor + '/' + v-file
                    ttpolist.DocXDate = v-fdatetime
                    ttpolist.LinkName = v-ponum
                    .
        END.
        IF v-fileext = 'csv' THEN
        DO:
            IF ttpolist.CSVDate < v-fdatetime 
                OR ttpolist.CSVDate = ?
                THEN
            DO:
                    
                ASSIGN 
                    ttpolist.CSVfile  = v-vendor + '/' + v-file
                    ttpolist.CSVDate  = v-fdatetime
                    ttpolist.LinkName = v-ponum
                    .
            /*                PUT STREAM out1 UNFORMATTED " assigned".*/
            END.
        END.
    /*        PUT STREAM out1 UNFORMATTED " done." SKIP.*/
    END.  
    
END PROCEDURE.

PROCEDURE ParceFilter:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE jsonParser       AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE jsonObject       AS JsonObject        NO-UNDO.
    DEFINE VARIABLE filterjsonarray  AS JsonArray         NO-UNDO.
    DEFINE VARIABLE filterjsonObject AS JsonObject        NO-UNDO.
    DEFINE VARIABLE logicFilter      AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE ablFilter        AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE abljsontxt       AS CHARACTER         NO-UNDO.
    DEFINE    VARIABLE      v-len            AS INTEGER               NO-UNDO. 
    DEFINE VARIABLE myLongchar       AS LONGCHAR          NO-UNDO.
    
    /*    PUT STREAM out1 UNFORMATTED  "filter= " filter SKIP.*/
    
    jsonParser = NEW ObjectModelParser().
    jsonObject = CAST(jsonParser:Parse(filter), jsonObject).
    logicFilter = jsonObject:GetCharacter("logic") NO-ERROR.
    
    abljsontxt = jsonObject:GetJsonText("filters").

    
    /*    PUT STREAM out1 UNFORMATTED          */
    /*        "***********5" SKIP              */
    /*        "logicfilter = " logicfilter SKIP*/
    /*        "abljsontxt = " abljsontxt SKIP  */
    /*        .                                */
    /*   
    DISPLAY 
        "logicfilter = " logicfilter SKIP
       "abljsontxt = " abljsontxt SKIP
        .
    */    
    
    filterjsonarray = CAST(jsonParser:Parse(abljsontxt), JsonArray).
        
    filterjsonObject = filterjsonarray:GetJsonObject(1).
    v-vendor-code =  filterjsonObject:GetCharacter("VendorCode") NO-ERROR.
    v-lblsize =  filterjsonObject:GetCharacter("LblSize") NO-ERROR.
    
    
    /*    PUT STREAM out1 UNFORMATTED              */
    /*        "***********" SKIP                   */
    /*        "v-vendor-code = " v-vendor-code SKIP*/
    /*        "v-lblsize = " v-lblsize SKIP        */
    /*        "***********" SKIP                   */
    /*        .                                    */
    /*DISPLAY "v-vendor-code = " v-vendor-code.*/
    
    /*    PUT STREAM out1 UNFORMATTED     */
    /*        "cutup:" v-vendor-code SKIP.*/
    
    IF NOT CAN-FIND(FIRST suppname  
        WHERE suppname.system-id = v-sysid 
        AND suppname.supplier-code = v-vendor-code) 
        THEN v-vendor-code = '426'.

END PROCEDURE.
