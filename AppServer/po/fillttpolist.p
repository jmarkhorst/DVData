
/*------------------------------------------------------------------------
    File        : fillttpolist.p
    Purpose     : 

    Syntax      :

    Description : Fill the ttpolist for polbllist.cls	

    Author(s)   : John
    Created     : Wed Dec 30 10:59:53 EST 2015
    Notes       :
  ----------------------------------------------------------------------*/
 
/* ***************************  Definitions  ************************** */
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.
{po/polbllist.i}  

DEFINE INPUT PARAMETER filter AS CHARACTER. 
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dspolist. 

DEFINE STREAM in1.
DEFINE STREAM out1.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE v-status       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-date         AS DATE      NO-UNDO.
DEFINE VARIABLE ip-status      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-potype       AS CHARACTER NO-UNDO.
DEFINE VARIABLE ip-potype      AS CHARACTER NO-UNDO.
DEFINE VARIABLE x              AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-lookbackdays AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-sysid        AS CHARACTER INITIAL "DV" NO-UNDO.
DEFINE VARIABLE v-po-ord-qty   AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-po-cost      AS DECIMAL   FORMAT "$>>,>>>.99" NO-UNDO.
DEFINE VARIABLE ip-ship-date   AS DATE      NO-UNDO.
DEFINE VARIABLE ip-all-dates   AS LOG       INIT TRUE NO-UNDO.  
DEFINE VARIABLE v-beg-date     AS DATE      NO-UNDO.
DEFINE VARIABLE v-end-date     AS DATE      NO-UNDO.
DEFINE VARIABLE ip-all-pos     AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE ip-po-num      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-beg-po       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-end-po       AS CHARACTER NO-UNDO.
DEFINE    VARIABLE      v-dtc          AS CHARACTER      NO-UNDO.
DEFINE    VARIABLE      v-vendor-code  AS CHARACTER      NO-UNDO.
DEFINE    VARIABLE      v-lblsize      AS CHARACTER      NO-UNDO.
DEFINE VARIABLE op-directory     AS CHARACTER     NO-UNDO.
    
EMPTY TEMP-TABLE ttpolist.


      
ASSIGN 
    /*op-directory = "\\vweb2\WebSites\DVC\Labels\" */
	op-directory = IF OPSYS = 'unix' THEN "/WebSites/DVC/Labels/"
	   ELSE "\\vweb2\WebSites\DVC\Labels1\" 
	
    v-beg-date = IF ip-all-dates THEN 1/1/2014 ELSE ip-ship-date
    v-end-date = IF ip-all-dates THEN 12/31/2099 ELSE ip-ship-date
    v-beg-po   = IF ip-all-pos OR ip-po-num = "" THEN "" ELSE ip-po-num
    v-end-po   = IF ip-all-pos OR ip-po-num = "" THEN "ZZZZZZZZZZZZZZZZZZZZZZ" ELSE ip-po-num
    .   
    
    
  
ASSIGN
    v-status       = "open,ordered"
    v-potype       = "purchase,special order"
    v-lookbackdays = 90
    v-date         = TODAY - v-lookbackdays
    v-vendor-code  = "900"
    .

RUN getDTfilestamp.

/*OUTPUT stream out1 to value("C:\workspace\TestLogs\filttpolist" + v-dtc + ".txt") unbuffered .*/

IF filter NE ? THEN
DO:
    /*    PUT STREAM out1 UNFORMATTED         "run parcefilter : " filter SKIP.*/
    RUN ParceFilter.     
END.

/*PUT STREAM out1 UNFORMATTED    'vendor-code = ' v-vendor-code SKIP.*/

 
FIND FIRST suppname NO-LOCK 
    WHERE suppname.system-id = v-sysid 
    AND suppname.supplier-code = v-vendor-code
    .

/*PUT STREAM out1 UNFORMATTED  "Suppname avail:" AVAILABLE(suppname) SKIP.*/
IF AVAILABLE(suppname) THEN
DO:
    op-directory = op-directory + trim(suppname.supplier-code) + '/'.
    RUN getpolist.
    RUN getPoFilesList.  
END.
   
/*OUTPUT stream out1 close.*/

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

PROCEDURE getPoFilesList:
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

PROCEDURE getpolist:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
         PO-types to look at are in v-potype if not in list next.
    ------------------------------------------------------------------------------*/
    ip-potype = 'purchase'. /* not needed anymore */ 
        
    DO x = 1 TO NUM-ENTRIES(v-status,','):
        ASSIGN 
            ip-status = ENTRY(X,v-status).
        
        FOR EACH po-header NO-LOCK WHERE 
            po-header.SYSTEM-ID = v-sysid AND 
            po-header.supplier-key = suppname.supplier-key AND 
            po-header.po-status = ip-status AND 
            /*po-header.po-type = ip-potype AND*/ 
            po-header.sched-delivery-date >= v-date AND 
            po-header.receipt-complete = NO AND  
            po-header.CANCELLED = NO  
            ,FIRST location NO-LOCK WHERE 
            location.system = po-header.SYSTEM-ID AND 
            location.location-key = po-header.ship-to-loc-key AND
            location.ACTIVE 
            :
                    
            IF po-header.received THEN NEXT.
            IF LOOKUP(po-header.po-type ,v-potype) = 0 THEN NEXT.
            
            CREATE ttpolist. 
            ASSIGN 
                ttpolist.PoNum     = po-header.po-num
                ttpolist.OkToPrint = YES
                ttpolist.PrintPo   = NO 
                ttpolist.ShipDate  = po-header.sched-ship-date 
                ttpolist.NumBoxes  = 0
                ttpolist.ShipTotal = po-header.merch-amt 
                ttpolist.ShipToWh  = location.location-prefix
                ttpolist.VendorKey = po-header.supplier-key 
                ttpolist.Docxfile  = v-vendor-code
                ttpolist.lblSize   = v-lblsize
                .
            
            FIND FIRST lblmst NO-LOCK 
                WHERE lblmst.company-id = po-header.SYSTEM-ID
                AND lblmst.po-num = po-header.po-num
                USE-INDEX po-num-batch NO-ERROR
                .
            ASSIGN 
                ttpolist.OkToPrint = AVAILABLE lblmst
                .
                
            ASSIGN 
                /*v-po-ship-qty = 0*/
                v-po-ord-qty = 0
                v-po-cost    = 0.
                    
            FOR EACH po-trailer NO-LOCK 
                WHERE po-trailer.system = po-header.SYSTEM-ID 
                AND po-trailer.po-num = po-header.po-num : 
                   
                ASSIGN 
                    v-po-cost    = v-po-cost + (po-trailer.qty-order * po-trailer.unit-cost)
                    v-po-ord-qty = v-po-ord-qty + po-trailer.qty-ordered.
          
            /** see if anything has been shipped **
            FIND FIRST po-trailer-ext NO-LOCK 
            WHERE po-trailer-ext.system = po-header.SYSTEM-ID  
            AND po-trailer-ext.po-num = po-header.po-num 
            AND po-trailer-ext.line-num = po-trailer.line-num  
            NO-ERROR.
                    
            IF AVAILABLE (po-trailer-ext) THEN
              ASSIGN v-po-ship-qty = v-po-ship-qty + po-trailer-ext.qty-shipped
                     v-total-ship = v-total-ship + po-trailer-ext.qty-shipped.
            *****************************/         
            END.
                      
            ASSIGN 
                ttpolist.NumBoxes = v-po-ord-qty.  
        END.
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
        THEN v-vendor-code = '900'.

END PROCEDURE.



/* end of program*/