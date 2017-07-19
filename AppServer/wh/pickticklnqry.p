
&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
"Structured Procedure File Template.

Use this template to create a new Structured Procedure file to compile and run PROGRESS 4GL code. You edit structured procedure files using the AB's Section Editor."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : PickTickLnQry.p
    Purpose     : 

    Syntax      :

    Description : Pick Ticket Line Query Program		

    Author(s)   : 
    Created     : Mon Nov 23 13:03:28 EST 2015 
    Notes       :
  ----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*BLOCK-LEVEL ON ERROR UNDO, THROW*/
USING Progress.Json.ObjectModel.*. 

{"PickTickLns.i"}

DEF TEMP-TABLE ttfilter NO-UNDO
    FIELD filtername AS CHAR FORMAT 'x(10)'
    FIELD filterbins AS CHAR FORMAT 'x(20)'
    FIELD filterseq AS INT
    INDEX idx1 IS PRIMARY filterseq
    INDEX idx2 filtername
    .
    

DEF INPUT PARAMETER filter AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER dataset FOR dsPickTickLns.

/*DEF VAR filter AS CHAR.*/
/*DEFINE dataset FOR dsPickTickLns.*/

DEF VAR v-cLblId AS CHAR FORMAT 'x(15)' NO-UNDO.
DEF VAR v-zone AS CHAR NO-UNDO.
DEF VAR v-cSystemId          AS CHAR   NO-UNDO INIT 'DV'.
DEF VAR v-cSetId AS CHAR NO-UNDO.
DEF VAR v-iscanseq AS INT NO-UNDO.
DEFINE VAR iseq AS INTEGER NO-UNDO.  /*sequence for dataset. */
DEF VAR v-cfilterOn AS CHAR NO-UNDO.


DEFINE VARIABLE iCount         AS INTEGER           NO-UNDO.
DEFINE VARIABLE ablFilter      AS CHARACTER         NO-UNDO.
DEFINE VARIABLE id             AS CHARACTER         INITIAL ? NO-UNDO.
DEFINE VARIABLE iMaxRows       AS INTEGER           INITIAL ? NO-UNDO.
DEFINE VARIABLE iSkipRows      AS INTEGER           INITIAL ? NO-UNDO.
DEFINE VARIABLE cOrderBy       AS CHARACTER         INITIAL "" NO-UNDO.

DEF STREAM out1. 
DEF VAR v-dir AS CHAR.
DEF VAR v-file AS CHAR. 


v-dir = IF OPSYS = 'unix' THEN "//WebSites//WebRest//Logs//"  
           ELSE "C://workspace//TestLogs//".
/*{icumconv.i}*/
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure Template
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

v-clblid = "SW013863360".

RUN build-filter.
RUN getset. 
v-file = "ptlnqry_" + v-cSetId + '.txt'.
OUTPUT STREAM out1 TO VALUE(v-dir + v-file) unbuffered.
 
PUT STREAM out1 UNFORMATTED 'filter=' filter SKIP .

IF TRIM(filter) <> "" THEN        
    RUN JFPFillMethod.
v-iscanseq = v-iscanseq + 1.

PUT STREAM out1 UNFORMATTED "PTNum0= " v-clblid SKIP. 
IF TRIM(ablfilter) <> "" THEN 
    RUN ParceABLFilter.
PUT STREAM out1 UNFORMATTED "PTNum1= " v-clblid SKIP
            "Zone:" v-zone SKIP . 

FIND FIRST ttfilter NO-LOCK
  WHERE ttfilter.filtername = v-zone NO-ERROR.
  
v-cfilterOn = IF AVAIL ttfilter THEN ttfilter.filterbins ELSE "" .  

PUT STREAM out1 UNFORMATTED "Run lblproc= " v-clblid SKIP  .
 
RUN lblproc (INPUT v-clblid).

/*RUN dumptable.*/
OUTPUT  stream out1 close.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-build-filter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-filter Procedure
PROCEDURE build-filter:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR vxx AS INT NO-UNDO.
    
    CREATE ttfilter.
    ASSIGN 
        ttfilter.filterseq = vxx
        vxx = vxx + 1
        ttfilter.filtername = 'All Zones'
        ttfilter.filterbins = ""
        .

    CREATE ttfilter.
    ASSIGN 
        ttfilter.filterseq = vxx
        vxx = vxx + 1
        ttfilter.filtername = 'Rose'
        ttfilter.filterbins = "O,P,Q,R,S,T"
        .
        
    CREATE ttfilter.
    ASSIGN 
        ttfilter.filterseq = vxx
        vxx = vxx + 1
        ttfilter.filtername = 'Trops'
        ttfilter.filterbins = "E,F,G"
        .    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-dumptable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumptable Procedure
PROCEDURE dumptable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
FOR EACH ttPickTickLns :
    
    DISPLAY ttPickTickLns.seq 
    ttPickTickLns.ProdCd
    ttPickTickLns.PtSeq
    .
    
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getset Procedure
PROCEDURE getset:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    v-cSetId = "Z" + string(DATETIME(TODAY, MTIME)).
    v-cSetId = REPLACE(v-cSetId,'/',"").
    v-cSetId = REPLACE(v-cSetId,'-',"").
    v-cSetId = REPLACE(v-cSetId,':',"").
    v-cSetId = REPLACE(v-cSetId,'.',"").
    v-cSetId = REPLACE(v-cSetId,' ',"").
    v-iscanseq = 0.

    EMPTY TEMP-TABLE ttPickTickLns NO-ERROR.
    /*EMPTY TEMP-TABLE ttPickTicks NO-ERROR.*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-JFPFillMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JFPFillMethod Procedure
PROCEDURE JFPFillMethod:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE jsonParser     AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE jsonObject     AS JsonObject        NO-UNDO.
    DEFINE VARIABLE cWhere         AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE hQuery         AS HANDLE            NO-UNDO.
    DEFINE VARIABLE lUseReposition AS LOGICAL           NO-UNDO.
/*    DEFINE VARIABLE iCount         AS INTEGER           NO-UNDO.           */
/*    DEFINE VARIABLE ablFilter      AS CHARACTER         NO-UNDO.           */
/*    DEFINE VARIABLE id             AS CHARACTER         INITIAL ? NO-UNDO. */
/*    DEFINE VARIABLE iMaxRows       AS INTEGER           INITIAL ? NO-UNDO. */
/*    DEFINE VARIABLE iSkipRows      AS INTEGER           INITIAL ? NO-UNDO. */
/*    DEFINE VARIABLE cOrderBy       AS CHARACTER         INITIAL "" NO-UNDO.*/
    
    
    jsonParser = NEW ObjectModelParser().
    jsonObject = CAST(jsonParser:Parse(filter), jsonObject).
    iMaxRows = jsonObject:GetInteger("top") NO-ERROR.
    iSkipRows = jsonObject:GetInteger("skip") NO-ERROR.
    ablFilter = jsonObject:GetCharacter("ablFilter") NO-ERROR.
    id = jsonObject:GetCharacter("id") NO-ERROR.
    cOrderBy = jsonObject:GetCharacter("orderBy") NO-ERROR.
    cWhere = "WHERE " + ablFilter NO-ERROR.
    IF cOrderBy > "" THEN 
    DO:
        cOrderBy = REPLACE(cOrderBy, ",", " by ").
        cOrderBy = "by " + cOrderBy + " ".
        /* NOTE: id and seq fields should be removed from
        cWhere and cOrderBy */
        cOrderBy = REPLACE(cOrderBy, "by id desc", "").
        cOrderBy = REPLACE(cOrderBy, "by id ", "").
        cOrderBy = REPLACE(cOrderBy, "by seq desc", "").
        cOrderBy = REPLACE(cOrderBy, "by seq ", "").
    END.
     /*
     BUFFER-COPY Customer TO ttCustomer.
    ASSIGN  ttCustomer.id  = STRING(ROWID(Customer))
     */
    PUT STREAM out1 UNFORMATTED
        'cwhere:' cwhere SKIP
        'corderby:' corderby SKIP
        'ablfilter:' ablfilter SKIP
        "ID:" id  
        "   imax: " imaxrows 
        "   iskip:" iskiprows SKIP 
        "Entry2: " ENTRY(2,ablFilter,'=') SKIP
    "**************" SKIP.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-LblProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LblProc Procedure
PROCEDURE LblProc:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT PARAM  ip-cPtNum AS CHAR NO-UNDO.
  
  
  DEF VAR v-cPtNum     AS CHAR     NO-UNDO.
  DEF VAR v-dtDateTime AS DATETIME NO-UNDO.
  DEF VAR v-deConvFact AS DEC      NO-UNDO.
  DEF VAR v-iQtyLbls   AS INT      NO-UNDO. 
  DEF VAR v-iRetCd     AS INT      NO-UNDO.
  DEF VAR v-line-item-name AS CHAR NO-UNDO.
  
  DEF BUFFER b1OePickTicketTrl FOR oe-pick-ticket-trl.
  DEF BUFFER b2Um              FOR um.
  DEF BUFFER b1Product         FOR product.
  DEF BUFFER b1Um              FOR um.  
  DEF BUFFER b1TtPickTickLns   FOR ttPickTickLns.
  DEF BUFFER b1oe-trailer      FOR oe-trailer.
  /*DEF BUFFER b1TtPickTicks     FOR ttPickTicks.*/
  DEF VAR v-cPickBin AS CHAR NO-UNDO.
  
  
  v-cptnum = ip-cptnum.
  v-dtDateTime = DATETIME(TODAY, MTIME).
  
  DO TRANSACTION:

    /*IF CAN-FIND (FIRST b1TtPickTicks WHERE b1TtPickTicks.PtNum = v-cPtNum) = FALSE THEN*/
    /* Process all lines for PT */
    FOR EACH b1OePickTicketTrl NO-LOCK
       WHERE b1OePickTicketTrl.system-id       = v-cSystemId
         AND b1OePickTicketTrl.pick-ticket-num = v-cPtNum
      ,FIRST b2Um NO-LOCK /* PT UM */
       WHERE b2Um.um-key = b1OePickTicketTrl.recommended-um
      ,FIRST b1oe-trailer NO-LOCK WHERE
              b1oe-trailer.system-id = b1OePickTicketTrl.system-id AND
              b1oe-trailer.order-num = b1OePickTicketTrl.order-num AND
              b1oe-trailer.entry-key = "" AND
              b1oe-trailer.line-num = b1OePickTicketTrl.order-line-num   
      ,FIRST b1Product NO-LOCK
       WHERE b1Product.system-id   = v-cSystemId
         AND b1Product.product-key = b1OePickTicketTrl.product-key
      ,FIRST b1Um NO-LOCK /* Base UM */
       WHERE b1Um.um-key = b1Product.um-key        
          BY b1OePickTicketTrl.system-id
          BY b1OePickTicketTrl.pick-ticket-num
          BY b1OePickTicketTrl.sequence
      :


        IF v-cfilterOn <> "" THEN
        DO:
           v-cPickBin = SUBSTRING(b1OePickTicketTrl.recommended-bin-name,4,1).
          /* MESSAGE 'on ' ttPickTickLns.PickBin v-cPickBin  LOOKUP(v-cPickBin,v-cfilterOn) VIEW-AS ALERT-BOX. */
           IF LOOKUP(v-cPickBin,v-cfilterOn) = 0 THEN NEXT.
        END.

        ASSIGN
        v-deConvFact = 1
        v-iQtyLbls   = 0.
                     
      IF b1Um.um-key <> b2Um.um-key THEN
        RUN dvicumconv.p
            (INPUT b1Um.um-key
            ,INPUT b2Um.um-key
            ,INPUT NO
            ,INPUT 1
            ,INPUT ''
            ,INPUT ''
            ,INPUT ''
            ,OUTPUT v-iRetCd
            ,OUTPUT v-deConvFact
            ).
            
            
            
      ASSIGN v-line-item-name = IF b1oe-trailer.line-item-name NE ""
                                  THEN b1oe-trailer.line-item-name
                                  ELSE b1Product.product-name.      

      FIND FIRST b1TtPickTickLns EXCLUSIVE-LOCK 
           WHERE b1TtPickTickLns.PtNum = b1OePickTicketTrl.pick-ticket-num
             AND b1TtPickTickLns.PtSeq = b1OePickTicketTrl.sequence
             NO-ERROR.

      IF NOT AVAIL b1TtPickTickLns THEN
      DO:

        CREATE b1TtPickTickLns.
        ASSIGN
        b1TtPickTickLns.seq  = iseq 
        iseq = iseq + 1
            b1TtPickTickLns.SetId= v-cSetId
            b1TtPickTickLns.scanseq      = v-iscanseq
            b1TtPickTickLns.UpdtDateTime = v-dtDateTime
            b1TtPickTickLns.PickBin      = b1OePickTicketTrl.recommended-bin-name
            b1TtPickTickLns.PickBinSeq   =  b1OePickTicketTrl.recommended-bin-seq
            b1TtPickTickLns.ProdCd       = b1Product.product-code
            b1TtPickTickLns.ProdDesc     = v-line-item-name
            b1TtPickTickLns.PtSeq        = b1OePickTicketTrl.sequence
            b1TtPickTickLns.PtNum        = b1OePickTicketTrl.pick-ticket-num
            b1TtPickTickLns.QtyLbls      = v-iQtyLbls
            b1TtPickTickLns.QtyPtLnPcs   = b1OePickTicketTrl.recommended-qty * v-deConvFact
            b1TtPickTickLns.QtyPicked    = 0
            b1TtPickTickLns.QtyToPick    = b1TtPickTickLns.QtyPtLnPcs
            b1TtPickTickLns.UmCd         = b2Um.um-code
            .
      END. /* IF NOT AVAIL b1TtPickTickLns.. */

      /******************************************
      FIND FIRST b1TtPickTicks
           WHERE b1TtPickTicks.PtNum = v-cPtNum
           EXCLUSIVE-LOCK NO-ERROR.

      IF NOT AVAIL b1TtPickTicks THEN
      DO:

        CREATE b1TtPickTicks.
        ASSIGN
          b1TtPickTicks.CustCd         = v-cCustCd
          b1TtPickTicks.CreateDateTime = v-dtDateTime
          b1TtPickTicks.PtNum          = v-cPtNum
          b1TtPickTicks.QtyPicked      = 0.
          
      END. /* IF NOT AVAIL b1TtPickTicks.. */

      ASSIGN
        b1TtPickTicks.QtyLbls   = b1TtPickTicks.QtyLbls   + b1TtPickTickLns.QtyLbls
        b1TtPickTicks.QtyToPick = b1TtPickTicks.QtyToPick + b1TtPickTickLns.QtyToPick
        b1TtPickTicks.QtyPtPcs  = b1TtPickTicks.QtyPtPcs  + b1TtPickTickLns.QtyPtLnPcs.
       ******************************************/
       
    END. /* FOR EACH b1OePickTicketTrl.. */

  END. /* TRANSACTION */
 
  FIND CURRENT b1TtPickTickLns NO-LOCK NO-ERROR.
  
  RELEASE b1TtPickTickLns   NO-ERROR.
  RELEASE b1OePickTicketTrl NO-ERROR.
  RELEASE b1Product         NO-ERROR.
  RELEASE b1Um              NO-ERROR.
  RELEASE b2Um              NO-ERROR.

  /*FIND CURRENT b1TtPickTicks   NO-LOCK NO-ERROR.*/
  /*RELEASE b1TtPickTicks     NO-ERROR.*/

  RETURN.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF



&IF DEFINED(EXCLUDE-ParceABLFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParceABLFilter Procedure
PROCEDURE ParceABLFilter:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/


    /*
    DEF VAR ablfilter AS CHAR NO-UNDO.
    ablfilter = "(PTNum = 'SW013863360' and Zone = 'All Zones')".
    */

    DEF VAR varray  AS CHAR EXTENT 20. 
    DEF VAR X       AS INT  NO-UNDO.
    DEF VAR X_Max   AS INT  NO-UNDO.
    DEF VAR v-field AS CHAR NO-UNDO.
    DEF VAR v-value AS CHAR NO-UNDO.

    IF ablfilter BEGINS '(' THEN
    DO :
        ablfilter = TRIM(ablfilter,'(').
        ablfilter = TRIM(ablfilter,')').
        ablfilter = REPLACE(ablfilter, " and ", "|").
        X_Max = NUM-ENTRIES(ablfilter,"|").
        DO X = 1 TO NUM-ENTRIES(ablfilter,"|"):
            varray[X] = REPLACE(TRIM(ENTRY(X,ablfilter,'|')),"'","").
            v-field = TRIM(ENTRY(1,varray[X],"=")).
            v-value = TRIM(ENTRY(2,varray[X],"=")).
            CASE v-field:
                WHEN 'PTnum' THEN 
                    DO:
                        v-clblid = v-value.
                    END.
                WHEN "Zone" THEN 
                    DO:
                        v-zone = v-value.
                    END.
            END CASE.
        END.

/*        MESSAGE ablfilter x_max*/
/*            SKIP varray[1]     */
/*            SKIP varray[2]     */
/*            SKIP varray[3]     */
/*            VIEW-AS ALERT-BOX. */


    END.
    ELSE
    DO:
        v-clblid = TRIM(ENTRY(2,ablFilter,'=')).
        v-clblid = TRIM(v-clblid,"'").

    END.



END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

