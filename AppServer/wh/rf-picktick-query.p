/* $Header: rf-picktick-query.p,v 1.1 2015/10/28 10:30:42 jleander Exp $ */
/* Revision Management Header above, please DO NOT DELETE these two lines. */
/*****************************************************************\
******************************************************************
**      Program: rf-picktick-query.p
**  Description: Scan PT's get summary of PT lines for the group of
                 PT's scanned.in BinSeq/BinID/Prod/ScanSeq ID.
**       Author: JMarkhorst
**         Date: 9/10/2015
******************************************************************
* Notes:                                                         *
        Brandon wants to add a filter function to filter groups
        of bins ie rose bins, local bins, etc. 
        This will be start of RF Loose pick function and Proconna 
        tracking. 
******************************************************************
* Updates:
*   2015.10.27 jl  Added 'U,V' filter.
\*****************************************************************/


/*****  SCOPED-DEFINE SECTION  *****/
&SCOPED-DEFINE ERR-BELL-CNT  4       /* Num of times to ring bell on warnings */
&SCOPED-DEFINE WARN-BELL-CNT 2         /* Num of times to ring bell on errors */
&SCOPED-DEFINE DISP-HEIGHT  11
&SCOPED-DEFINE DISP-WIDTH   21
&SCOPED-DEFINE FRAME-WIDTH  30

DEF STREAM out1.
/*{icumconv.i}*/

DEF VAR v-cSystemId          AS CHAR   NO-UNDO INIT 'DV'.
DEF VAR v-cLblId AS CHAR FORMAT 'x(15)' NO-UNDO.
DEF VAR v-cfile AS CHAR NO-UNDO.
DEF VAR v-cdir AS CHAR NO-UNDO /*INIT 'c:\temp\'*/.
DEF VAR v-RC AS CHAR NO-UNDO.
DEF VAR v-iscanseq AS INT NO-UNDO.
DEF VAR v-dumphdr AS CHAR NO-UNDO.
DEF VAR v-cSetId AS CHAR NO-UNDO.
DEF VAR v-cfilterOn AS CHAR NO-UNDO.
DEF VAR v-cfilterlistname AS CHAR NO-UNDO.
DEF VAR v-cfilterlistbin AS CHAR NO-UNDO.

DEFINE TEMP-TABLE tt-select NO-UNDO
  FIELD disp-value AS CHAR
  FIELD disp-order AS INT
  FIELD ret-value  AS CHAR
  INDEX idx1 IS PRIMARY disp-order
  INDEX idx2 disp-value.

DEF TEMP-TABLE ttPickTickLns NO-UNDO
  FIELD SetId       AS CHAR 
  FIELD Scanseq      AS INT
  FIELD PickBin      AS CHAR
  FIELD ProdCd       AS CHAR
  FIELD QtyToPick    AS INT
  FIELD UmCd         AS CHAR
  FIELD PtNum        AS CHAR
  FIELD PtSeq        AS INT
  FIELD QtyPtLnPcs   AS INT
  FIELD QtyPicked    AS INT
  FIELD QtyLbls      AS INT
  FIELD UpdtDateTime AS DATETIME
  FIELD PickBinSeq   AS INT
  INDEX idx1 IS PRIMARY PtNum PtSeq
  INDEX idx2 PtNum QtyToPick DESC UpdtDateTime DESC PtSeq
  INDEX bybinprod pickbinseq pickbin prodcd scanseq  
    .
v-dumphdr = "SetId,OrderSeq,Bin,Prod,Qpick,UM,PT#,PTLn#,QPtLnPcs,QPcked,QLabels,DateTime,BinSeq".


DEF TEMP-TABLE ttPickTicks NO-UNDO
  FIELD CustCd         AS CHAR
  FIELD PtNum          AS CHAR
  FIELD QtyLbls        AS INT
  FIELD QtyPicked      AS INT
  FIELD QtyPtPcs       AS INT
  FIELD QtyToPick      AS INT
  FIELD CreateDateTime AS DATETIME
  INDEX idx1 IS PRIMARY PtNum
  INDEX idx2 QtyToPick DESC CreateDateTime DESC PtNum.

DEF TEMP-TABLE ttfilter NO-UNDO
    FIELD filtername AS CHAR FORMAT 'x(10)'
    FIELD filterbins AS CHAR FORMAT 'x(20)'
    FIELD filterseq AS INT
    INDEX idx1 IS PRIMARY filterseq
    .

DEFINE VARIABLE v-cRetVal   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-cTitle              AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-cDispLine           AS CHARACTER NO-UNDO EXTENT 4.
DEFINE VARIABLE v-cContKey            AS CHARACTER NO-UNDO EXTENT 3.
DEFINE VARIABLE v-cContKeyLbl         AS CHARACTER NO-UNDO EXTENT 3.

FORM
  v-cTitle         AT ROW 1 COL  1 NO-LABEL FORMAT 'X({&DISP-WIDTH})'
  v-cContKeyLbl[1] AT ROW 2 COL  1
  v-cContKey[1]    AT ROW 2 COL  9 NO-LABEL FORMAT 'X(11)'
  v-cContKeyLbl[2] AT ROW 3 COL  1
  v-cContKey[2]    AT ROW 3 COL  9 NO-LABEL FORMAT 'X(11)'
  v-cContKeyLbl[3] AT ROW 4 COL  1
  v-cContKey[3]    AT ROW 4 COL  9 NO-LABEL FORMAT 'X(10)'
  v-cDispLine[1]   AT ROW 5 COL  1 NO-LABEL FORMAT 'X({&DISP-WIDTH})'
  v-cDispLine[2]   AT ROW 6 COL  1 NO-LABEL FORMAT 'X({&DISP-WIDTH})'
  v-cDispLine[3]   AT ROW 7 COL  1 NO-LABEL FORMAT 'X({&DISP-WIDTH})'
  v-cDispLine[4]   AT ROW 8 COL  1 NO-LABEL FORMAT 'X({&DISP-WIDTH})'
  WITH FRAME f-RfPick SIZE-CHARS {&FRAME-WIDTH} BY 8
  ATTR-SPACE OVERLAY NO-LABELS NO-BOX COL 1 ROW 1.

RUN build-filter.


/*ASSIGN v-clblid = "SW013473788".*/
ASSIGN 
    v-cfilterOn = ""    
    v-cTitle = "Enter PT# to List"
    v-cContKeyLbl[1] = 'PT#'
    v-cContKeyLbl[2] = 'Lst PT#'
    v-cContKeyLbl[3] = "Count"
    v-cDispLine[2] = "Filter: " + v-cfilteron
    v-cDispLine[4] = /*2015.10.27.jl "F3-Filter On/Off"*/ "F3-Filter Toggle"
    .

REPEAT WITH FRAME f-rfpick.
        VIEW FRAME f-RfPick.
        
        RUN buildset.
        /*RUN dumpit. */
        IF v-clblid = "0" THEN LEAVE. 

        RUN dispit.
        ASSIGN 
            /*v-cContKeyLbl = ""*/
            v-cContKey = ''.

        DISPLAY v-ctitle v-cContKeyLbl v-cContKey.
        VIEW FRAME f-RfPick.
END.

PROCEDURE build-filter:
    DEF VAR vxx AS INT NO-UNDO.
    
    CREATE ttfilter.
    ASSIGN 
        ttfilter.filterseq = vxx
        vxx = vxx + 1
        ttfilter.filtername = 'None'
        ttfilter.filterbins = ""
        .

    CREATE ttfilter.
    ASSIGN 
        ttfilter.filterseq = vxx
        vxx = vxx + 1
        ttfilter.filtername = 'Rose'
        ttfilter.filterbins = "O,P,Q,R,S,T"
        .

END PROCEDURE. 

PROCEDURE BldTtSelecfilterst:

  DEFINE VARIABLE v-cCustNum AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-cPickBin AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-cProdCd  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-cUmCd    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-iDispOrd AS INTEGER   NO-UNDO.
  DEF VAR v-color AS CHAR NO-UNDO.

  EMPTY TEMP-TABLE tt-select NO-ERROR.

  CREATE tt-select.
  ASSIGN
    tt-select.disp-value = 'Filter|Bins' /*Column labels*/
    tt-select.disp-order = 0
    tt-select.ret-value  = ''.

    FOR EACH  ttfilter NO-LOCK 
        USE-INDEX idx1
        :

        CREATE tt-select.
        ASSIGN
          v-iDispOrd           = v-iDispOrd + 1
          tt-select.disp-value = ttfilter.filtername + '|' +
                                 ttfilter.filterbins 
          tt-select.disp-order = v-iDispOrd
          tt-select.ret-value  = ttfilter.filterbins /*STRING(v-iDispOrd)*/
          .
    END.
  RETURN.
  
END PROCEDURE. /* BldTtSelecfilterst */


PROCEDURE dispit:
    RUN BldTtSelect.
    RUN LstPTSet.
END PROCEDURE. 


PROCEDURE BldTtSelect:

  DEFINE VARIABLE v-cCustNum AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-cPickBin AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-cProdCd  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-cUmCd    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-iDispOrd AS INTEGER   NO-UNDO.
  DEF VAR v-color AS CHAR NO-UNDO.

  DEFINE BUFFER b-rf-cont-attrib      FOR rf-cont-attrib.
  DEFINE BUFFER b-rf-cont-attrib-type FOR rf-cont-attrib-type.
  DEFINE BUFFER b-rf-cont-trl         FOR rf-cont-trl.

  EMPTY TEMP-TABLE tt-select NO-ERROR.

  CREATE tt-select.
  ASSIGN
    tt-select.disp-value = 'PickBin|Prod|Qty|UM|Color' /*Column labels*/
    tt-select.disp-order = 0
    tt-select.ret-value  = ''.

    FOR EACH ttPickTickLns NO-LOCK 
        USE-INDEX bybinprod
        :

        IF v-cfilterOn <> "" THEN
        DO:
           v-cPickBin = SUBSTRING(ttPickTickLns.PickBin,4,1).
          /* MESSAGE 'on ' ttPickTickLns.PickBin v-cPickBin  LOOKUP(v-cPickBin,v-cfilterOn) VIEW-AS ALERT-BOX. */
           IF LOOKUP(v-cPickBin,v-cfilterOn) = 0 THEN NEXT.
        END.

        FIND FIRST product NO-LOCK
            WHERE product.SYSTEM-ID = 'dv'
            AND product.product-code = ttPickTickLns.ProdCd
            NO-ERROR.

        v-color = "".
        IF AVAIL product THEN v-color = product.usr-def-fld-3. 

        CREATE tt-select.
        ASSIGN
          v-iDispOrd           = v-iDispOrd + 1
          tt-select.disp-value = ttPickTickLns.PickBin + '|' +
                                 ttPickTickLns.ProdCd + '|' +
                                 string(ttPickTickLns.QtyToPick,">>9") + '|' +
                                 ttPickTickLns.UmCd
                                 + '|' + v-color
          tt-select.disp-order = v-iDispOrd
          tt-select.ret-value  = STRING(v-iDispOrd).
    END.
  RETURN.
END PROCEDURE. /* BldTtSelect */

PROCEDURE LstPTSet:

  /*DEFINE VARIABLE v-cRetVal   AS CHARACTER NO-UNDO.*/
  DEFINE VARIABLE v-cMssg1    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-cMssg2    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-cStatusLn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-hTmpTable AS HANDLE    NO-UNDO.
  DEFINE VARIABLE v-lSort     AS LOGICAL   NO-UNDO.

  /*RUN BldTtSelect.*/

  ASSIGN
    v-cRetVal     = '1'  /*Seed to reposition list*/
    v-cMssg1      = 'UP/DOWN/LEFT/RIGHT'
    v-cMssg2      = 'ENTER to leave'
    /*v-cRunProg[1] = v-cRunProg[2]*/
    v-cStatusLn   = 'Review PT contents'
    v-lSort       = FALSE.
    
  IF CAN-FIND(FIRST tt-select NO-LOCK) = NO THEN RETURN.

  ASSIGN v-hTmpTable = TEMP-TABLE tt-select:HANDLE NO-ERROR.

  IF VALID-HANDLE(v-hTmpTable) = NO THEN RETURN.

  RUN rf-format-disp.p
      (INPUT '|'
      ,INPUT v-hTmpTable
      ,INPUT v-lSort
      ).

  RUN rf-sel-list.p
      (INPUT v-cMssg1
      ,INPUT v-cMssg2
      ,INPUT v-cStatusLn
      ,INPUT v-hTmpTable
      ,INPUT-OUTPUT v-cRetVal
      ).

  RETURN.

END PROCEDURE. /* PROCEDURE LstSkd */

PROCEDURE buildset:
/*jfm*/
    ON 'F3':U OF v-cContKey[1] IN FRAME f-rfpick
    OR 'PF3':U OF v-cContKey[1] IN FRAME f-rfpick
    DO:
        /*v-cfilterOn = NOT v-cfilterOn.*/
        /*RUN BldTtSelecfilterst.
        RUN LstPTSet.*/
/*2015.10.27.jl> 
        ASSIGN 
        v-cfilterOn = IF v-cfilteron = '' THEN "O,P,Q,R,S,T" ELSE ""
*/
      CASE v-cfilterOn:
        WHEN '' THEN ASSIGN v-cfilterOn = 'O,P,Q,R,S,T'.
        WHEN 'O,P,Q,R,S,T' THEN ASSIGN v-cfilterOn = 'U,V'.
        OTHERWISE ASSIGN v-cfilterOn = ''.
      END CASE.

      ASSIGN
/*<2015.10.27.jl<*/
        v-cDispLine[2] = "Filter: " + v-cfilteron
        v-cDispLine[2]:SCREEN-VALUE = "Filter: " + v-cfilteron.
        /*MESSAGE "Filter:" v-cfilteron  /*v-cRetVal*/ VIEW-AS ALERT-BOX.*/
        RETURN NO-APPLY . 
    END. 

    
    RUN getset.
    REPEAT WITH FRAME f-rfpick:
    

        DISPLAY v-ctitle v-cContKeyLbl v-cContKey v-cDispLine.
        UPDATE v-cContKey[1] .  
        ASSIGN 
            v-clblid = v-cContKey[1]
            v-cContKey[2] = v-cContKey[1]
             .
        IF TRIM(v-clblid) = ""  THEN LEAVE.
        IF v-clblid = "0"  THEN LEAVE.

        IF NOT CAN-FIND(FIRST ttPickTicks WHERE ttPickTicks.PtNum  = v-clblid) THEN 
        DO:
            v-iscanseq = v-iscanseq + 1.
            RUN lblproc (INPUT v-clblid, OUTPUT v-rc).
        END.
        ASSIGN 
            v-cContKey[3] = STRING(v-iscanseq,">>9")
            v-cContKey[1] = "".
    END.

END PROCEDURE. 

PROCEDURE getset:
    v-cSetId = "Z" + string(DATETIME(TODAY, MTIME)).
    v-cSetId = REPLACE(v-cSetId,'/',"").
    v-cSetId = REPLACE(v-cSetId,'-',"").
    v-cSetId = REPLACE(v-cSetId,':',"").
    v-cSetId = REPLACE(v-cSetId,'.',"").
    v-cSetId = REPLACE(v-cSetId,' ',"").
    v-iscanseq = 0.

    EMPTY TEMP-TABLE ttPickTickLns NO-ERROR.
    EMPTY TEMP-TABLE ttPickTicks NO-ERROR.

    
END PROCEDURE.

PROCEDURE dumpit: 

    v-cfile = 'PTDump.txt'.
    OUTPUT STREAM out1 TO VALUE(v-cdir + v-cfile).
    PUT STREAM out1 UNFORMATTED v-dumphdr SKIP. 

    FOR EACH ttPickTickLns NO-LOCK 
        USE-INDEX bybinprod
        :
        EXPORT STREAM out1 DELIMITER ','
           ttPickTickLns.
    END.
    OUTPUT STREAM out1 CLOSE.

END PROCEDURE. 

PROCEDURE LblProc:

  DEF INPUT PARAM  ip-cPtNum AS CHAR NO-UNDO.
  DEF OUTPUT PARAM op-cRetCd AS CHAR NO-UNDO.

  DEF VAR v-cCustCd    AS CHAR     NO-UNDO.
  DEF VAR v-cPtNum     AS CHAR     NO-UNDO.
  DEF VAR v-dtDateTime AS DATETIME NO-UNDO.
  DEF VAR v-deConvFact AS DEC      NO-UNDO.
  DEF VAR v-iQtyLbls   AS INT      NO-UNDO.
  DEF VAR v-iPtSeq     AS INT      NO-UNDO.
  DEF VAR v-iRetCd     AS INT      NO-UNDO.

  DEF BUFFER b1OePickTicketTrl FOR oe-pick-ticket-trl.
  DEF BUFFER b1Product         FOR product.
  DEF BUFFER b1RfContAttrib    FOR rf-cont-attrib.
  DEF BUFFER b1RfContHdr       FOR rf-cont-hdr.
  
  DEF BUFFER b1TtPickTickLns   FOR ttPickTickLns.
  DEF BUFFER b1TtPickTicks     FOR ttPickTicks.
  DEF BUFFER b1Um              FOR um.
  DEF BUFFER b2Um              FOR um.

  /*
  RUN LblLkup
      (INPUT  ip-cLblId
      ,OUTPUT v-cCustCd
      ,OUTPUT v-cPtNum
      ,OUTPUT v-iPtSeq
      ,OUTPUT op-cRetCd
      ).
  */
  v-cptnum = ip-cptnum.
  v-dtDateTime = DATETIME(TODAY, MTIME).

  

  IF op-cRetCd > '' THEN RETURN.

  DO TRANSACTION:

    IF CAN-FIND (FIRST b1TtPickTicks WHERE b1TtPickTicks.PtNum = v-cPtNum) = FALSE THEN
    /* Process all lines for PT */
    FOR EACH b1OePickTicketTrl NO-LOCK
       WHERE b1OePickTicketTrl.system-id       = v-cSystemId
         AND b1OePickTicketTrl.pick-ticket-num = v-cPtNum

      ,FIRST b2Um NO-LOCK /* PT UM */
       WHERE b2Um.um-key = b1OePickTicketTrl.recommended-um
  
      ,FIRST b1Product NO-LOCK
       WHERE b1Product.system-id   = v-cSystemId
         AND b1Product.product-key = b1OePickTicketTrl.product-key

      ,FIRST b1Um NO-LOCK /* Base UM */
       WHERE b1Um.um-key = b1Product.um-key
        
          BY b1OePickTicketTrl.system-id
          BY b1OePickTicketTrl.pick-ticket-num
          BY b1OePickTicketTrl.sequence:

      ASSIGN
        v-deConvFact = 1
        v-iQtyLbls   = 0.
                     
      IF b1Um.um-key <> b2Um.um-key THEN

        RUN icumconv
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

      FIND FIRST b1TtPickTickLns
           WHERE b1TtPickTickLns.PtNum = b1OePickTicketTrl.pick-ticket-num
             AND b1TtPickTickLns.PtSeq = b1OePickTicketTrl.sequence
             EXCLUSIVE-LOCK NO-ERROR.

      IF NOT AVAIL b1TtPickTickLns THEN
      DO:
/*
        /* Process shipping labels for PT line */
        FOR EACH b1RfContAttrib NO-LOCK
           WHERE b1RfContAttrib.system-id            = v-cSystemId
             AND b1RfContAttrib.cont-attrib-type-key = v-cPtLnNumAttrTypKey
             AND b1RfContAttrib.attrib-value         = b1OePickTicketTrl.pick-ticket-num + '-' +
                                                       STRING(b1OePickTicketTrl.sequence, '9999')
             AND b1RfContAttrib.active               = TRUE

          ,FIRST b1RfContHdr NO-LOCK
           WHERE b1RfContHdr.system-id     = v-cSystemId
             AND b1RfContHdr.active        = TRUE
             AND b1RfContHdr.cont-type-key = v-cPkgContTypKey
             AND b1RfContHdr.cont-hdr-key  = b1RfContAttrib.cont-hdr-key:

          ASSIGN v-iQtyLbls = v-iQtyLbls + 1.

          IF CAN-FIND
             (FIRST ttSkdPkgLbls
              WHERE ttSkdPkgLbls.PtNum = b1OePickTicketTrl.pick-ticket-num
                AND ttSkdPkgLbls.PtSeq = b1OePickTicketTrl.sequence
                AND ttSkdPkgLbls.LblId = b1RfContHdr.cont-hdr-key
                NO-LOCK
             ) = FALSE
          THEN
          DO:

            CREATE b1ttSkdPkgLbls.
            ASSIGN
              b1ttSkdPkgLbls.LblId   = b1RfContHdr.cont-hdr-key
              b1ttSkdPkgLbls.PtSeq   = b1OePickTicketTrl.sequence
              b1ttSkdPkgLbls.PtNum   = b1OePickTicketTrl.pick-ticket-num
              b1ttSkdPkgLbls.Scanned = 0.

          END. /* IF CAN-FIND(FIRST ttSkdPkgLbls.. */

        END. /* FOR EACH b1RfContAttrib.. */
*/

        CREATE b1TtPickTickLns.
        ASSIGN
            b1TtPickTickLns.SetId= v-cSetId
            b1TtPickTickLns.scanseq      = v-iscanseq
            b1TtPickTickLns.UpdtDateTime = v-dtDateTime
            b1TtPickTickLns.PickBin      = b1OePickTicketTrl.recommended-bin-name
            b1TtPickTickLns.PickBinSeq   =  b1OePickTicketTrl.recommended-bin-seq
            b1TtPickTickLns.ProdCd       = b1Product.product-code
            b1TtPickTickLns.PtSeq        = b1OePickTicketTrl.sequence
            b1TtPickTickLns.PtNum        = b1OePickTicketTrl.pick-ticket-num
            b1TtPickTickLns.QtyLbls      = v-iQtyLbls
            b1TtPickTickLns.QtyPtLnPcs   = b1OePickTicketTrl.recommended-qty * v-deConvFact
            b1TtPickTickLns.QtyPicked    = 0
            b1TtPickTickLns.QtyToPick    = b1TtPickTickLns.QtyPtLnPcs
            b1TtPickTickLns.UmCd         = b2Um.um-code
            .
      END. /* IF NOT AVAIL b1TtPickTickLns.. */

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

    END. /* FOR EACH b1OePickTicketTrl.. */
  
    /* Tag label as scanned and increment pick counters */
    /*
    FOR FIRST b1TtPickTicks EXCLUSIVE-LOCK
        WHERE b1TtPickTicks.PtNum = v-cPtNum

       ,FIRST b1TtPickTickLns EXCLUSIVE-LOCK
        WHERE b1TtPickTickLns.PtNum = b1TtPickTicks.PtNum  
          AND b1TtPickTickLns.PtSeq = v-iPtSeq

       ,FIRST b1ttSkdPkgLbls EXCLUSIVE-LOCK
        WHERE b1ttSkdPkgLbls.PtNum   = b1TtPickTickLns.PtNum  
          AND b1ttSkdPkgLbls.PtSeq   = b1TtPickTickLns.PtSeq              
          AND b1ttSkdPkgLbls.LblId   = ip-cLblId
          AND b1ttSkdPkgLbls.Scanned = 0:
  
      ASSIGN
        b1ttSkdPkgLbls.Scanned             = 1
        b1TtPickTickLns.QtyPicked    = b1TtPickTickLns.QtyPicked + 1
        b1TtPickTickLns.QtyToPick    = MAX(0, b1TtPickTickLns.QtyToPick - 1)
        b1TtPickTickLns.UpdtDateTime = v-dtDateTime
        b1TtPickTicks.QtyPicked      = b1TtPickTicks.QtyPicked + 1
        b1TtPickTicks.QtyToPick      = MAX(0, b1TtPickTicks.QtyToPick - 1)
        v-cLkupPtNum                 = b1TtPickTicks.PtNum.
  
    END. /* FOR FIRST b1TtPickTickLns.. */
    */

  END. /* TRANSACTION */

  /*FIND CURRENT b1ttSkdPkgLbls  NO-LOCK NO-ERROR.*/
  FIND CURRENT b1TtPickTickLns NO-LOCK NO-ERROR.
  FIND CURRENT b1TtPickTicks   NO-LOCK NO-ERROR.  

  RELEASE b1OePickTicketTrl NO-ERROR.
  RELEASE b1Product         NO-ERROR.
  RELEASE b1RfContAttrib    NO-ERROR.
  RELEASE b1RfContHdr       NO-ERROR.
  /*RELEASE b1ttSkdPkgLbls    NO-ERROR.*/
  RELEASE b1TtPickTickLns   NO-ERROR.
  RELEASE b1TtPickTicks     NO-ERROR.
  RELEASE b1Um              NO-ERROR.
  RELEASE b2Um              NO-ERROR.

  RETURN.

END PROCEDURE. /* LblProc */
