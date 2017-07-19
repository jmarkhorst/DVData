/* $Header: rflblinfo.p,v 1.6 2013/08/16 16:39:51 jleander Exp $ */
/* Revision Management Header above, please DO NOT DELETE these two lines. */
/*****************************************************************\
******************************************************************
**      Program: rflblinfo.p
**  Description: Lable Info and Inv Matrix for Product
**       Author: JFM
**         Date: 6/3/03
******************************************************************
* Notes: 
*   Program is used to deduct quantities from original scan
* in case a box is moved after scanned. 
*                                                         
******************************************************************
* Updates:
* 2012.06.20  JL Changed age-code to Julian day of the year.
* 2013.08.15  JL Mod to scr-label-id for new display format.
\*****************************************************************/
&SCOPED-DEFINE s-delim ^
&SCOPED-DEFINE s-ldbname "apprise"
DEF BUFFER p-um FOR um.
DEF BUFFER b-bin FOR bin.

DEF VAR v-company-id AS CHAR INIT 'dv' NO-UNDO.
DEF VAR scr-bin-id LIKE lblmst.bin-id FORMAT 'x(10)' NO-UNDO .
DEF VAR scr-bin LIKE lblmst.bin-id NO-UNDO.
/*2013.08.15.jl>DEF VAR scr-label-id  LIKE lblmst.label-id NO-UNDO.*/
DEF VAR scr-label-id  AS DECIMAL FORMAT ">>>>>>>>>9.9999" NO-UNDO. /*2013.08.15.jl*/
DEF VAR v-date-text AS CHAR FORMAT 'x(18)' NO-UNDO.
DEF VAR scr-prod AS CHAR FORMAT "x(18)" NO-UNDO.
DEF VAR scr-info1 AS CHAR FORMAT 'x(18)' NO-UNDO.
DEF VAR scr-info2 AS CHAR FORMAT 'x(18)' NO-UNDO.
DEF VAR v-p-choice AS LOG FORMAT 'Y/N' NO-UNDO.
def var v-menu-list as CHAR NO-UNDO.
DEF VAR icmod-hndl AS HANDLE NO-UNDO.
DEF VAR v-m-choice AS CHAR NO-UNDO.
RUN icmod.p PERSISTENT SET icmod-hndl.

DEF TEMP-TABLE tt-prod-um-choices NO-UNDO
 FIELD mseq AS INT FORMAT '9'
 FIELD um-code AS CHAR FORMAT 'x(6)'
 FIELD um-key AS CHAR 
 FIELD qty-avail AS DEC
 FIELD bin-id AS CHAR
 INDEX idx1 IS PRIMARY um-code
 INDEX idx2 mseq.


FORM 
    scr-label-id LABEL 'Scn Id' AT 1
    scr-prod NO-LABEL AT 1 
    scr-info1 NO-LABEL AT 1
    scr-info2 NO-LABEL AT 1
    /*v-date-text NO-LABEL AT 1*/
    SKIP(3)
    WITH WIDTH 23 SIDE-LABELS NO-BOX NO-ATTR-SPACE ROW 1 COL 1 FRAME scan-frame OVERLAY.
PAUSE 0 BEFORE-HIDE.

REPEAT :
    scr-label-id = 0.
    /*scr-label-id = 179.7973.*/
    UPDATE scr-label-id WITH FRAME scan-frame.
    IF scr-label-id = 0 THEN LEAVE.

    RUN ip-find-label-info(INPUT scr-label-id, INPUT v-company-id).
    v-p-choice = YES.
    MESSAGE "Right Prod" UPDATE v-p-choice FORMAT 'Y/N'.
    IF NOT v-p-choice THEN NEXT.
    RUN ip-menu(INPUT v-menu-list, OUTPUT v-m-choice).

END.

PROCEDURE ip-menu:
DEF INPUT PARAMETER i-list AS CHAR.
DEF OUTPUT PARAMETER o-choice AS CHAR.
DEF VAR menu-items AS CHARACTER VIEW-AS SELECTION-LIST INNER-CHARS 15 INNER-LINES 3 SORT.

    FORM 
        "  Enter to Return" AT 1
        menu-items AT 1
      WITH WIDTH 20 no-LABELS NO-BOX NO-ATTR-SPACE ROW 1 COL 1 frame f-menu OVERLAY. 

    ON 'return':U OF menu-items IN FRAME f-menu
    DO:
        APPLY 'go' TO menu-items IN FRAME f-menu.
        RETURN NO-APPLY.
    END.

   menu-items:LIST-ITEMS IN FRAME f-menu = i-list.
   UPDATE menu-items WITH FRAME f-menu.
   IF menu-items = "" THEN menu-items = ENTRY(1,i-list).

   o-choice = menu-items.
   IF INDEX(o-choice,"|") <> 0 THEN
       o-choice = SUBSTRING(o-choice,1,INDEX(o-choice,"|") - 1).

   HIDE FRAME f-manu NO-PAUSE.

END PROCEDURE.


PROCEDURE ip-find-label-info:
    DEF INPUT PARAMETER i-label-id LIKE lblmst.label-id.
    DEF INPUT PARAMETER i-company AS CHAR.
    DEF VAR v-date-text AS CHAR.
    DEF VAR X AS INT.
    DEF VAR v-qoh AS DEC NO-UNDO.
    DEF VAR v-qoh1 AS DEC NO-UNDO.
    DEF VAR rc AS LOGICAL NO-UNDO.
    DEF VAR v-bin-loc LIKE bin.location-key NO-UNDO.

    FIND FIRST lblmst NO-LOCK
        WHERE lblmst.company-id = i-company
        AND lblmst.label-id = i-label-id
        NO-ERROR.

    IF NOT AVAIL lblmst THEN
    DO:
        DISPLAY 'Bad scan' @ scr-prod WITH FRAME scan-frame.
        RETURN.
    END.

    FIND FIRST bin NO-LOCK 
        WHERE bin.SYSTEM-ID = lblmst.company-id
        AND bin.bin-name = lblmst.bin-id
        NO-ERROR.

    v-bin-loc = IF AVAIL bin THEN bin.location-key ELSE '00000006'. /*sw*/

    FIND FIRST um NO-LOCK
        WHERE um.um-key = lblmst.um-key
        NO-ERROR.
/*>2012.06.20-JL>
    RUN rcvdtcde.p (INPUT 'encode',
                    INPUT STRING(age-date,'99/99/99'),
                    OUTPUT v-date-text).
  */
    ASSIGN
      /* DAY OF YEAR */
      v-date-text = STRING(age-date - DATE('12/31/' +
                           STRING(YEAR(age-date) - 1, '9999'))
                          ).
/*<2012.06.20-JL<*/

    FIND FIRST product NO-LOCK 
        WHERE product.SYSTEM-ID = v-company-id
        AND product.product-code = lblmst.prod-code
        NO-ERROR.

    DO TRANSACTION:
        EMPTY TEMP-TABLE tt-prod-um-choices NO-ERROR.

        v-menu-list  = "".
        FOR EACH product-um FIELDS(SYSTEM-ID product-key location-key picking-um um-key)
            NO-LOCK
            WHERE product-um.SYSTEM-ID = product.SYSTEM-ID
            AND product-um.product-key = product.product-key
            AND product-um.location-key = v-bin-loc /*bin.location-key*/
            AND product-um.picking-um
            ,FIRST p-um FIELDS(um-key um)
            NO-LOCK 
            WHERE p-um.um-key = product-um.um-key
            :

            FOR EACH inventory NO-LOCK
                WHERE inventory.system-id = product.SYSTEM-ID 
                AND inventory.product-key = product.product-key 
                AND inventory.level = 5 
                AND inventory.level-key-2 = v-bin-loc /*bin.location-key */
                AND inventory.level-key-3 = "%NONE%" 
                AND inventory.level-key-4 = p-um.um-key 
                /*AND inventory.level-key-5 <> bin.bin-key */
                /*AND inventory.level-key-6 = "%NONE%"*/
                BREAK 
                BY inventory.level-key-2
                BY inventory.level-key-3
                BY inventory.level-key-4
                BY inventory.level-key-5:

                IF FIRST-OF(inventory.level-key-5) THEN 
                DO:
                   ASSIGN v-qoh = 0.
                END.
                v-qoh = v-qoh + inventory.qoh.
                IF LAST-OF(inventory.level-key-5) AND v-qoh <> 0 THEN 
                DO:
                    FIND FIRST b-bin NO-LOCK 
                        WHERE b-bin.SYSTEM-ID = inventory.SYSTEM-ID 
                        AND b-bin.bin-key = inventory.level-key-5
                        NO-ERROR.

                    RUN icumconv(INPUT product.um-key,
                        INPUT p-um.um-key,
                        INPUT NO,
                        INPUT v-qoh,
                        INPUT "",
                        INPUT "",
                        INPUT "",
                        OUTPUT rc,
                        OUTPUT v-qoh1).

                    CREATE tt-prod-um-choices.
                    ASSIGN tt-prod-um-choices.um-code = p-um.um
                        tt-prod-um-choices.um-key = p-um.um-key
                        tt-prod-um-choices.bin-id = b-bin.bin-name
                        tt-prod-um-choices.qty-avail = int(v-qoh1)
                        v-menu-list = v-menu-list 
                                    + string(b-bin.bin-name,'x(5)') + "|" 
                                    + string(tt-prod-um-choices.um-code,'x(5)') +  "|" 
                                    + STRING(tt-prod-um-choices.qty-avail) 
                                    + ',' 
                                    .
                END.
            END.
        END.
        FOR EACH tt-prod-um-choices USE-INDEX idx1
            X = 1 TO X + 1:
            ASSIGN tt-prod-um-choices.mseq = X.
        END.
        v-menu-list = trim(v-menu-list,',').
    END.

    ASSIGN 
        scr-prod = trim(lblmst.prod-code)
        scr-info1 = trim(lblmst.prod-code) + " | " + um.um
        scr-info2 = "  " + v-date-text 
        .

/*
    ASSIGN 
        prod-um-key = product.um-key
        scr-um = um.um
        v-label-um = um.um
        v-label-um-key = lblmst.um-key
        v-um-key = lblmst.um-key
        v-rowid-lblmst = ROWID(lblmst)
        .
*/
     DISPLAY scr-prod scr-info1 scr-info2 WITH FRAME scan-frame. 
END PROCEDURE.
