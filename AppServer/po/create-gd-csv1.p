/* $Header: create-gd-csv.p,v 1.1 2015/02/03 10:57:02 sfaucett Exp $ */
/* Revision Management Header above, please DO NOT DELETE these two lines. */
DEFINE INPUT PARAMETER ip-po-num LIKE po-trailer.po-num.
DEFINE OUTPUT PARAMETER op-file-name AS CHAR.
DEFINE OUTPUT PARAMETER op-error AS LOG.

DEF VAR scr-date     AS DATE    NO-UNDO.
DEF VAR v-age-date   AS DATE    NO-UNDO.
DEF VAR v-supp-code  LIKE suppname.supplier-code NO-UNDO.
DEF VAR v-um         LIKE um.um-code NO-UNDO.
DEF VAR v-wave       LIKE route.wave NO-UNDO.
DEF VAR v-route      LIKE route.route-num NO-UNDO.
DEF VAR v-door       LIKE route.door NO-UNDO.
DEF VAR v-date-move  AS LOG     NO-UNDO.
DEF VAR v-date-text  AS CHAR    NO-UNDO.
DEF VAR v-ord-line   LIKE oe-trailer.line-num NO-UNDO.
DEF VAR v-cust       LIKE custname.cust-code NO-UNDO.
DEF VAR lbl-out-type AS CHAR    INITIAL "CSV" NO-UNDO.
DEF VAR v-gdsupp-prod-code LIKE gr-direct-dtl.supplier-prod-code NO-UNDO.
DEF VAR v-gdsched-deliv-date LIKE po-header.sched-delivery-date NO-UNDO.
DEF VAR v-gd-vendor-misc1 LIKE gr-direct-dtl.vendor-misc1 NO-UNDO.                  
DEF VAR v-gd-vendor-misc2 LIKE gr-direct-dtl.vendor-misc2 NO-UNDO.
/* added for rest ease of use*/
DEF VAR v-system-id  AS CHAR    INITIAL 'DV' NO-UNDO.
DEF VAR op-directory AS CHAR    NO-UNDO.
DEF VAR v-dirchar AS CHAR NO-UNDO.
DEF VAR gd-po-class  AS CHAR    NO-UNDO.    
DEF VAR is-gd-po     AS LOGICAL NO-UNDO.
DEF VAR v-prodname AS CHAR NO-UNDO.

/* end of add */

DEFINE STREAM label-out.
DEFINE STREAM out1.
DEF VAR v-dtc AS CHAR NO-UNDO.
        
ASSIGN 
    /*op-directory = "\\vweb2\WebSites\DVC\Labels\"*/
    op-directory = "/WebSites/DVC/Labels/"
    v-dirchar = '/'
    v-dtc = STRING(NOW)
    v-dtc = REPLACE(v-dtc,'/','')
    v-dtc = REPLACE(v-dtc,' ','-') 
    v-dtc = REPLACE(v-dtc,':','')
    v-dtc = SUBSTRING(v-dtc,1,R-INDEX(v-dtc,'-') - 1)
    .       

/*   for debugging propath issue */
/*OUTPUT stream out1 to value("C:\workspace\TestLogs\cr-here" + v-dtc + ".txt").  */
/*PUT STREAM out1 '1)processing:' ip-po-num SKIP.                                 */
/*/*PUT STREAM out1 UNFORMATTED "3a)= " SEARCH("gd-lblprnt-csv.r") SKIP PROPATH.*/*/
/*OUTPUT stream out1 close.                                                       */

/*{system.i}*/
/*{dv-rcvngprnt.i}*/

ASSIGN 
    lbl-out-type = "CSV". 


FIND FIRST po-header NO-LOCK WHERE
    po-header.SYSTEM-ID =  v-system-id  AND
    po-header.po-num = ip-po-num NO-ERROR.
 
IF NOT AVAIL po-header THEN
DO :
    ASSIGN 
        op-error = TRUE.
    RETURN.
END.

FOR FIRST suppname FIELDS(SYSTEM-ID supplier-key supplier-code) NO-LOCK  
    WHERE suppname.SYSTEM-ID = po-header.SYSTEM-ID AND
    suppname.supplier-key = po-header.supplier-key 
    :
    ASSIGN 
        v-supp-code = suppname.supplier-code.
END.


FIND FIRST gen-attrib NO-LOCK 
    WHERE gen-attrib.table-field = "gr-dir" 
    AND gen-attrib.attribute-name = "po class" 
    AND gen-attrib.attribute-value = "global"
    NO-ERROR.
IF AVAILABLE (gen-attrib) THEN 
    ASSIGN gd-po-class = gen-attrib.field-value.

/*
RUN find-dir.p (INPUT "unix/w-prbid.w", OUTPUT op-directory).
*/

/*for windows op-directory needs to be a UNC reference */
/*op-directory =  op-directory + trim(suppname.supplier-code) + '\'.*/

ASSIGN 
    op-file-name = op-directory + trim(suppname.supplier-code) + v-dirchar
                   + suppname.supplier-code + "_" + po-header.po-num + "-" + 
                   STRING(TODAY,"999999") + "-" + STRING(TIME,"999999") + ".csv".

/*JM add GD-header check*/
IF AVAILABLE (po-header) AND po-header.po-class = gd-po-class THEN
    ASSIGN is-gd-po = YES.
ELSE 
    ASSIGN is-gd-po = NO.
/*JM add GD-header check*/

/*PUT STREAM out1 UNFORMATTED '2: op-file-name :== ' op-file-name SKIP.*/

OUTPUT STREAM label-out TO VALUE(op-file-name) . 

/*PUT STREAM out1 UNFORMATTED "llabel-out open" SKIP.*/

RUN label-begin.

FOR EACH lblmst WHERE lblmst.company-id = po-header.SYSTEM-ID AND
    lblmst.po-num = po-header.po-num NO-LOCK:
    
/*    PUT STREAM out1 UNFORMATTED "1) in foreach lblmst: " lblmst.expired-date.*/
        
    IF lblmst.expired-date LT TODAY THEN
        NEXT.

    FOR FIRST product FIELDS(SYSTEM-ID
        product-code
        product-name
        category-key
        sub-category-key) WHERE
        product.SYSTEM-ID = po-header.SYSTEM-ID AND
        product.product-code = lblmst.prod-code NO-LOCK:
        
    END.
    IF NOT AVAIL product THEN
        NEXT.
    
    ASSIGN 
        v-Age-Date = IF po-header.sched-delivery-date NE ? THEN
                           po-header.sched-delivery-date
                        ELSE po-header.required-date 
        v-um       = "".

    FIND FIRST rcvlblexception NO-LOCK WHERE
        rcvlblexception.system-id = po-header.SYSTEM-ID AND
        rcvlblexception.category-key = product.category-key
        NO-ERROR.
    v-date-move = NO.
    IF AVAIL rcvlblexception THEN
    DO:
        FIND FIRST sub-category NO-LOCK WHERE
            sub-category.system-id = po-header.SYSTEM-ID AND
            sub-category.sub-category-key = product.sub-category-key
            NO-ERROR.

        IF sub-category.sub-category-code MATCHES rcvlblexception.sub-cat-list THEN            
            ASSIGN v-date-move = YES
                v-date-text = STRING(v-age-date - DATE('12/31/' +
                                     STRING(YEAR(v-age-date) - 1, '9999'))).
            
    END.
    ASSIGN 
        v-date-text = IF v-date-move THEN "" ELSE v-date-text.

    FIND FIRST um WHERE um.um-key  = lblmst.um-key NO-LOCK NO-ERROR.
    IF AVAIL um THEN
        ASSIGN v-um = um.um-code.

    ASSIGN 
        v-ord-line = 0
        v-cust     = ""
        v-wave     = ""
        v-route    = ""
        v-door     = ""
        v-gdsupp-prod-code   = ""   /** cfr 020310 **/
        v-gdsched-deliv-date = ?      /** cfr 020910 **/
        v-gd-vendor-misc1    = ""         /** cfr 020310 **/
        v-gd-vendor-misc2    = ""   
    .
    IF po-header.po-class =  gd-po-class  THEN
    DO:
    
        FOR FIRST po-trailer WHERE 
            po-trailer.system = po-header.system AND
            po-trailer.po-num = po-header.po-num AND
            po-trailer.line-num = lblmst.po-line-num NO-LOCK ,
            FIRST oe-trailer WHERE                   /*** cfr - 020310 **/
            oe-trailer.system = po-header.system AND
            oe-trailer.entry-key = "" AND 
            oe-trailer.order-num = po-header.cust-order-num AND 
            oe-trailer.po-num = po-header.po-num AND
            oe-trailer.po-line-num = po-trailer.line-num NO-LOCK,
 
            FIRST adv-oeline WHERE
            adv-oeline.system = po-header.system AND
            adv-oeline.order-num = oe-trailer.order-num AND
            adv-oeline.order-line-num = oe-trailer.line-num NO-LOCK,
            FIRST gr-direct-dtl WHERE
            gr-direct-dtl.system = po-header.system AND
            gr-direct-dtl.offer-key = adv-oeline.offer-key AND
            gr-direct-dtl.line-num = adv-oeline.offer-line-num AND
            gr-direct-dtl.latest-rev NO-LOCK,   
            
            FIRST oe-header WHERE 
            oe-header.system = po-header.system AND
            oe-header.order-num = po-header.cust-order-num NO-LOCK,
            FIRST custname WHERE 
            custname.system = po-header.system AND
            custname.cust-key = oe-header.cust-key NO-LOCK BY lblmst.po-line-num:

            ASSIGN 
                v-ord-line = oe-trailer.line-num
                v-cust     = custname.cust-code.

            FIND FIRST route WHERE 
                route.system = oe-header.system AND
                route.route-num = oe-header.route-num NO-LOCK NO-ERROR.
            IF AVAIL route THEN
                ASSIGN v-wave  = route.wave
                    v-route = route.route-num
                    v-door  = route.door.
                    

            
            ASSIGN 
                v-gdsupp-prod-code   = gr-direct-dtl.supplier-prod-code   /** cfr 020310 **/
                v-gdsched-deliv-date = po-header.sched-delivery-date      /** cfr 020910 **/
                v-gd-vendor-misc1    = gr-direct-dtl.vendor-misc1         /** cfr 020310 **/
                v-gd-vendor-misc2    = gr-direct-dtl.vendor-misc2.        /** cfr 020310 **/ 
                    
                    
                    
        END.
    END.
    
    v-prodname = product.product-name.
    IF is-gd-po THEN v-prodname = lblmst.user-chr1.
    
    RUN label-data(INPUT product.product-code,
        INPUT v-prodname,
        INPUT lblmst.bin-id,
        INPUT v-date-text,
        INPUT lblmst.vend-part,
        INPUT v-supp-code,
        INPUT v-um,
        INPUT po-header.po-num,
        INPUT lblmst.label-id, 
        INPUT lblmst.po-line-num,     /** cfr 031506  **/
        INPUT po-header.cust-order-num,       /** cfr 111708 - new fields for Grower Direct labels **/
        INPUT v-ord-line,
        INPUT v-cust,
        INPUT v-wave,
        INPUT v-route, 
        INPUT v-door,
        INPUT v-gdsupp-prod-code,
        INPUT v-gdsched-deliv-date,
        INPUT v-gd-vendor-misc1,
        INPUT v-gd-vendor-misc2 
        ).           /** cfr 111708 - end of new fields **/
END.

RUN label-data(INPUT "",
    INPUT "END OF RUN",
    INPUT "",
    INPUT "",
    INPUT "",
    INPUT "",
    INPUT "",
    INPUT "",
    INPUT "",   /*** cfr 031506 **/
    INPUT "",   /** cfr 111708 start of new fields **/
    INPUT "",
    INPUT "", 
    INPUT "",
    INPUT "",
    INPUT "",
    INPUT "").  /** cfr 111708 end **/

RUN label-end.

OUTPUT STREAM label-out CLOSE.

/*PUT STREAM out1 'got-to-end' SKIP.*/
/*OUTPUT stream out1 close.*/

PROCEDURE label-begin:
/*    PUT STREAM out1 UNFORMATTED "1) label-begin ".*/
    RUN CSV-OUTPUT-LBL-Hdr.  /* .csv CSV OUT CSV */
END PROCEDURE.
    
PROCEDURE label-data:
/*    PUT STREAM out1 UNFORMATTED "1) label-data ".*/
    DEF INPUT PARAMETER ip-product   AS CHAR.
    DEF INPUT PARAMETER ip-desc      AS CHAR.
    DEF INPUT PARAMETER ip-bin       AS CHAR.
    DEF INPUT PARAMETER ip-date      AS CHAR.
    DEF INPUT PARAMETER ip-vend-part AS CHAR.
    DEF INPUT PARAMETER ip-vend-id   AS CHAR.
    DEF INPUT PARAMETER ip-um        AS CHAR.
    DEF INPUT PARAMETER ip-po-num    AS CHAR.
    DEF INPUT PARAMETER ip-label-id  AS DEC.
    DEF INPUT PARAMETER ip-line-num  AS CHAR.  /** cfr 031506 **/
    DEF INPUT PARAMETER ip-order-num AS CHAR.     /** cfr 111708 - added fields for Grower Direct labels **/
    DEF INPUT PARAMETER ip-ord-line-num AS CHAR.
    DEF INPUT PARAMETER ip-cust-code AS CHAR.
    DEF INPUT PARAMETER ip-wave-num  AS CHAR.
    DEF INPUT PARAMETER ip-route-num AS CHAR.
    DEF INPUT PARAMETER ip-door      AS CHAR.    /** cfr 111708 - end of new fields **/
    
    DEF INPUT PARAMETER ip-gdsupp-prod-code AS CHAR.
    DEF INPUT PARAMETER ip-gdsched-deliv-date AS CHAR.
    DEF INPUT PARAMETER ip-gd-vendor-misc1 AS CHAR.
    DEF INPUT PARAMETER ip-gd-vendor-misc2 AS CHAR.

    DEF VAR vend-part-hight AS INT .
    DEF VAR yy              AS INT.
    DEF VAR v-um-field      AS CHAR .

   
    ASSIGN
        vend-part-hight = 60
        v-um-field      = IF ip-um NE "" THEN 'UM: ' + ip-um ELSE ip-um
        ip-po-num       = IF ip-po-num NE "" THEN "PO#:" + ip-po-num ELSE ip-po-num.

    IF LENGTH(ip-vend-part) LT 17 THEN 
    DO:
        ASSIGN
            yy           = ROUND((17 - LENGTH(ip-vend-part)) / 2,0)
            ip-vend-part = FILL(' ',yy) + ip-vend-part.
    END.
    IF LENGTH(ip-vend-part) GT 17 THEN
        vend-part-hight = 52.

    CASE lbl-out-type:
        WHEN "csv"   THEN 
            DO:   /* .csv CSV OUT CSV */
                IF ip-desc  NE "END OF RUN" THEN 
                DO:
                    RUN CSV-OUTPUT-LBL-Det (INPUT ip-product,
                        INPUT ip-desc,
                        INPUT ip-bin,
                        INPUT ip-date,
                        INPUT ip-vend-part,
                        INPUT ip-vend-id,
                        INPUT ip-um,
                        INPUT ip-po-num,
                        INPUT v-um-field,
                        INPUT ip-label-id,    
                        INPUT ip-line-num,    /*** cfr 031506 ***/
                        INPUT ip-order-num,   /*** cfr 111708 - new fields used for Grower Direct  **/
                        INPUT ip-ord-line-num ,
                        INPUT ip-cust-code,
                        INPUT ip-wave-num,
                        INPUT ip-route-num,
                        INPUT ip-door,
                        INPUT ip-gdsupp-prod-code,
                        INPUT ip-gdsched-deliv-date,
                        INPUT ip-gd-vendor-misc1,
                        INPUT ip-gd-vendor-misc2
                        ).      /*** cfr 111708 - end of new field **/
                END.
                ELSE
                    RUN CSV-OUTPUT-LBL-End (INPUT ip-desc,
                        INPUT ip-label-id
                        ).
            /*                MESSAGE 'Got here 2 ' ip-label-id VIEW-AS ALERT-BOX. */
            END.
        
    END CASE.

END PROCEDURE.


PROCEDURE label-end:
/*    PUT STREAM out1 UNFORMATTED "1) label-end".*/
    
    RUN CSV-OUTPUT-LBL-end (INPUT "", INPUT "" ).  /* .csv CSV OUT CSV */
END PROCEDURE.


/***************** C S V  O U T P U T ********************** C S V **/
PROCEDURE CSV-OUTPUT-LBL-Hdr:

    IF NOT is-gd-po THEN
        PUT STREAM label-out UNFORMATTED
            "Code"        + "," +
            "Desc"        + "," +
            "Bin"         + "," +
            "Qty"         + "," +
            "Date"        + "," +
            "Stock"       + "," +
            "Vendor"      + "," +
            "UM"          + "," +
            "PO-num"      + "," +
            "Label-id"    + "," +   
            "Line #"                /** cfr 031506 **/
            SKIP.
    ELSE
        PUT STREAM label-out UNFORMATTED
            "Code"        + "," +
            "Desc"        + "," +
            "Bin"         + "," +
            "Qty"         + "," +
            "Date"        + "," +
            "Stock"       + "," +
            "Vendor"      + "," +
            "UM"          + "," +
            "PO-num"      + "," +
            "Label-id"    + "," +   
            "Line #"      + "," +   /** cfr 031506 **/
            "Ord Num"     + "," +   /** cfr 111708 - new fields for GD labels **/
            /**"Ord Line#"   + "," +  **/        
            "Cust Cd"     + "," +  
            "Wave "       + "," +
            "Route"       + "," + 
            "Door"        + "," +           /** cfr 111708 - end of new fields **/
            "Supp Prod"   + "," +  /** cfr 020310 **/
           "Cust Deliv"   + "," +   /** cfr 020910 **/
           "Misc1"        + "," + /** cfr 020310 **/
           "Misc2"      /** cfr 020310 **/
            SKIP.

END PROCEDURE.

PROCEDURE CSV-OUTPUT-LBL-Det:

    DEF INPUT PARAMETER ip-product   AS CHAR.
    DEF INPUT PARAMETER ip-desc      AS CHAR.
    DEF INPUT PARAMETER ip-bin       AS CHAR.
    DEF INPUT PARAMETER ip-date      AS CHAR.
    DEF INPUT PARAMETER ip-vend-part AS CHAR.
    DEF INPUT PARAMETER ip-vend-id   AS CHAR.
    DEF INPUT PARAMETER ip-um        AS CHAR.
    DEF INPUT PARAMETER ip-po-num    AS CHAR.
    DEF INPUT PARAMETER v-um-field   AS CHAR.
    DEF INPUT PARAMETER ip-label-id  AS DEC.
    DEF INPUT PARAMETER ip-line-num  AS CHAR.   /** cfr 031506 **/
    DEF INPUT PARAMETER ip-order-num AS CHAR.   /** cfr 111708 - new fields for GD labels **/
    DEF INPUT PARAMETER ip-ord-line-num AS CHAR. 
    DEF INPUT PARAMETER ip-cust-code AS CHAR.
    DEF INPUT PARAMETER ip-wave-num  AS CHAR.
    DEF INPUT PARAMETER ip-route-num AS CHAR.
    DEF INPUT PARAMETER ip-door      AS CHAR.   /** cfr 111708 - end of new fields **/
    DEF INPUT PARAMETER ip-gdsupp-prod-code AS CHAR.
    DEF INPUT PARAMETER ip-gdsched-deliv-date AS CHAR.
    DEF INPUT PARAMETER ip-gd-vendor-misc1 AS CHAR.
    DEF INPUT PARAMETER ip-gd-vendor-misc2 AS CHAR.

    IF NOT is-gd-po THEN
        PUT STREAM label-out UNFORMATTED
            '"' +
            ip-product                      + '","' +
            ip-desc                         + '","' +
            ip-bin                          + '",'  +
            STRING(1)                       + ',"'  +
            ip-date                         + '","' +
            TRIM(ip-vend-part)              + '","' +
            TRIM(ip-vend-id)                + '","' +
            REPLACE(v-um-field, "UM:", "")  + '","' +
            REPLACE(ip-po-num, "PO#:", "")  + '","' +
            /*>2012.05.29-JL>
                     STRING(ip-label-id,"9999.9999") + '","' +   /*** jfm ***/
              */
            (IF ip-label-id - INT(ip-label-id) > 0
            THEN TRIM(STRING(ip-label-id,">>>>9.9999")) + '","' /*2012.06.22-JL Added 'TRIM'*/
            ELSE TRIM(STRING(ip-label-id,">>>>>>>>>9")) + '","' /*2012.06.22-JL Added 'TRIM'*/
            ) +
            /*<2012.05.29-JL<*/
            ip-line-num                     + '"'       /** cfr 031506 **/ 
            SKIP.
    ELSE
        PUT STREAM label-out UNFORMATTED
            '"' +
            ip-product                      + '","' +
            ip-desc                         + '","' +
            ip-bin                          + '",'  +
            STRING(1)                       + ',"'  +
            ip-date                         + '","' +
            TRIM(ip-vend-part)              + '","' +
            TRIM(ip-vend-id)                + '","' +
            REPLACE(v-um-field, "UM:", "")  + '","' +
            REPLACE(ip-po-num, "PO#:", "")  + '","' +
            /*>2012.05.29-JL>
                     STRING(ip-label-id,"9999.9999") + '","' +   /*** jfm ***/
              */
            (IF ip-label-id - INT(ip-label-id) > 0
            THEN TRIM(STRING(ip-label-id,">>>>9.9999")) + '","' /*2012.06.22-JL Added 'TRIM'*/
            ELSE TRIM(STRING(ip-label-id,">>>>>>>>>9")) + '","' /*2012.06.22-JL Added 'TRIM'*/
            ) +
            /*<2012.05.29-JL<*/
            ip-line-num                     + '","'  +  /** cfr 031506 **/ 
            ip-order-num                    + '","'  +  /** cfr 111708 - new fields for GD **/
            /* ip-ord-line-num                 + '","'  +  */
            ip-cust-code                    + '","'  + 
            ip-wave-num                     + '","'  +
            ip-route-num                    + '","'  +
            ip-door                         + '","'  +
            ip-gdsupp-prod-code             + '","'  +
            ip-gdsched-deliv-date           + '","'  +
            ip-gd-vendor-misc1              + '","'  +
            ip-gd-vendor-misc2              + '"'        /** cfr 111708 - end of new fields **/
            SKIP.

END PROCEDURE.


PROCEDURE CSV-OUTPUT-LBL-End:
    DEF INPUT PARAMETER ip-desc      AS CHAR.
    DEF INPUT PARAMETER ip-label-id  AS DEC.

    PUT STREAM label-out UNFORMATTED

        '"'           + '","' +
        ""            + '","' +
        ""            + '",,,"' +

        ip-desc       + '"'
        SKIP.

END PROCEDURE.




/*
PROCEDURE load-super-proc :

 DEF VAR sess-super-counter AS INTEGER NO-UNDO INITIAL 1.
 DEF VAR sess-super-handle AS HANDLE NO-UNDO.
  
  DO WHILE sess-super-counter LE NUM-ENTRIES(SESSION:SUPER-PROCEDURES):
     sess-super-handle = WIDGET-HANDLE(ENTRY(sess-super-counter,SESSION:SUPER-PROCEDURES)).
     IF VALID-HANDLE(sess-super-handle) AND INDEX(sess-super-handle:INTERNAL-ENTRIES,"alloc-proc") GT 0 THEN
        ASSIGN sess-super-counter = 99999.
     ELSE
        ASSIGN sess-super-counter = sess-super-counter + 1.
  END.
  
  IF sess-super-counter LT 99999 THEN 
     RUN picklogic.p PERSISTENT SET sess-super-handle.
  
  SESSION:ADD-SUPER-PROCEDURE(sess-super-handle).
END PROCEDURE.
*/
