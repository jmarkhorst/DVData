
/*------------------------------------------------------------------------
    File        : polbllist.i
    Purpose     : 

    Syntax      :

    Description : polabel list program

    Author(s)   : Markhorst
    Created     : Thu Dec 10 19:17:55 EST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
@openapi.openedge.entity.primarykey (fields="PoNum").

DEFINE TEMP-TABLE ttpolist BEFORE-TABLE bttpolist 
FIELD PoNum AS CHARACTER 
FIELD OkToPrint AS LOG
FIELD PrintPo AS LOG
FIELD ShipDate AS DATETIME
FIELD NumBoxes AS DECIMAL
FIELD ShipTotal AS DECIMAL
FIELD ShipToWh AS CHARACTER
FIELD VendorKey AS CHARACTER
FIELD CSVfile AS CHARACTER
FIELD CSVDate AS DATETIME
FIELD DocXfile AS CHAR
FIELD DocXDate AS DATETIME 
FIELD LinkName AS CHAR 
FIELD lblSize AS CHAR
INDEX ponum IS PRIMARY ponum.

DEFINE DATASET dspolist FOR ttpolist.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
