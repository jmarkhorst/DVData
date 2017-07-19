/*------------------------------------------------------------------------
    File        : PickTickLns.i
    Purpose     : Display PT lines from a individual PT query	

    Syntax      :

    Description : Pick Ticket Lines 

    Author(s)   : JFM
    Created     : Fri Nov 20 13:34:59 EST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
@openapi.openedge.entity.primarykey (fields="PTNum,PTSeq").

DEF TEMP-TABLE ttPickTickLns NO-UNDO
  FIELD seq   AS INTEGER INITIAL ?
  FIELD SetId       AS CHAR 
  FIELD Scanseq      AS INT
  FIELD PickBin      AS CHAR
  FIELD ProdCd       AS CHAR
  FIELD QtyToPick    AS INT
  FIELD UmCd         AS CHAR
  FIELD ProdDesc     AS CHAR 
  FIELD PtNum        AS CHAR
  FIELD PtSeq        AS INT
  FIELD QtyPtLnPcs   AS INT
  FIELD QtyPicked    AS INT
  FIELD QtyLbls      AS INT
  FIELD UpdtDateTime AS DATETIME
  FIELD PickBinSeq   AS INT
  INDEX seq IS PRIMARY UNIQUE seq
  INDEX idx1 PtNum PtSeq
  INDEX idx2 PtNum QtyToPick DESC UpdtDateTime DESC PtSeq
  INDEX bybinprod pickbinseq pickbin prodcd scanseq  
    .
    
DEFINE DATASET dsPickTickLns FOR ttPickTickLns.    

