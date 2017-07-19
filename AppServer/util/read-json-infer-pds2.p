
/*------------------------------------------------------------------------
    File        : read-json-infer-pds2.p
    Purpose     : readjsonarray

    Syntax      :

    Description : The following procedure reads the output from write-json-pds2.p into a dynamic
ProDataSet, inferring the ProDataSet’s schema from the JSON data. It then outputs the


    Author(s)   : progress/jfm
    Created     : Thu Dec 31 11:08:25 EST 2015
    Notes       :
        The following procedure reads the output from write-json-pds2.p into a dynamic
ProDataSet, inferring the ProDataSet’s schema from the JSON data. It then outputs the
schema and data to another file, so you can examine the results:
  ----------------------------------------------------------------------*/
USING Progress.Json.ObjectModel.*.

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */

DEFINE INPUT PARAMETER ipjsonObject AS JsonObject .

/* ***************************  Main Block  *************************** */
DEF STREAM out1. 
/* read-json-infer-pds2.p */
DEFINE VARIABLE hDataset AS HANDLE  NO-UNDO.
DEFINE VARIABLE lRetOK   AS LOGICAL NO-UNDO.
DEFINE VARIABLE hQuery   AS HANDLE  NO-UNDO.
DEFINE VARIABLE hBuffer  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hField   AS HANDLE  NO-UNDO.
DEFINE VARIABLE idx1     AS INTEGER NO-UNDO.
DEFINE VARIABLE idx2     AS INTEGER NO-UNDO.
DEFINE VARIABLE idx3     AS INTEGER NO-UNDO.
CREATE DATASET hDataset.
OUTPUT stream out1 TO value("C:\workspace\TestLogs\InferPDS2.out") APPEND.

/*lRetOK = hDataset:READ-JSON("file", "dsOrderLog2.json", "empty").*/
lRetOK = hDataset:READ-JSON("longchar", ipjsonObject, "empty").

RUN displayResults.
DELETE OBJECT hDataset NO-ERROR.
OUTPUT CLOSE.


PROCEDURE displayResults:
    PUT STREAM out1 UNFORMATTED  "READ-JSON return value: " lRetOK SKIP.
    PUT STREAM out1 UNFORMATTED  SKIP 
        "** hDataset schema info **" SKIP.
    PUT STREAM out1 UNFORMATTED  "ProDataSet name: " hDataset:NAME
        "Num-buffers " hDataset:NUM-BUFFERS.
    DO idx1 = 1 TO hDataset:NUM-BUFFERS:
        hBuffer = hDataset:GET-BUFFER-HANDLE(idx1).
        PUT STREAM out1 UNFORMATTED  SKIP 
            "Buffer " idx1 "Buffer name: " hBuffer:NAME.
        PUT STREAM out1 UNFORMATTED  "Buffer Field info".
        DO idx2 = 1 TO hBuffer:NUM-FIELDS:
            hField = hBuffer:BUFFER-FIELD(idx2).
            PUT STREAM out1 UNFORMATTED  "Field name: " hField:NAME "Data type: " hField:DATA-TYPE
                " Extent: " hField:EXTENT.
        END. /* idx2 loop */
    END. /* idx1 loop */
    PUT STREAM out1 UNFORMATTED  SKIP 
        "** hDataset data **".
    DO idx1 = 1 TO hDataset:NUM-BUFFERS:
        hBuffer = hDataset:GET-BUFFER-HANDLE(idx1).
        PUT STREAM out1 UNFORMATTED  "*** Buffer " hBuffer:NAME " Data: ***".
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hBuffer).
        hQuery:QUERY-PREPARE("for each " + hBuffer:NAME).
        hQuery:QUERY-OPEN.
        hQuery:GET-NEXT() NO-ERROR.
        DO WHILE NOT hQuery:QUERY-OFF-END:
            PUT STREAM out1 UNFORMATTED  SKIP.
            DO idx2 = 1 TO hBuffer:NUM-FIELDS:
                hField = hBuffer:BUFFER-FIELD(idx2).
                IF hField:EXTENT = 0 THEN
                    PUT STREAM out1 UNFORMATTED  hField:NAME ": " hField:BUFFER-VALUE.
                ELSE
                    PUT STREAM out1 UNFORMATTED  hField:NAME.
                DO idx3 = 1 TO hField:EXTENT:
                    PUT STREAM out1 UNFORMATTED  hField:NAME ": " hField:BUFFER-VALUE(idx3).
                END. /* idx3 loop */
            END. /* idx2 loop */
            hQuery:GET-NEXT() NO-ERROR.
        END. /* hQuery loop */
        PUT STREAM out1 UNFORMATTED  SKIP.
        DELETE OBJECT hQuery NO-ERROR.
    END. /* idx1 loop */
END PROCEDURE.