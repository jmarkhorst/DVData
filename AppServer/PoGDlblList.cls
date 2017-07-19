 
 /*------------------------------------------------------------------------
    File        : PoGDlblList
    Syntax      : 
    Author(s)   : jmarkhorst
    Created     : Wed May 25 14:19:58 EDT 2016
    Notes       : 
  ----------------------------------------------------------------------*/
  
@program FILE(name="PoGDlblList.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="PoGDlblList", URI="/PoGDlblList", schemaName="dspolist", schemaFile="DVData/AppServer/po/polbllist.i").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS PoGDlblList INHERITS BusinessEntity:
	/*------------------------------------------------------------------------------
			Purpose:                                                                      
			Notes:                                                                        
	------------------------------------------------------------------------------*/
	
	{"po/polbllist.i"}
	DEFINE STREAM out1.    
    DEF VAR v-dtc        AS CHAR NO-UNDO.
	
	/* TODO Fill in appropriate database tables */
	DEFINE DATA-SOURCE srcttpolist FOR ttpolist.
    
	/*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PoGDlblList():
    	
    	DEFINE VAR hDataSourceArray AS HANDLE NO-UNDO EXTENT 1.
        DEFINE VAR cSkipListArray AS CHAR NO-UNDO EXTENT 1.
        
        SUPER (DATASET dspolist:HANDLE).
        
        /* Data Source for each table in dataset. Should be in table order as defined 
     	   in DataSet */ 
     	hDataSourceArray[1] = DATA-SOURCE srcttpolist:HANDLE.
     	     	
     	/* Skip-list entry for each table in dataset. Should be in temp-table order 
           as defined in DataSet */
        /* Each skip-list entry is a comma-separated list of field names, to be
 	       ignored in create stmt */
     	
     	/* TODO Update cSkipListArray if required by your BusinessEntity */
     	cSkipListArray[1] = ?.
     	
     	THIS-OBJECT:ProDataSource = hDataSourceArray.
        THIS-OBJECT:SkipList = cSkipListArray.
     	        
    END CONSTRUCTOR.
    	
    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadPoGDlblList(
    		INPUT filter AS CHARACTER, 
    		OUTPUT DATASET dspolist):
    	RUN fillgdttpolist.p (INPUT filter, INPUT-OUTPUT dataset dspolist).
/*    	SUPER:ReadData(filter).*/
    	      
    END METHOD.
	  
    /*------------------------------------------------------------------------------
            Purpose: Create one or more new records                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/  
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="create", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID CreatePoGDlblList(INPUT-OUTPUT DATASET dspolist):    		
    	       
/*        SUPER:CreateData(DATASET dspolist BY-REFERENCE).*/
    END METHOD.    
	
    /*------------------------------------------------------------------------------
            Purpose:  Update one or more records                                                                  
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="update", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID UpdatePoGDlblList(INPUT-OUTPUT DATASET dspolist):    		
	
/*        SUPER:UpdateData(DATASET dspolist BY-REFERENCE).*/
    END METHOD.
	
    /*------------------------------------------------------------------------------
            Purpose:    Delete a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="delete", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID DeletePoGDlblList(INPUT-OUTPUT DATASET dspolist):     		       
	
/*        SUPER:DeleteData(DATASET dspolist BY-REFERENCE).*/
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Submit a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="submit", URI="/SubmitPoGDlblList", alias="", mediaType="application/json").
    METHOD PUBLIC VOID SubmitPoGDlblList(INPUT-OUTPUT DATASET dspolist):     		       
	
/*      	SUPER:Submit(DATASET dspolist BY-REFERENCE).    	*/
    END METHOD.
	 
    
END CLASS.
