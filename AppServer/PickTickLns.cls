 
 /*------------------------------------------------------------------------
    File        : PickTickLns
    Syntax      : 
    Author(s)   : jmarkhorst
    Created     : Fri Nov 20 13:43:37 EST 2015
    Notes       :  update prodesc to temptable 
  ----------------------------------------------------------------------*/
  
@program FILE(name="PickTickLns.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="PickTickLns", URI="/PickTickLns", schemaName="dsPickTickLns", schemaFile="DVData/AppServer/wh/PickTickLns.i").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.
USING Progress.Json.ObjectModel.*. 

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS PickTickLns INHERITS BusinessEntity:
	/*------------------------------------------------------------------------------
			Purpose:                                                                        
			Notes:                                                                         
	------------------------------------------------------------------------------*/
	
	{"wh/PickTickLns.i"} 
	
	/* TODO Fill in appropriate database tables */
	DEFINE DATA-SOURCE srcttPickTickLns FOR oe-pick-ticket-trl.
	
	DEFINE VAR iseq AS INTEGER NO-UNDO.
    
	/*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PickTickLns():
    	
    	DEFINE VAR hDataSourceArray AS HANDLE NO-UNDO EXTENT 1.
        DEFINE VAR cSkipListArray AS CHAR NO-UNDO EXTENT 1.
        
        SUPER (DATASET dsPickTickLns:HANDLE).
        
        /* Data Source for each table in dataset. Should be in table order as defined 
     	   in DataSet */ 
     	hDataSourceArray[1] = DATA-SOURCE srcttPickTickLns:HANDLE.
     	     	
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
    @openapi.openedge.method.property (name="mappingType", value="JFP").
    @openapi.openedge.method.property (name="capabilities", value="ablFilter,top,skip,id,orderBy").
    METHOD PUBLIC VOID ReadPickTickLns(
    		INPUT filter AS CHARACTER, 
    		OUTPUT DATASET dsPickTickLns):
    	
    	RUN pickticklnqry.p (INPUT filter, INPUT-OUTPUT dataset dsPickTickLns ).
    	/*SUPER:ReadData(filter).*/
    	      
    END METHOD.
	  
    /*------------------------------------------------------------------------------
            Purpose: Create one or more new records                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/  
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="create", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID CreatePickTickLns(INPUT-OUTPUT DATASET dsPickTickLns):    		
    	       
        /*SUPER:CreateData(DATASET dsPickTickLns BY-REFERENCE).*/     
           
    END METHOD.    
	
    /*------------------------------------------------------------------------------
            Purpose:  Update one or more records                                                                  
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="update", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID UpdatePickTickLns(INPUT-OUTPUT DATASET dsPickTickLns):    		
	
        /*SUPER:UpdateData(DATASET dsPickTickLns BY-REFERENCE).*/          
    END METHOD.
	
    /*------------------------------------------------------------------------------
            Purpose:    Delete a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="delete", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID DeletePickTickLns(INPUT-OUTPUT DATASET dsPickTickLns):     		       
	
        /*SUPER:DeleteData(DATASET dsPickTickLns BY-REFERENCE).*/
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Submit a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="submit", URI="/SubmitPickTickLns", alias="", mediaType="application/json").
    METHOD PUBLIC VOID SubmitPickTickLns(INPUT-OUTPUT DATASET dsPickTickLns):     		       
	
      	/*SUPER:Submit(DATASET dsPickTickLns BY-REFERENCE).*/    	   
    END METHOD.
	 
    
END CLASS.
