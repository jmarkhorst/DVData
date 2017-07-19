 
 /*------------------------------------------------------------------------
    File        : Customer
    Syntax      : 
    Author(s)   : jmarkhorst
    Created     : Tue Nov 17 13:19:10 EST 2015
    Notes       : 
  ----------------------------------------------------------------------*/
  
@program FILE(name="Customer.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="Customer", URI="/Customer", schemaName="dscustname", schemaFile="DVData/AppServer/Customer/customer.i").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS DVCustomer INHERITS BusinessEntity:
	/*------------------------------------------------------------------------------
			Purpose:                                                                      
			Notes:                                                                        
	------------------------------------------------------------------------------*/
	
	{"Customer/customer.i"}
	
	DEFINE DATA-SOURCE srccustname FOR custname.
	
    
	/*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DVCustomer():
    	
    	DEFINE VAR hDataSourceArray AS HANDLE NO-UNDO EXTENT 1.
        DEFINE VAR cSkipListArray AS CHAR NO-UNDO EXTENT 1.
        
        SUPER (DATASET dscustname:HANDLE).
        
        /* Data Source for each table in dataset. Should be in table order as defined 
     	   in DataSet */     
     	hDataSourceArray[1] =  DATA-SOURCE srccustname:HANDLE.
     	
     	     	
     	/* Skip-list entry for each table in dataset. Should be in temp-table order 
           as defined in DataSet */
        /* Each skip-list entry is a comma-separated list of field names, to be
 	       ignored in create stmt */
     	    
     	cSkipListArray[1] = "".
     	
     	
     	THIS-OBJECT:ProDataSource = hDataSourceArray.
        THIS-OBJECT:SkipList = cSkipListArray.
     	        
    END CONSTRUCTOR.
    	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID JFPFillMethod(INPUT filter AS CHARACTER):
	   EMPTY TEMP-TABLE ttcustname.
	   MESSAGE ' JFM2'. 
	    FOR EACH custname NO-LOCK 
	    WHERE custname.SYSTEM-ID = 'DV'
	    AND custname.cust-code = 'MARKK'
	    /*AND custname.customer-class= "Employee"*/ 
	     :
	         CREATE ttcustname.
	         BUFFER-COPY custname TO ttcustname.
	    END.
		
		RETURN.

	END METHOD.

    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadCustomer(
    		INPUT filter AS CHARACTER, 
    		OUTPUT DATASET dscustname):
    	
    	THIS-OBJECT:JFPFillMethod (INPUT filter).	    
    	/*	    
    	IF filter BEGINS "~{" THEN 
            THIS-OBJECT:JFPFillMethod (INPUT filter).
            
        ELSE 
        DO:
            /*BUFFER ttCustname:HANDLE:BATCH-SIZE = 0.*/   
            /*JFM add system id to where clause*/
            filter = "WHERE custname.SYSTEM-ID = 'DV' AND custname.cust-code = 'MARKK' ".
            SUPER:ReadData(filter).
        END. 
        */
         
    END METHOD.
	  
    /*------------------------------------------------------------------------------
            Purpose: Create one or more new records                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/  
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    @progress.service.resourceMapping(type="REST", operation="create", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID CreateCustomer(INPUT-OUTPUT DATASET dscustname):    		
    	       
        SUPER:CreateData(DATASET dscustname BY-REFERENCE).        
    END METHOD.    
	
    /*------------------------------------------------------------------------------
            Purpose:  Update one or more records                                                                  
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    @progress.service.resourceMapping(type="REST", operation="update", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID UpdateCustomer(INPUT-OUTPUT DATASET dscustname):    		
	
        SUPER:UpdateData(DATASET dscustname BY-REFERENCE).          
    END METHOD.
	
    /*------------------------------------------------------------------------------
            Purpose:    Delete a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    @progress.service.resourceMapping(type="REST", operation="delete", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID DeleteCustomer(INPUT-OUTPUT DATASET dscustname):     		       
	
        SUPER:DeleteData(DATASET dscustname BY-REFERENCE).
    END METHOD.
	 
    
END CLASS.
