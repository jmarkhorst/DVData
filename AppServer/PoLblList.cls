 
/*------------------------------------------------------------------------
   File        : PoLblList
   Syntax      : 
   Author(s)   : jmarkhorst
   Created     : Thu Dec 10 19:27:31 EST 2015
   Notes       : 
   url to use  : http://is-jmwin7:8085/DVData/rest/DVData/PoLblList
 ----------------------------------------------------------------------*/
  
@program FILE(name="PoLblList.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
@progress.service.resource FILE(name="PoLblList", URI="/PoLblList", schemaName="dspolist", schemaFile="DVData/AppServer/po/polbllist.i").

USING Progress.Lang.*.
USING OpenEdge.BusinessLogic.BusinessEntity.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS PoLblList INHERITS BusinessEntity:
	/*------------------------------------------------------------------------------
			Purpose:                                                                      
			Notes:                                                                        
	------------------------------------------------------------------------------*/
	/* make sure you have appserv/po in propath*/
    {"po/polbllist.i"}
    DEFINE STREAM out1.	
    DEF VAR v-dtc        AS CHAR NO-UNDO.
    /* TODO Fill in appropriate database tables */
    DEFINE DATA-SOURCE srcttpolist FOR ttpolist /* use temptable name here from include apprise.po-header*/.
    
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PoLblList():
    	
        DEFINE VARIABLE hDataSourceArray AS HANDLE    NO-UNDO EXTENT 1.
        DEFINE VARIABLE cSkipListArray   AS CHARACTER NO-UNDO EXTENT 1.
        
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
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID PoLblFillMethod(INPUT filter AS CHARACTER  ):
        
        RUN fillttpolist.p (INPUT filter, INPUT-OUTPUT dataset dspolist).

        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadPoLblList(
        INPUT filter AS CHARACTER, 
        OUTPUT DATASET dspolist):
        
/*        MESSAGE 'ReadPoLblList'.*/
        
        ASSIGN 
            v-dtc = STRING(NOW)
            v-dtc = REPLACE(v-dtc,'/','')
            v-dtc = REPLACE(v-dtc,' ','-') 
            v-dtc = REPLACE(v-dtc,':','')
            v-dtc = SUBSTRING(v-dtc,1,R-INDEX(v-dtc,'-') - 1)
            .   
            
/*        OUTPUT stream out1 to value("C:\workspace\TestLogs\readpolbllist-" + v-dtc + ".txt").*/
            
/*    	PUT STREAM out1 UNFORMATTED 'readpolbllist-- filter= ' filter SKIP.*/
    	OUTPUT stream out1 close. 
    	
        THIS-OBJECT:PoLblFillMethod (INPUT filter).
        
    /*SUPER:ReadData(filter).*/
    	      
    END METHOD.
	  
    /*------------------------------------------------------------------------------
            Purpose: Create one or more new records                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/  
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="create", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID CreatePoLblList(INPUT-OUTPUT DATASET dspolist):    		
    	 
/*        MESSAGE 'createpolbllist'.*/
        /*SUPER:CreateData(DATASET dspolist BY-REFERENCE).*/   
        RETURN.      
    END METHOD.    
	
    /*------------------------------------------------------------------------------
            Purpose:  Update one or more records                                                                  
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="update", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID UpdatePoLblList(INPUT-OUTPUT DATASET dspolist):    		
/*        MESSAGE 'updatepolbllist'.*/
        /*SUPER:UpdateData(DATASET dspolist BY-REFERENCE).*/
        RETURN.           
    END METHOD.
	
    /*------------------------------------------------------------------------------
            Purpose:    Delete a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="delete", URI="", alias="", mediaType="application/json").
    METHOD PUBLIC VOID DeletePoLblList(INPUT-OUTPUT DATASET dspolist):     		       
/*        MESSAGE 'deletepolbllist'.*/
        /*SUPER:DeleteData(DATASET dspolist BY-REFERENCE).*/
        RETURN. 
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:    Submit a record                                                               
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="true").
    @progress.service.resourceMapping(type="REST", operation="submit", URI="/SubmitPoLblList", alias="", mediaType="application/json").
    METHOD PUBLIC VOID SubmitPoLblList(INPUT-OUTPUT DATASET dspolist):
        DEF VAR vvfilter AS CHAR INIT "nofilter".      		       
        
/*        MESSAGE 'run-4submitpolbllist'.*/
        
        RUN polblprocs.p ( INPUT 'SubmitPoLblList' , INPUT-OUTPUT dataset dspolist , INPUT vvfilter ).
        	   
        /*SUPER:Submit(DATASET dspolist BY-REFERENCE).*/
        RETURN.     	   
    END METHOD.
	 
    
END CLASS.
