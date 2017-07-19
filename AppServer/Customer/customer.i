
 /*------------------------------------------------------------------------
    File        : Customer
    Purpose		:
    Syntax      : 
    Description :
    Author(s)   : jmarkhorst
    Created     : Tue Nov 17 13:19:10 EST 2015
    Notes       : 
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

  /** Dynamically generated schema file **/
   
@openapi.openedge.entity.primarykey (fields="system-id,cust-name,cust-code").
	
DEFINE TEMP-TABLE ttcustname BEFORE-TABLE bttcustname
FIELD system-id AS CHARACTER
FIELD cust-name AS CHARACTER LABEL "Customer Name"
FIELD cust-code AS CHARACTER LABEL "ID"
FIELD cust-key AS CHARACTER LABEL "Key"
FIELD cust-name-soundex AS CHARACTER
FIELD address-1 AS CHARACTER
FIELD address-2 AS CHARACTER
FIELD city AS CHARACTER
FIELD state AS CHARACTER
FIELD zip AS CHARACTER
FIELD country AS CHARACTER
FIELD history-company AS CHARACTER LABEL "History Co. Code"
FIELD phone AS CHARACTER
FIELD fax AS CHARACTER LABEL "FAX #"
FIELD group-id AS CHARACTER LABEL "Group ID"
FIELD default-terms AS CHARACTER LABEL "Terms"
FIELD default-currency AS CHARACTER LABEL "Currency"
FIELD default-payment-method AS CHARACTER LABEL "Payment Method"
FIELD normal-payment-method AS CHARACTER
FIELD normal-payment-days AS INTEGER INITIAL "0"
FIELD default-discount AS DECIMAL INITIAL "0" LABEL "Standard Discount"
FIELD start-date AS DATE LABEL "Start Date"
FIELD active AS LOGICAL INITIAL "yes"
FIELD fed-tax-exempt AS CHARACTER LABEL "Federal Tax exempt #"
FIELD state-tax-exempt AS CHARACTER
FIELD date-first-order AS DATE
FIELD cust-division AS CHARACTER
FIELD trade-class AS CHARACTER LABEL "Trade Class"
FIELD default-sales-agent AS CHARACTER LABEL "Sales Agent"
FIELD territory AS CHARACTER LABEL "Territory"
FIELD sales-location-key AS CHARACTER LABEL "Sales Location"
FIELD gets-statement AS LOGICAL INITIAL "no" LABEL "Statement"
FIELD gets-finance-charges AS LOGICAL INITIAL "no" LABEL "Finance Charge"
FIELD db-code AS CHARACTER LABEL "D&B Code"
FIELD target-customer AS LOGICAL INITIAL "no" LABEL "Target Customer"
FIELD customer-class AS CHARACTER LABEL "Class"
FIELD gets-price-change AS LOGICAL INITIAL "no" LABEL "Price Change"
FIELD pays-superfund AS LOGICAL INITIAL "no" LABEL "Superfund"
FIELD default-price-code AS CHARACTER LABEL "Price Code"
FIELD billing-customer-code AS CHARACTER LABEL "Billing Code"
FIELD credit-customer-key AS CHARACTER LABEL "Credit Company"
FIELD preferred-shipper AS CHARACTER LABEL "Shipper ID"
FIELD gets-misc-charges AS LOGICAL INITIAL "no" LABEL "Miscellaneous Charges"
FIELD over-due-days AS INTEGER INITIAL "0" LABEL "Overdue Days"
FIELD overdue-percentage AS DECIMAL INITIAL "0" LABEL "Overdue %"
FIELD credit-limit AS DECIMAL INITIAL "0" LABEL "Credit Limit"
FIELD overdue-amount AS DECIMAL INITIAL "0" LABEL "Overdue Amt"
FIELD gets-container-service AS LOGICAL INITIAL "no" LABEL "Container Service Charge"
FIELD manifest-needed AS LOGICAL INITIAL "no" LABEL "Manifest Required"
FIELD on-credit-hold AS LOGICAL INITIAL "no" LABEL "Credit Hold"
FIELD on-credit-watch AS LOGICAL INITIAL "no"
FIELD min-fin-chg-threshold AS DECIMAL INITIAL "0"
FIELD min-fin-chg-amt AS DECIMAL INITIAL "0"
FIELD industry-code AS CHARACTER
FIELD order-priority AS INTEGER INITIAL "0"
FIELD req-ord-acknow AS LOGICAL INITIAL "no" LABEL "Require Order Acknowledgement"
FIELD language AS CHARACTER
FIELD req-invoice AS LOGICAL INITIAL "no" LABEL "Require Invoice"
FIELD agent-code AS CHARACTER
FIELD bank AS CHARACTER
FIELD bank-acct-num AS CHARACTER
FIELD reference AS CHARACTER
FIELD consignee-code AS CHARACTER
FIELD credi-review-date AS DATE
FIELD credit-score AS DECIMAL INITIAL "0"
FIELD req-c-of-a AS LOGICAL INITIAL "no" LABEL "Require Certificate of Analysis"
FIELD update-by AS CHARACTER
FIELD update-date AS DATE
FIELD update-time AS CHARACTER
FIELD created-by AS CHARACTER
FIELD created-date AS DATE
FIELD security-access AS CHARACTER
FIELD user-1 AS CHARACTER
FIELD supplier-key AS CHARACTER
FIELD ar-gl-acct-ptr AS CHARACTER
FIELD alt-search AS CHARACTER
FIELD credit-matrix AS DECIMAL EXTENT 15 INITIAL "0"
FIELD email-address AS CHARACTER
FIELD credit-class-key AS CHARACTER
FIELD price-book-key AS CHARACTER
FIELD default-credit-check AS CHARACTER
FIELD consolidate-invoices AS CHARACTER
FIELD cancel-backorder AS LOGICAL INITIAL "NO"
FIELD second-sales-agent AS CHARACTER
FIELD print-invoices AS LOGICAL INITIAL "no"
FIELD ucc128-format AS CHARACTER
FIELD ship-complete AS LOGICAL INITIAL "no"
FIELD ship-bo-complete AS LOGICAL INITIAL "no"
FIELD disc-acct-mask AS CHARACTER
FIELD price-cust-key AS CHARACTER
FIELD co-op-flag AS LOGICAL INITIAL "no"
FIELD comm-book-name AS CHARACTER
FIELD ft-approved AS LOGICAL INITIAL "no"
FIELD internal-customer AS LOGICAL INITIAL "no"
FIELD shipping-location-key AS CHARACTER
FIELD gets-ucc128 AS CHARACTER
FIELD cust-type AS CHARACTER
FIELD freight-free AS LOGICAL INITIAL "no"
FIELD county AS CHARACTER
INDEX cust-by-alt  system-id  ASCENDING  alt-search  ASCENDING 
INDEX cust-city-idx  system-id  ASCENDING  city  ASCENDING  cust-name  ASCENDING 
INDEX CUST-CODE-ACTIVE  system-id  ASCENDING  cust-code  ASCENDING  active  ASCENDING 
INDEX CUST-IDX-BY-ACTIVE-BILL  system-id  ASCENDING  active  ASCENDING  billing-customer-code  ASCENDING 
INDEX CUST-IDX-BY-ACTIVE-CRED  system-id  ASCENDING  active  ASCENDING  credit-customer-key  ASCENDING 
INDEX CUST-IDX-BY-ACTIVE-HIST  system-id  ASCENDING  active  ASCENDING  history-company  ASCENDING 
INDEX CUST-IDX-BY-ACTIVE-KEY  system-id  ASCENDING  active  ASCENDING  cust-key  ASCENDING 
INDEX CUST-IDX-BY-BILLING-COMPANY  system-id  ASCENDING  billing-customer-code  ASCENDING 
INDEX Cust-idx-by-code IS  UNIQUE  system-id  ASCENDING  cust-code  ASCENDING 
INDEX CUST-IDX-BY-CREDIT-COMPANY  system-id  ASCENDING  credit-customer-key  ASCENDING 
INDEX CUST-IDX-BY-HISTORY-COMPANY  system-id  ASCENDING  history-company  ASCENDING 
INDEX Cust-idx-by-key IS  UNIQUE  system-id  ASCENDING  cust-key  ASCENDING 
INDEX Cust-idx-by-name IS  PRIMARY  UNIQUE  system-id  ASCENDING  cust-name  ASCENDING  cust-code  ASCENDING 
INDEX Cust-idx-by-phone  system-id  ASCENDING  phone  ASCENDING  cust-name  ASCENDING 
INDEX Cust-idx-by-zip  system-id  ASCENDING  zip  ASCENDING  cust-name  ASCENDING 
INDEX CUST-NAME-ACTIVE  system-id  ASCENDING  cust-name  ASCENDING  active  ASCENDING 
INDEX cust-name-idx  cust-name  ASCENDING 
INDEX custname-by-group-id  system-id  ASCENDING  group-id  ASCENDING  cust-code  ASCENDING 
INDEX custname-by-sales-agt  system-id  ASCENDING  default-sales-agent  ASCENDING  cust-code  ASCENDING 
INDEX custname-by-terms  system-id  ASCENDING  default-terms  ASCENDING  cust-code  ASCENDING . 


DEFINE DATASET dscustname FOR ttcustname.




