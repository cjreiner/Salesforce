#### Salesforce to BiqQuery Data Uploader ####
#### This code provides a process to export data from Saleforce into R, 
#### then load it into BigQuery. It does a total replace if the table is
#### already in BigQuery.

#Import Dependencies
library(salesforcer)
library(bigrquery)

#### LOGIN ####

#BigQuery OAuth Login
bq_auth()
#Salesforce OAuth Login
sf_auth()
sf_user_info()$userFullName #will return data

#### SETUP ####

#### Function to input sf data and load to bq ####
i_project_name <- 'your-bq-project-name' #Enter the BigQuery Project name
i_dataset <- 'salesforce' #enter the dataset (aka schema) name to place the upload
sf_to_bq <- function(data,i_project_name,i_dataset,sf_object){
  
  #Adjust column names to BQ standards (. is not allowed, so we convert to _)
  col_list <- colnames(data)
  colnames(data) <- gsub("\\.","_",col_list)
  
  #Create BQ references/objects and upload to BQ
  t <- bq_table(i_project_name,i_dataset,table = sf_object) #define BQ table
  if(bq_table_exists(t)){
    bq_table_delete(t) #delete prior version of the table
  } #end if
  bq_new_table <- bq_table_create(t,data) #create table uisng bq_table and dataframe as the design
  bq_table_upload(bq_new_table,data) #upload the actual data to BQ
  
  #clean up
  rm(data,t,bq_new_table,col_list,sf_object)
} #end function


########################################
####   ENTER SOQL AND TABLES HERE   ####
########################################

####Table of Contents
#1. Account Example


#### 1. Account Example ####
#Query Salesforce
sf_object <- "Account"
soql <- ("Select Id, 
          Name
          FROM Account
          ")
rm(data) # clear data from prior run. useful when running more than 1 upload
data <- sf_query(soql,object_name=sf_object,api_type = "Bulk 2.0", guess_types=F)
sf_to_bq(data,i_project_name,i_dataset,sf_object)


#### END CLEAN ANY OUTSTANDING DATA ####
rm(sf_to_bq,data,bq_table_name,sf_object,soql,i_dataset,i_project_name)
gc()