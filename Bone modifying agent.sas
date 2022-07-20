Libname intern "/home/u59232468/internship";

PROC IMPORT DATAFILE= "/home/u59232468/internship/Trial.xlsx"
	        DBMS=XLSX
	        OUT= INTERN.intern
	        replace ;
	        
	        GETNAMES=YES;
	        run; 

Data Intern.Intern;
     SET intern.intern;
     IF diagnosis_to_bone_modification = . THEN diagnosis_to_bone_modification = 0;
     IF bone_modifying_agent = . THEN bone_modifying_agent = 0;
     
	        
PROC CONTENTS DATA=intern.intern;
	        
PROC PRINT DATA = intern.intern;

Proc Format; 
Value yn
1 = ' 1'
0 = '0';

Value Race 
1 = ' WHITE'
2 = 'BLACK'
3= 'HISPANIC'
4= 'OTHERS';

Value smoking
0 = ' 0'
1= '1'
2='2';

Value insurance

1 = ' 1'
2 = '2'
3 = '3'
4 = '4';

Value disease

1 = ' 1'
2 = '2'
3 = '3';

Value bonevolume

0= ' 0'
1 = '1'
2 = '2';

value modificationagent

1 = ' 1'
2 = '2';

*Descriptive analysis;

PROC MEANS DATA = intern.intern MEAN STD MEDIAN ORDER=formatted;
           Format race race.;
PROC SORT data=intern.intern;
BY bone_modifying_therapy;
PROC MEANS DATA = INTERN.INTERN;
           BY bone_modifying_therapy;
           VAR Age;
           
   
Proc Ttest data = intern.intern;
class bone_modifying_therapy;
Var age;
PROC FREQ DATA=intern.intern ORDER=formatted;
          TABLES 	age*bone_modifying_therapy / CHISQ;
          Format race race. bone_modifying_therapy yn.;

PROC MEANS DATA = INTERN.INTERN;
           BY bone_modifying_therapy;
           VAR Follow_Up;
           
Proc Ttest data = intern.intern;
class bone_modifying_therapy;
Var Follow_Up;
           
PROC FREQ DATA=intern.intern ORDER=formatted;
          TABLES 	Follow_Up*bone_modifying_therapy / CHISQ;
          Format race race. bone_modifying_therapy yn.;          
           
PROC MEANS DATA = INTERN.INTERN;
           BY bone_modifying_therapy;
           VAR diagnosis_to_bone_modification;
Proc Ttest data = intern.intern;
class bone_modifying_therapy;
Var diagnosis_to_bone_modification;
PROC FREQ DATA=intern.intern ORDER=formatted;
          TABLES diagnosis_to_bone_modification*bone_modifying_therapy / CHISQ;
          Format race race. bone_modifying_therapy yn.;

PROC FREQ DATA = intern.INTERN order=formatted;
          TABLES SMOKING_STATUS*bone_modifying_therapy /cHIsq;        
          Format SMOKING_STATUS smoking. bone_modifying_therapy yn.;   
 
PROC FREQ DATA = intern.INTERN order = formatted;
          TABLES insurance*bone_modifying_therapy /chisq;        
          Format insurance insurance. bone_modifying_therapy yn.;
          
PROC FREQ DATA = intern.INTERN order = formatted;
          TABLES extent_of_disease*bone_modifying_therapy /chisq;       
          Format extent_of_disease disease. bone_modifying_therapy yn.; 
          
PROC FREQ DATA = intern.INTERN order =formatted;
          TABLES volume_of_bone_metastases*bone_modifying_therapy /cmh riskdiff relrisk;       
          Format volume_of_bone_metastases bonevolume. bone_modifying_therapy yn.; 
          
PROC FREQ DATA = intern.INTERN order =formatted;
          TABLES 	survival_status*bone_modifying_therapy /chisq;       
          Format survival_status yn. bone_modifying_therapy yn.;             

PROC FREQ DATA = intern.INTERN order =formatted;
          TABLES 	bone_modifying_agent*bone_modifying_therapy /chisq;       
          Format bone_modifying_agent modificationagent. bone_modifying_therapy yn.;             
        
*Bivariate analysis;

*Age;

PROC LOGISTIC DATA = intern.intern;
              MODEL bone_modifying_therapy = Age / EXPB;
              FORMAT  bone_modifying_therapy yn.;
              
*Follow up status;
                          
PROC LOGISTIC DATA = intern.intern;
              MODEL bone_modifying_therapy = Follow_Up / EXPB;
              FORMAT  bone_modifying_therapy yn.;

*Diagnosis time to bone modification;

PROC LOGISTIC DATA = intern.intern;
              MODEL bone_modifying_therapy = diagnosis_to_bone_modification / EXPB;
              FORMAT  bone_modifying_therapy yn.;
* race;
              
PROC LOGISTIC DATA = intern.intern descending ;
              CLASS race (ref = "1") / param= reference;
              MODEL bone_modifying_therapy = RACE;
              ESTIMATE "OR white vs black" Race 1 0 0 /EXP;
              ESTIMATE "OR white vs HISPANIC" Race 0 1 0 /EXP;
              ESTIMATE "OR white vs OTHERS" Race 0 0 1 /EXP;

*Smoking status;
                                               
PROC LOGISTIC DATA = intern.intern descending;
              CLASS SMOKING_STATUS (ref = "0") / param= ref;
              MODEL bone_modifying_therapy = SMOKING_STATUS;
              ESTIMATE "never versus former"  SMOKING_STATUS 1 0 0 / exp;
              ESTIMATE "never versus current" SMOKING_STATUS 0 1 0 / exp;
              ESTIMATE "never versus unknown" SMOKING_STATUS 0 0 1 / exp;

*Insurance;

PROC LOGISTIC DATA = intern.intern descending;
              Class insurance (ref = "1") / param= ref;
              MODEL bone_modifying_therapy = insurance;
              Estimate "medicare vs medicaid" insurance 1 0 0 / exp;
              Estimate "medicare vs private"  insurance 0 1 0 / exp;
              Estimate "medicare vs other"    insurance 1 0 0 / exp;

*Extent of Disease;
              
PROC LOGISTIC DATA = intern.intern descending;
              CLASS extent_of_disease (ref = "1") / param= ref;
              MODEL bone_modifying_therapy = extent_of_disease;
              ESTIMATE "localised vs metastatic"     extent_of_disease 1 0 / exp;
              ESTIMATE "localised vs regional nodes" extent_of_disease 0 1 / exp;

*Volume of bone metastases;
              
PROC LOGISTIC DATA = intern.intern DESC;
              CLASS volume_of_bone_metastases (REF="0") / PARAM = REFERENCE;
              MODEL bone_modifying_therapy = volume_of_bone_metastases;
              ESTIMATE "None vs low volume of bone metastasis"  volume_of_bone_metastases 1 0 / exp;
              ESTIMATE "None vs high volume of bone metastasis" volume_of_bone_metastases 0 1 / exp;

*Survival status;

PROC LOGISTIC DATA = intern.intern DESCENDING;
              CLASS survival_status (REF="0") / PARAM = REFERENCE;
              MODEL bone_modifying_therapy = survival_status;
              ESTIMATE "Alive vs Dead"  survival_status 1 / exp; 
              
*Bone modifying agents;              

PROC LOGISTIC DATA = intern.intern;
              CLASS bone_modifying_agent (REF="2") / PARAM = REFERENCE;
              MODEL bone_modifying_therapy = bone_modifying_agent;
              ESTIMATE "Denosumab vs zoledronic acid"  bone_modifying_agent 1 0 / exp;

PROC LOGISTIC DATA = intern.intern;
              *CLASS bone_modifying_agent (REF="2") / PARAM = REFERENCE;
              MODEL bone_modifying_therapy = allostatic_load /expb;
              *ESTIMATE "Denosumab vs zoledronic acid"  bone_modifying_agent 1 0 / exp;
*Race;
    
*NHW VS NHB BY BONE MODIFYING THERAPY ;
     
PROC FREQ DATA = INTERN.INTERN order=formatted;
          TABLES 	race*bone_modifying_therapy /cmh riskdiff relrisk chisq;       
          where race in (1,2);
          Format race race. bone_modifying_therapy yn.;
          
*NHW VS H BY BONE MODIFYING THERAPY ;	        

PROC FREQ DATA = intern.INTERN order=formatted;
          TABLES 	race*bone_modifying_therapy /cmh riskdiff relrisk;       
          where race in (1,3);	
          Format race race. bone_modifying_therapy yn.;

*NHW VS OTHERS BY BONE MODIFYING THERAPY ;	                 

PROC FREQ DATA = intern.INTERN order=formatted;
          TABLES race*bone_modifying_therapy /cmh riskdiff relrisk;       
          where race in (1,4); 
          Format race race. bone_modifying_therapy yn.;


*SMOKING STATUS;
          
*NEVER VS former BY BONE MODIFYING AGENT;

PROC FREQ DATA = intern.INTERN order=formatted;
          TABLES SMOKING_STATUS*bone_modifying_therapy /cmh riskdiff relrisk;       
          where SMOKING_STATUS in (0,1); 
          Format SMOKING_STATUS smoking. bone_modifying_therapy yn.;
        
*NEVER VS current BY BONE MODIFYING AGENT;
          
PROC FREQ DATA = intern.INTERN order=formatted;
          TABLES 	SMOKING_STATUS*bone_modifying_therapy /cmh riskdiff relrisk;       
          where SMOKING_STATUS in (0,2);  
          Format SMOKING_STATUS smoking. bone_modifying_therapy yn.;

*Insurance;
          
*Medicare vs medicaid BY BONE MODIFYING AGENT;

PROC FREQ DATA = intern.INTERN order = formatted;
          TABLES 	insurance*bone_modifying_therapy /cmh riskdiff relrisk;       
          where insurance in (1,2);  
          Format insurance insurance. bone_modifying_therapy yn.;
        
*PRIVATE vs medicaid BY BONE MODIFYING AGENT;

PROC FREQ DATA = intern.INTERN order= formatted;
          TABLES 	insurance*bone_modifying_therapy /cmh riskdiff relrisk;       
          where insurance in (1,3); 
          Format insurance insurance. bone_modifying_therapy yn.;
 
*Allostatic load by bone modifying agent;

PROC FREQ DATA = intern.INTERN order = formatted;
          TABLES 	allostatic_load*bone_modifying_therapy /cmh riskdiff relrisk;       
          Format allostatic_load yn. bone_modifying_therapy yn.;

*Extent of disease;
           
*Localised vs Metastatic by bone modifying agent;

PROC FREQ DATA = intern.INTERN order = formatted;
          TABLES 	extent_of_disease*bone_modifying_therapy /cmh riskdiff relrisk;       
          where extent_of_disease in (1,2); 
          Format extent_of_disease disease. bone_modifying_therapy yn.;

*Localised vs Regional nodes by bone modifying agent;

PROC FREQ DATA = intern.INTERN order = formatted;
          TABLES 	extent_of_disease*bone_modifying_therapy /cmh riskdiff relrisk;       
          where extent_of_disease in (1,3); 
          Format extent_of_disease disease. bone_modifying_therapy yn.;

*Volume of bone diseases;

*None vs low volume of bone metastases;
PROC FREQ DATA = intern.INTERN order =formatted;
          TABLES 	volume_of_bone_metastases*bone_modifying_therapy /cmh riskdiff relrisk;       
          where volume_of_bone_metastases in (0,1); 
          Format volume_of_bone_metastases bonevolume. bone_modifying_therapy yn.;

          
*None vs High volume of bone metastases;

PROC FREQ DATA = intern.INTERN order = formatted;
          TABLES 	volume_of_bone_metastases*bone_modifying_therapy /cmh riskdiff relrisk;       
          where volume_of_bone_metastases in (0,2); 
          Format volume_of_bone_metastases bonevolume. bone_modifying_therapy yn.;
          
PROC FREQ DATA = intern.INTERN order = formatted;
          TABLES 	bone_modifying_agent*diagnosis_to_bone_modification*bone_modifying_therapy /chisq cmh riskdiff relrisk;       
          *where volume_of_bone_metastases in (0,2); 
          Format volume_of_bone_metastases bonevolume. bone_modifying_therapy yn.;

*Final Model;

PROC LOGISTIC DATA = INTERN.INTERN descending;
              CLASS race (ref = "1") SMOKING_STATUS (ref = "0") insurance (ref = "1") extent_of_disease (ref = "1") volume_of_bone_metastases (REF="0") survival_status (REF="0") bone_modifying_agent (REF = "2")/param=reference;
              MODEL bone_modifying_therapy = Age Follow_Up race smoking_status insurance extent_of_disease volume_of_bone_metastases survival_status bone_modifying_agent diagnosis_to_bone_modification /expb;
              ESTIMATE "OR white vs black" Race 1 0 0                                         / exp;
              ESTIMATE "OR white vs black" Race 0 1 0                                         / exp;
              ESTIMATE "OR white vs black" Race 0 0 1                                         / exp;                               
              ESTIMATE "never versus former"  SMOKING_STATUS 1 0 0                            / exp;
              ESTIMATE "never versus current" SMOKING_STATUS 0 1 0                            / exp;
              ESTIMATE "never versus unknown" SMOKING_STATUS 0 0 1                            / exp;
              Estimate "medicare vs medicaid" insurance 1 0 0                                 / exp;
              Estimate "medicare vs private"  insurance 0 1 0                                 / exp;
              Estimate "medicare vs other"    insurance 1 0 0                                 / exp;
              ESTIMATE "localised vs metastatic"     extent_of_disease 1 0                    / exp;
              ESTIMATE "localised vs regional nodes" extent_of_disease 0 1                    / exp;
              ESTIMATE "None vs low volume of bone metastasis"  volume_of_bone_metastases 1 0 / exp;
              ESTIMATE "None vs high volume of bone metastasis" volume_of_bone_metastases 0 1 / exp;
              ESTIMATE "Alive vs Dead"  survival_status 1                                     / exp; 
              ESTIMATE "Denosumab vs zoledronic acid"  bone_modifying_agent 1 0 / exp;

PROC LOGISTIC DATA = INTERN.INTERN descending;
              CLASS race (ref = "1") SMOKING_STATUS (ref = "0") insurance (ref = "1") extent_of_disease (ref = "1") volume_of_bone_metastases (REF="0") survival_status (REF="0") bone_modifying_agent (REF = "2")/param=reference;
              MODEL bone_modifying_therapy = Age Follow_Up race smoking_status insurance extent_of_disease volume_of_bone_metastases survival_status bone_modifying_agent diagnosis_to_bone_modification /selection=backward;
              ESTIMATE "OR white vs black" Race 1 0 0                                         / exp;
              ESTIMATE "OR white vs black" Race 0 1 0                                         / exp;
              ESTIMATE "OR white vs black" Race 0 0 1                                         / exp;                               
              ESTIMATE "never versus former"  SMOKING_STATUS 1 0 0                            / exp;
              ESTIMATE "never versus current" SMOKING_STATUS 0 1 0                            / exp;
              ESTIMATE "never versus unknown" SMOKING_STATUS 0 0 1                            / exp;
              Estimate "medicare vs medicaid" insurance 1 0 0                                 / exp;
              Estimate "medicare vs private"  insurance 0 1 0                                 / exp;
              Estimate "medicare vs other"    insurance 1 0 0                                 / exp;
              ESTIMATE "localised vs metastatic"     extent_of_disease 1 0                    / exp;
              ESTIMATE "localised vs regional nodes" extent_of_disease 0 1                    / exp;
              ESTIMATE "None vs low volume of bone metastasis"  volume_of_bone_metastases 1 0 / exp;
              ESTIMATE "None vs high volume of bone metastasis" volume_of_bone_metastases 0 1 / exp;
              ESTIMATE "Alive vs Dead"  survival_status 1                                     / exp; 
              ESTIMATE "Denosumab vs zoledronic acid"  bone_modifying_agent 1 0 / exp;

PROC LOGISTIC DATA = INTERN.INTERN descending;
              CLASS race (ref = "1") SMOKING_STATUS (ref = "0") insurance (ref = "1") extent_of_disease (ref = "1") volume_of_bone_metastases (REF="0") survival_status (REF="0") bone_modifying_agent (REF = "2")/param=reference;
              MODEL bone_modifying_therapy = Age Follow_Up race smoking_status insurance extent_of_disease volume_of_bone_metastases survival_status bone_modifying_agent allostatic_load diagnosis_to_bone_modification /selection=backward;
              ESTIMATE "OR white vs black" Race 1 0 0                                         / exp;
              ESTIMATE "OR white vs black" Race 0 1 0                                         / exp;
              ESTIMATE "OR white vs black" Race 0 0 1                                         / exp;                               
              ESTIMATE "never versus former"  SMOKING_STATUS 1 0 0                            / exp;
              ESTIMATE "never versus current" SMOKING_STATUS 0 1 0                            / exp;
              ESTIMATE "never versus unknown" SMOKING_STATUS 0 0 1                            / exp;
              Estimate "medicare vs medicaid" insurance 1 0 0                                 / exp;
              Estimate "medicare vs private"  insurance 0 1 0                                 / exp;
              Estimate "medicare vs other"    insurance 1 0 0                                 / exp;
              ESTIMATE "localised vs metastatic"     extent_of_disease 1 0                    / exp;
              ESTIMATE "localised vs regional nodes" extent_of_disease 0 1                    / exp;
              ESTIMATE "None vs low volume of bone metastasis"  volume_of_bone_metastases 1 0 / exp;
              ESTIMATE "None vs high volume of bone metastasis" volume_of_bone_metastases 0 1 / exp;
              ESTIMATE "Alive vs Dead"  survival_status 1                                     / exp; 
              ESTIMATE "Denosumab vs zoledronic acid"  bone_modifying_agent 1 0 / exp;

PROC LOGISTIC DATA = INTERN.INTERN descending;
              CLASS race (ref = "1") SMOKING_STATUS (ref = "0") insurance (ref = "1") extent_of_disease (ref = "1") volume_of_bone_metastases (REF="0") survival_status (REF="0") bone_modifying_agent (REF = "2")/param=reference;
              MODEL bone_modifying_therapy = Age Follow_Up race allostatic_load smoking_status insurance extent_of_disease volume_of_bone_metastases survival_status bone_modifying_agent diagnosis_to_bone_modification /expb;
              ESTIMATE "OR white vs black" Race 1 0 0                                         / exp;
              ESTIMATE "OR white vs black" Race 0 1 0                                         / exp;
              ESTIMATE "OR white vs black" Race 0 0 1                                         / exp;                               
              ESTIMATE "never versus former"  SMOKING_STATUS 1 0 0                            / exp;
              ESTIMATE "never versus current" SMOKING_STATUS 0 1 0                            / exp;
              ESTIMATE "never versus unknown" SMOKING_STATUS 0 0 1                            / exp;
              Estimate "medicare vs medicaid" insurance 1 0 0                                 / exp;
              Estimate "medicare vs private"  insurance 0 1 0                                 / exp;
              Estimate "medicare vs other"    insurance 1 0 0                                 / exp;
              ESTIMATE "localised vs metastatic"     extent_of_disease 1 0                    / exp;
              ESTIMATE "localised vs regional nodes" extent_of_disease 0 1                    / exp;
              ESTIMATE "None vs low volume of bone metastasis"  volume_of_bone_metastases 1 0 / exp;
              ESTIMATE "None vs high volume of bone metastasis" volume_of_bone_metastases 0 1 / exp;
              ESTIMATE "Alive vs Dead"  survival_status 1                                     / exp; 
              ESTIMATE "Denosumab vs zoledronic acid"  bone_modifying_agent 1 0 / exp;




 