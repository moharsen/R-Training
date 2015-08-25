%macro excel_data();


/* TREE DESIGN */

%include "/prod/user/sam/ent/crm2/mroblr/non_npi/PROJECTS/INGD/LEARNING/Tool_phase_1/node_extr.sas";

/* This creates a table named tree design in with 3 columns */
/* Node - Terminal Node No. */
/* Event - No. of Events in the terminal node */
/* Obs - No. of Observations in the terminal node */
proc sql;
create table tree_design_in as
select cat("TerminalNode",node) as Node, sum(&target_pdf.) as event, count(*) as obs
from out_cart.cartout
group by 1;
quit;

/* This basically build on the previous code block */
/* Node - Terminal Node No. */
/* Event - No. of Events in the terminal node */
/* Obs - No. of Observations in the terminal node */
proc sql;
create table tree_design as
select a.Node, a.Parent, a.Spliting_rules as Split, b.event as events, b.obs /*Deleted child column, renamed event as events, Splitting_rules as split */
from node_data_temp a left join tree_design_in b
on a.Node = b.Node;
quit;

proc export data=tree_design
    outfile="&out_cart/tree_design.xlsx"
    dbms=xlsx
    replace;
run; 

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/* NODE PROFILE */

data orig_scored_out;
set out_data.&dataset._modify_o;
%include "&out_cart./CART_OUT/&target_pdf..sas";
run;

data orig_scored_in;
set out_data.&dataset._modify_i;
%include "&out_cart./CART_OUT/&target_pdf..sas";
run;

proc contents data = orig_scored_out out =out_cart.contents_new;
run;

proc sql noprint ;
select name into :varlist_new separated by ' ' from out_cart.contents_new where type = 1 and format not in ('DATETIME') and name not in  ('node', '&target_pdf.');
quit;

proc sort data =orig_scored_out; by node; run; 
proc sort data =orig_scored_in; by node; run;

proc means  StackODSOutput data = orig_scored_out  min p25 median p75 max mean;
var &varlist_new.;
by node build_flag;
ods output summary=node_dist_out;
run;

proc means  StackODSOutput data = orig_scored_in  min p25 median p75 max mean;
var &varlist_new.;
by node build_flag;
ods output summary=node_dist_in;
run;

data node_dist;
set node_dist_in node_dist_out;
run;

/*
proc sql;
create table node_dist_final as
select a.*, b.cart_ind as cart_ind
from node_dist a left outer join node_vars b on
"a.Variable" = "b.cart_vars";
quit;
*/

proc sort data = node_dist;
by Variable;
run;

data node_dist_final;
merge node_dist(in=a) node_vars(rename=(cart_vars=Variable));
by Variable;
if a;
run;

proc export data=node_dist_final
    outfile="&out_cart/node_dist.csv"
    dbms=csv
    replace;

run;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/* ACCURACY TESTS */

%include "/prod/user/sam/ent/crm2/mroblr/non_npi/PROJECTS/INGD/LEARNING/Tool_phase_1/Subs_new.sas";


proc sql;
select ar into : avg_ar_in from roc_ar0_in;
quit;

proc sql;
select ar into : avg_ar_out from roc_ar0_out;
quit;

proc sql;
select areaROC into : avg_roc_in from roc_ar0_in;
quit;

proc sql;
select areaROC into : avg_roc_out from roc_ar0_out;
quit;

data roc_ar_in_exc;
set roc_ar_in (keep = snapshot_date_internal AreaROC AR);
AR_avg = &avg_ar_in;
Area_ROC_avg = &avg_roc_in;
build_flag = 1;
run;

data roc_ar_out_exc;
set roc_ar_out (keep = snapshot_date_internal AreaROC AR);
AR_avg = &avg_ar_out;
Area_ROC_avg = &avg_roc_out;
build_flag = 0;
run;

data roc_ar_exc (rename=(snapshot_date_internal = obs_time_field));
set roc_ar_in_exc roc_ar_out_exc;
run;

/*
proc sql;
create table ROC_AR_final as
select  case when month(snapshot_date_internal)<10 then cat(year(snapshot_date_internal),'0',month(snapshot_date_internal)) else 
cat(year(snapshot_date_internal),month(snapshot_date_internal)) end as obs_time_field
 , AR, AreaROC, build_flag
from roc_ar_exc;
run;
*/

proc sql;
create table ROC_AR_final as
select * from roc_ar_exc;
quit;

data roc_ar_final;
set roc_ar_final;
run;

proc export data=roc_ar_final
    outfile="&out_cart/roc_ar.xlsx"
    dbms=xlsx
    replace;
run;

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

data discrimstat0_in_exc;
set discrimstat0_in;
build_flag = 1;
run;

data discrimstat0_out_exc;
set discrimstat0_out;
build_flag = 0;
run;

data discrimstat0_exc;
set discrimstat0_in_exc discrimstat0_out_exc;
run;

proc sql;
select max(node) into : node_remove from discrimstat0_exc;
quit;

data discrimstat0_exc;
set discrimstat0_exc;
if node = &node_remove. then node = .;
run;

proc export data=discrimstat0_exc
    outfile="&out_cart/discrimstat0.xlsx"
    dbms=xlsx
    replace;
run;

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

proc sql;
select max(KS) into : KS_in from KS_out_in;
quit;

proc sql;
select max(KS) into : KS_out from KS_out_out;
quit;

data KS_out_in_exc;
set KS_out_in;
build_flag = 1;
max_KS = &KS_in;
run;

data KS_out_out_exc;
set KS_out_out;
build_flag = 0;
max_KS = &KS_out;
run;

data KS_out_exc;
set KS_out_in_exc KS_out_out_exc;
run;

/*
proc sql;
create table KS_out_final as
select  case when month(snapshot_date_internal)<10 then cat(year(snapshot_date_internal),'0',month(snapshot_date_internal)) else 
cat(year(snapshot_date_internal),month(snapshot_date_internal)) end as obs_time_field
 , KS, build_flag
from KS_out_exc;
run;
*/

proc sql;
create table KS_out_final as
select * from KS_out_exc;
quit;

data KS_out_final (rename=(snapshot_date_internal = obs_time_field));
set KS_out_final;
run;

proc export data=KS_out_final
    outfile="&out_cart/KS_out.xlsx"
    dbms=xlsx
    replace;
run;

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

%include "/prod/user/sam/ent/crm2/mroblr/non_npi/PROJECTS/INGD/LEARNING/Tool_phase_1/Homogen.sas";


data cldiffs1_in_exc;
set cldiffs1_in;
build_flag = 1;
run;

data cldiffs1_out_exc;
set cldiffs1_out;
build_flag = 0;
run;

data cldiffs1_exc;
set cldiffs1_in_exc cldiffs1_out_exc;
run;

proc export data=cldiffs1_exc
    outfile="&out_cart/Cldiffs1_exc.xlsx"
    dbms=xlsx
    replace;
run;

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

data mt_in_exc;
set mt_in;
build_flag = 1;
run;

data mt_out_exc;
set mt_out;
build_flag = 0;
run;

data mt_exc;
set mt_in_exc mt_out_exc;
run;

proc export data=mt_exc
    outfile="&out_cart/Mt_exc.xlsx"
    dbms=xlsx
    replace;
run;


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */



/* SUMMARY NODE */

proc sql;
create table summary_node_in as
select &obs_date_field. as obs_time_field, &target_pdf. as target_variable, node, build_flag
from out_data.&dataset._modify_i;
quit;

proc sql;
create table summary_node_out as
select &obs_date_field. as obs_time_field, &target_pdf. as target_variable, node, build_flag
from out_data.&dataset._modify_o;
quit;

data summary_node;
set summary_node_in summary_node_out;
format obs_time_field DDMMYY10.;
run;

/*
data summary_node;
set summary_node_in summary_node_out;
format obs_time_field yymmn6.;
run;
*/


proc export data=summary_node
    outfile="&out_cart/summary_node.csv"
    dbms=csv
    replace;
run;

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */



%mend;

%excel_data();
