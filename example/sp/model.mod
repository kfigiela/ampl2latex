# vim: syntax=ampl
# based on model presented in 
# Maciej Malawski, Kamil Figiela, Marian Bubak, Ewa Deelman, and Jarek Nabrzyski, Scheduling Multilevel Deadline-Constrained Scientific Workflows on Clouds Based on Cost Optimization, Scientific Programming, vol. 2015, Article ID 680271, 13 pages, 2015.
# doi:10.1155/2015/680271.
# http://www.hindawi.com/journals/sp/2015/680271/

set INSTANCE "I | set of instance types";
set STORAGE "S | set of available cloud storage sites";
set PROVIDER "P | set of possible computing cloud providers";
set PROVIDER_INSTANCES "PI | set of instances that belong to provider" {p in PROVIDER} within INSTANCE;
set STORAGE_LOCAL "LS | set of compute cloud providers that are local to the storage platform $s$" {s in STORAGE} within PROVIDER; 

set LAYER "L | a set of levels the workflow is divided into";
set TASK "G | a set of task groups, tasks in a groups have the same computational cost and input/output size";
set LAYER_TASK "L^{L} | a set of task groups belonging to a level $l$" {l in LAYER} within TASK;

param provider_max_machines "n^{Pmax} | upper limit of number of instances allowed by a cloud provider $p$" {p in PROVIDER} >= 0 integer;       # VM limit per provider (Amazon has default limit of 20 machnies)
param instance_max_machines "n^{Imax} | upper limit of number of instances allowed by a cloud provider of instance $i$" {i in INSTANCE} > 0 integer;        # copy of previous, for easier access

param instance_price "p^I | a fee (in US dollars) for running the instance of type $i$ for one hour" {INSTANCE} >= 0;                      # price per instance hour
param ccu "ccu^{I} | performance of instance of type $i$ in CloudHarmony Compute Units (CCU)" {INSTANCE} >= 0;                                 # CloudHarmony Compute Units, similar to Amazon ECU, results of benchmarks from http://blog.cloudharmony.com/2010/05/what-is-ecu-cpu-benchmarking-in-cloud.html
                                               
param request_price "p^{R} | price per task for a queuing service, such as Amazon SQS" >= 0;                                  # price per data request
param instance_transfer_price_out "p^{Iout} | price in dollars per MiB for non-local data transfers" {INSTANCE} >= 0;         # price per MB of data transfer outbound             
param instance_transfer_price_in "p^{Iin} | price in dollars per MiB for non-local data transfers"  {INSTANCE} >= 0;         # price per MB of data transfer inbound
param storage_transfer_price_out "p^{Sout} | price in dollars per MiB for non-local data transfers"  {STORAGE}  >= 0;         # price per MB of data transfer outbound             
param storage_transfer_price_in "p^{Sin} | price in dollars per MiB for non-local data transfers"  {STORAGE}  >= 0;         # price per MB of data transfer inbound
                                               
param local "l | matrix showing which transfers are local (0) and which non-local (1)" {i in INSTANCE, s in STORAGE} binary >= 0;               # matrix showing which transfers are local (0) and which non-local (1)
param transfer_rate "r | data transfer rates between a given storage site $s$ and instance $i$ in MiB per second" {i in INSTANCE, s in STORAGE} >= 0;
                                               
param exec_time "t^x | execution time in hours of a single task in a group $g$ on a machine with the processor performance of 1 CloudHarmony Compute Unit (CCU)" {g in TASK} >= 0;                               # average task execution time on m1.small instance in hours                                              
param task_count "A^{tot} | number of tasks in a group $g$" {g in TASK} > 0 integer;                       # total number of tasks
param data_size_in "d^{in} | data size for input of a task in group $g$" {g in TASK} >= 0;                            # data size per task
param data_size_out "d^{out} | data size for output of a task in group $g$" {g in TASK} >= 0;                           # data size per task
param workflow_deadline "t^D | total time allowed for completing workflow (deadline)" > 0 integer;                       # maximum duration of computing
param storage "s | a selected storage site" symbolic;                                    # which storage is used

# precalculated parameters
param transfer_time "t^{net} | {\em transfer time} in hours, i.e. time for data transfer between instances of type $i$ and storage site $s$ for a task in task group $g$" {t in TASK, i in INSTANCE, s in STORAGE} := (data_size_in[t]+data_size_out[t])/(transfer_rate[i,s] * 3600);
param unit_time     "t^u | time in hours for processing a task in group $g$ on instance of type $i$ using storage site $s$" {t in TASK, i in INSTANCE, s in STORAGE} := max(exec_time[t] / ccu[i], transfer_time[t,i,storage]); # time required for task to compute on instance i
param transfer_cost "c^T | a cost of data transfer between an instance of type $i$ and a storage site $s$ when processing task in group $g$" {t in TASK, i in INSTANCE, s in STORAGE} := (data_size_out[t] * (instance_transfer_price_out[i]+storage_transfer_price_in[s]) + data_size_in[t] * (storage_transfer_price_out[s]+instance_transfer_price_in[i])) * local[i,s];


var InstanceActive "N | $1$ iff (if and only if) instance of type $i$ with index $k \in \instanceSet_i$ is launched to process task group $g$, otherwise $0$ (binary)" {t in TASK, i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)} binary;
var InstanceHours  "H | for how many hours the instance of index $k$ is launched (integer)" {t in TASK, i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)} integer >= 0 <= workflow_deadline;
var InstanceTasks  "T | how many tasks of $g$ are processed on that instance (integer)" {t in TASK, i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)} integer >= 0 <= task_count[t];
var LayerDeadline  "D | actual computation time for level $l$ (real)" {l in LAYER}                                                           integer >= 1 <= workflow_deadline;
var LayerTime      "D^t | maximal number of hours (deadline) that instances are allowed to run at level $l$ (integer)" {l in LAYER}                                                                   >= 0 <= workflow_deadline;

minimize TotalCost "is total cost of executing workflow": 
  sum { t in TASK, i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1) } (instance_price[i] * InstanceHours[t,i,idx] + (request_price + transfer_cost[t,i,storage])*InstanceTasks[t,i,idx]);

subject to
  keep_layer_deadlines_sum_under_workflow_deadline "ensures that workflow finishes in given deadline": 
    sum { l in LAYER} LayerTime[l] <= workflow_deadline;
  keep_layer_time {l in LAYER} "and \ref{constraint:keep_layer_time_2} bind that $\glssymbol{LayerDeadline} = \ceil{\glsSymbol{LayerTime}}":
    LayerTime[l] <= LayerDeadline[l];

  keep_layer_time_2 {l in LAYER}:
    LayerDeadline[l] <= LayerTime[l] + 1;
  
  bind_instance_active_with_instance_hours_1 "and \ref{constraint:bind_instance_active_with_instance_hours_2} ensure that $\glssymbol{InstanceHours}$ may be allocated only iif $\glssymbol{InstanceActive}$ is $1$" {t in TASK, i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)}: 
    InstanceHours[t,i,idx] >= InstanceActive[t,i,idx];
  bind_instance_active_with_instance_hours_2 {t in TASK, i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)}: 
    InstanceHours[t,i,idx] <= workflow_deadline*InstanceActive[t,i,idx];
  
  bind_instance_active_with_instance_tasks_1  "and \ref{constraint:bind_instance_active_with_instance_tasks_2} ensure that $\glssymbol{InstanceTasks}$ may be allocated only iif $\glssymbol{InstanceActive}$ is $1$"{t in TASK, i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)}: 
    InstanceTasks[t,i,idx] >= InstanceActive[t,i,idx];
  bind_instance_active_with_instance_tasks_2 {t in TASK, i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)}: 
    InstanceTasks[t,i,idx] <= task_count[t] * InstanceActive[t,i,idx];

  keep_layer_deadline "enforces level deadline on instances runtime" {l in LAYER, t in LAYER_TASK[l], i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)}:
    InstanceHours[t,i,idx] <= LayerDeadline[l];

  keep_layer_deadline_2 "enforces level finishes work in $\glssymbol{LayerTime}$" {l in LAYER, t in LAYER_TASK[l], i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)}:
    InstanceTasks[t,i,idx]*unit_time[t,i,storage] <= LayerTime[l];
 
  is_there_enough_processing_power_to_do_the_tasks "adjust $\glssymbol{InstanceHours}$ respectively to $\glssymbol{InstanceTasks}$" {t in TASK, i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)}:
    InstanceHours[t,i,idx] >= InstanceTasks[t,i,idx]*unit_time[t,i,storage];
  but_not_more {t in TASK, i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)}:
    InstanceHours[t,i,idx] <= InstanceTasks[t,i,idx]*unit_time[t,i,storage] + 1;

  enough_power "ensures that all tasks are processed" {t in TASK}:
    sum {i in INSTANCE, idx in 0 .. (instance_max_machines[i] - 1)} InstanceTasks[t,i,idx] = task_count[t];
   
  discard_symmetric_solutions_1 "to \ref{constraint:discard_symmetric_solutions_3} reject symmetric solutions"{t in TASK, i in INSTANCE, idx in 1 .. (instance_max_machines[i] - 1)}:
    InstanceHours[t,i,idx] <= InstanceHours[t,i,idx-1]; 
  discard_symmetric_solutions_2 {t in TASK, i in INSTANCE, idx in 1 .. (instance_max_machines[i] - 1)}:
    InstanceActive[t,i,idx] <= InstanceActive[t,i,idx-1];
  discard_symmetric_solutions_3 {t in TASK, i in INSTANCE, idx in 1 .. (instance_max_machines[i] - 1)}:
    InstanceTasks[t,i,idx] <= InstanceTasks[t,i,idx-1];

  force_provider_instance_limit "enforces providers' instance limits" {l in LAYER, p in PROVIDER}: 
    sum {i in PROVIDER_INSTANCES[p], t in LAYER_TASK[l], idx in 0 .. (instance_max_machines[i] - 1)} InstanceActive[t,i,idx] <= provider_max_machines[p];


       
