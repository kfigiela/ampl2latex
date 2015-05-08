set PROVIDERS "P | Set of providers";                                          # Set of Cloud Providers
set VM_TYPES "V | Set of VM types";                                           # Set of VM Types
set PROVIDER_VM_TYPES "V^P | Set of VM types provided by provider $p$" {p in PROVIDERS} within VM_TYPES; # Indexed set of sets of vms for given provider
set COMPONENTS "C | Set of application Components, hardcoded for Scalarm" := {'cEM', 'cSM', 'cIS', 'cLB', 'cW'};   # Set of app components (needs to be here not in the data file, as some components are hardcoded in the constraints)

param provider_max_machines "n^{max} | Maximal number of instances at provider $p$" {p in PROVIDERS} >= 0 integer;   # Limit of machines per provider

# Upper and lower bounds of component instances
param component_max_instances "n^{max} | Maximal number of instances of component $c$" {c in COMPONENTS} > 0 integer;
param component_min_instances "n^{min} | Minimal number of instances of component $c$"{c in COMPONENTS} >= 0 integer;

# Price for running VM for one hour and parameters of VMs
param vm_price "p^{price} | Price of $vm$ per hour in Euros" {vm in VM_TYPES} > 0;
param vm_cores "p^{cores} | Number of CPU cores parameter of $vm$" {vm in VM_TYPES} > 0 integer;
param vm_ghz "p^{ghz} | CPU frequency parameter of $vm$" {vm in VM_TYPES} > 0;
param vm_ram "p^{ram} | RAM parameter of $vm$" {vm in VM_TYPES} > 0;
param vm_disk "p^{disk} | Disk parameter of $vm$" {vm in VM_TYPES} > 0;

# Component requirements
param component_min_cores "r^{cores} | Minimal required CPU cores for component $c$" {c in COMPONENTS} >= 0 integer;
param component_min_ghz "r^{ghz} | Minimal required CPU frequency for component $c$" {c in COMPONENTS} >= 0;
param component_min_ram "r^{ram} | Minimal required RAM for component $c$" {c in COMPONENTS} >= 0;
param component_min_disk "r^{disk} | Minimal required disk for component $c$" {c in COMPONENTS} >= 0;


param throughput "v | Throughput of $vm$ VM type (estimated or measured)" {vm in VM_TYPES} >= 0;                       # Estimated or measured throughput of Worker component on each VM type

param simulations_to_go "s^{to-go} | Number of simulations in queue" >= 0 integer;                   # Number of simulations to go
param current_runtime "t^{current} | Elapsed runtime" >= 0;                             # Elapsed runtime of simulation
param incured_cost "c^{inc} | Already incured cost" >= 0;                                # Already incured cost

param min_throughput "v^{min} | Minimal required throughput for constraint (optional)" >=0 ;                              # Minimal througput (set to 0 to disable)
param max_cost "c^{max} | Maximal cost for constraint (optional)" >= 0;                                    # Maximal cost (set to 0 to disable)

param max_workers_per_em "n^{WperEM} | Maximal number of Workers per Experiment Manager (on given cloud)" > 0 integer;                   # Maximal number of Workers per Experiment Manager

# Sets of vm types allowed per component with respect to component requirements i.e. CPU, etc.
set ALLOWED_VM_TYPES "V^{a} | Indexed set of VM types allowed for component that comply with component requirements" {c in COMPONENTS} := {vm in VM_TYPES: vm_ghz[vm] >= component_min_ghz[c] && vm_cores[vm] >= component_min_cores[c] && vm_disk[vm] >= component_min_disk[c] && vm_ram[vm] >= component_min_ram[c]};
set ALLOWED_PROVIDER_VM_TYPES "V^{A} | Indexed set of VM types provided that are allowed for component $c$ by provider $p$" {p in PROVIDERS, c in COMPONENTS} within ALLOWED_VM_TYPES[c] := {vm in PROVIDER_VM_TYPES[p]: vm_ghz[vm] >= component_min_ghz[c] && vm_cores[vm] >= component_min_cores[c] && vm_disk[vm] >= component_min_disk[c] && vm_ram[vm] >= component_min_ram[c]};

# Variable: how many instances of type $vm are allowed to run for component $c
var use "U" {c in COMPONENTS, vm in ALLOWED_VM_TYPES[c]} integer >= 0 <= component_max_instances[c];

# Objective 1: Maximize Throughput
maximize Throughput "Maximal throughput":
    sum {vm in ALLOWED_VM_TYPES['cW']} use['cW', vm] * throughput[vm];

# Objective 2: Minimize Cost
minimize Cost "Maximal cost":
    incured_cost + (sum {c in COMPONENTS, vm in ALLOWED_VM_TYPES[c]} vm_price[vm] * use[c, vm]) * (simulations_to_go / sum {vm in ALLOWED_VM_TYPES['cW']} use['cW', vm] * throughput[vm]);

# These two are there just for displaying their values after solution is produced.
maximize RunningCost "is for debugging": sum {c in COMPONENTS, vm in ALLOWED_VM_TYPES[c]} vm_price[vm] * use[c, vm];
minimize TotalRuntime "is for debugging": current_runtime + (simulations_to_go / sum {vm in ALLOWED_VM_TYPES['cW']} use['cW', vm] * throughput[vm]);

subject to

  upper_component_instance_limit " and \ref{constraint:lower_component_instance_limit} ensure number of component instances within limits. This constraint also covers that all components except \emph{Load Balancer} need to have at least one instance, and \emph{Storage Manager} and \emph{Information Service} exactly one – appropriate values are assigned to parameters \glssymbol{component_min_instances} and \glssymbol{component_max_instances}" {c in COMPONENTS}:
    (sum {vm in ALLOWED_VM_TYPES[c]} use[c,vm]) <= component_max_instances[c];

  lower_component_instance_limit {c in COMPONENTS}:
    (sum {vm in ALLOWED_VM_TYPES[c]} use[c,vm]) >= component_min_instances[c];

  provider_instance_limit "ensures provider instance limit" {p in PROVIDERS}:
    (sum {c in COMPONENTS, vm in ALLOWED_PROVIDER_VM_TYPES[p, c]} use[c, vm]) <= provider_max_machines[p];

  sm_is_on_same_provider "ensures that \emph{Storage Manager} and \emph{Information Service} are deployed on the same cloud" {p in PROVIDERS}:
    (sum {vm in ALLOWED_PROVIDER_VM_TYPES[p, 'cIS']} use['cIS', vm]) = (sum {vm in ALLOWED_PROVIDER_VM_TYPES[p, 'cSM']} use['cSM', vm]);

  max_workers_per_em_on_provider "ensures that one \emph{Experiment Manager} is deployed for \glssymbol{max_workers_per_em} \emph{Workers} (per provider)" {p in PROVIDERS}:
    max_workers_per_em*(sum {vm in ALLOWED_PROVIDER_VM_TYPES[p, 'cEM']} use['cEM', vm]) >= sum {vm in ALLOWED_PROVIDER_VM_TYPES[p, 'cW']} use['cW', vm];

  max_one_load_balancer_per_provider "ensures at most one \emph{Load Balancer} per cloud provider" {p in PROVIDERS}:
     sum {vm in ALLOWED_PROVIDER_VM_TYPES[p, 'cLB']} use['cLB', vm] <= 1;

  more_em_than_lb_on_provider "and \ref{constraint:lb_when_more_than_two_em_on_provider} ensure to deploy \emph{Load Balancer} when more than one \emph{Experiment Manager} is going to be deployed at particular provider. As I believe it requires a little bit more explanation: \begin{itemize}  \item constraint \ref{constraint:more_em_than_lb_on_provider} says that if there is \emph{Load Balancer} there need to be at least two \emph{Experiment Managers}, \item constraint \ref{constraint:lb_when_more_than_two_em_on_provider} says that if we have no \emph{Load Balancer} only one \emph{Experiment Manager} is allowed, otherwise we allow a max number of \emph{Experiment Managers} on that provider. \end{itemize}" {p in PROVIDERS}:
    sum {vm in ALLOWED_PROVIDER_VM_TYPES[p, 'cEM']} use['cEM', vm] >= 2*sum {vm in ALLOWED_PROVIDER_VM_TYPES[p, 'cLB']} use['cLB', vm];

  lb_when_more_than_two_em_on_provider {p in PROVIDERS}:
    (sum {vm in ALLOWED_PROVIDER_VM_TYPES[p, 'cEM']} use['cEM', vm]) - 1 <= (component_max_instances['cEM'] * sum {vm in ALLOWED_PROVIDER_VM_TYPES[p, 'cLB']} use['cLB', vm]);

  minimal_throughput "ensures that minimal throughput can be achieved (optional)": # optional
      sum {vm in ALLOWED_VM_TYPES['cW']} use['cW', vm] * throughput[vm] >= min_throughput;

  maximal_cost "ensures that maximal cost doesn't exceed limit (optional)": # optional
      incured_cost + (sum {c in COMPONENTS, vm in ALLOWED_VM_TYPES[c]} vm_price[vm] * use[c, vm]) * (simulations_to_go / sum {vm in ALLOWED_VM_TYPES['cW']} use['cW', vm] * throughput[vm]) <= max_cost;
