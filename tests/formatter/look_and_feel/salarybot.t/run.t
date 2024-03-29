  $ scilla-fmt salarybot.scilla
  scilla_version 0
  
  import ListUtils
  
  (***************************************************)
  (*               Associated library                *)
  (***************************************************)
  library SalaryBotLib
  
  let mk_employee_already_exists_event =
    fun (emp_addr : ByStr20) =>
      { _eventname : "Employee already exists"; employee_address : emp_addr }
  
  let mk_employee_added_event =
    fun (emp_addr : ByStr20) =>
      { _eventname : "Employee added"; employee_address : emp_addr }
  
  let mk_employee_non_existent_event =
    fun (emp_addr : ByStr20) =>
      { _eventname : "Employee does not exist"; employee_address : emp_addr }
  
  let mk_employee_removed_event =
    fun (emp_addr : ByStr20) =>
      { _eventname : "Employee removed"; employee_address : emp_addr }
  
  
  contract SalaryBot (owner : ByStr20)
  
  
  field employees : Map ByStr20 Uint128 = Emp (ByStr20) (Uint128)
  
  procedure validate_owner ()
    is_owner = builtin eq owner _sender;
    match is_owner with
    | True =>
    | False =>
      e = { _exception : "Not owner exeption" };
      throw e
    end
  end
  
  transition add_funds ()
    validate_owner;
    accept
  end
  
  transition add_employee (emp_addr : ByStr20, salary : Uint128)
    validate_owner;
    emp_exists <- exists employees[emp_addr];
    match emp_exists with
    | True =>
      e = mk_employee_already_exists_event emp_addr;
      event e
    | False =>
      employees[emp_addr] := salary;
      e = mk_employee_added_event emp_addr;
      event e
    end
  end
  
  transition remove_employee (emp_addr : ByStr20)
    validate_owner;
    emp_exists <- exists employees[emp_addr];
    match emp_exists with
    | True =>
      delete employees[emp_addr];
      e = mk_employee_removed_event emp_addr;
      event e
    | False =>
      e = mk_employee_non_existent_event emp_addr;
      event e
    end
  end
  
  transition pay_all_salaries ()
    validate_owner;
    employees_local <- employees;
    emp_sal_pairs = builtin to_list employees_local;
    mapper = @list_map (Pair ByStr20 Uint128) (Message);
    mapf =
      fun (empsal : Pair ByStr20 Uint128) =>
        match empsal with
        | Pair emp_addr sal => { _tag : ""; _recipient : emp_addr; _amount : sal }
        end;
    messages = mapper mapf emp_sal_pairs;
    send messages
  end
  
