define print_callee_save_registers
    printf "rbx = %u, rbp = %u, r12 = %u, r13 = %u, r14 = %u, r15 = %u.\n", $rbx, $rbp, $r12, $r13, $r14, $r15
end

define set_callee_save_registers
    set $rbx = $arg0
    set $r12 = $arg1
    set $r13 = $arg2
    set $r14 = $arg3
    set $r15 = $arg4
end

define print_stack_alignment
    printf "Stack alignment: %u.\n", (unsigned long long)$rsp % 16
end
