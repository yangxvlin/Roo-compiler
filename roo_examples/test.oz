    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 29
# load parameters
# initialise variables
    int_const r0, 0
    store 0, r0
    store 1, r0
    store 2, r0
    store 3, r0
    store 4, r0
    store 5, r0
    store 6, r0
    store 7, r0
    store 8, r0
    store 9, r0
    store 10, r0
    store 11, r0
    store 12, r0
    store 13, r0
    store 14, r0
    store 15, r0
    store 16, r0
    store 17, r0
    store 18, r0
    store 19, r0
    store 20, r0
    store 21, r0
    store 22, r0
    store 23, r0
    store 24, r0
    store 25, r0
    store 26, r0
    store 27, r0
    store 28, r0
# LId "n" <- IntConst 23
    int_const r0, 23
    load_address r1, 0
    store_indirect r1, r0
# Call "proc"
    int_const r0, 0
    load_address r1, 0
    load_address r2, 2
    load_address r3, 5
    call proc_proc
# Writeln Op_and (Op_and (Lval (LBrackets "b" (IntConst 0))) (Lval (LBrackets "b" (IntConst 1)))) (Lval (LBrackets "b" (IntConst 2)))
    int_const r0, 0
    load_address r1, 2
    int_const r2, 1
    mul_int, r0, r0, r2
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 1
    load_address r2, 2
    int_const r3, 1
    mul_int, r1, r1, r3
    sub_offset r1, r2, r1
    load_indirect r1, r1
    and r0, r0, r1
    int_const r1, 2
    load_address r2, 2
    int_const r3, 1
    mul_int, r1, r1, r3
    sub_offset r1, r2, r1
    load_indirect r1, r1
    and r0, r0, r1
    call_builtin print_bool
    call_builtin print_newline
# LId "result" <- IntConst 0
    int_const r0, 0
    load_address r1, 1
    store_indirect r1, r0
# While Op_large_eq (Lval (LId "n")) (IntConst 0)
label_0:
    load_address r0, 0
    load_indirect r0, r0
    int_const r1, 0
    cmp_geint, r0, r0, r1
    branch_on_False, r0, label_1
# do
# LId "result" <- Op_add (Lval (LId "result")) (Lval (LBrackets "d" (Lval (LId "n"))))
    load_address r0, 1
    load_indirect r0, r0
    load_address r1, 0
    load_indirect r1, r1
    load_address r2, 5
    int_const r3, 1
    mul_int, r1, r1, r3
    sub_offset r1, r2, r1
    load_indirect r1, r1
    add_int, r0, r0, r1
    load_address r1, 1
    store_indirect r1, r0
# LId "n" <- Op_sub (Lval (LId "n")) (IntConst 1)
    load_address r0, 0
    load_indirect r0, r0
    int_const r1, 1
    sub_int, r0, r0, r1
    load_address r1, 0
    store_indirect r1, r0
    branch_uncond "label_0"
# od
label_1:
# Writeln Lval (LId "result")
    load_address r0, 1
    load_indirect r0, r0
    call_builtin print_int
    call_builtin print_newline
# epilogue
    pop_stack_frame 29
    return
proc_proc:
# prologue
    push_stack_frame 5
# load parameters
    store 0, r0
    store 1, r1
    store 2, r2
    store 3, r3
# initialise variables
    int_const r0, 0
    store 4, r0
# "i" <- "n"
    load r0, 1
    load_indirect r0, r0
    load_address r1, 4
    store_indirect r1, r0
# While Op_large_eq (Lval (LId "i")) (IntConst 0)
label_2:
    load_address r0, 4
    load_indirect r0, r0
    int_const r1, 0
    cmp_geint, r0, r0, r1
    branch_on_False, r0, label_3
# do
# LBrackets "i_arr" (Lval (LId "i")) <- Op_sub (Lval (LId "n")) (Lval (LId "i"))
    load r0, 1
    load_indirect r0, r0
    load_address r1, 4
    load_indirect r1, r1
    sub_int, r0, r0, r1
    load_address r1, 4
    load_indirect r1, r1
    load r2, 3
    int_const r3, 1
    mul_int, r1, r1, r3
    sub_offset r1, r2, r1
    store_indirect r1, r0
# LId "i" <- Op_sub (Lval (LId "i")) (IntConst 1)
    load_address r0, 4
    load_indirect r0, r0
    int_const r1, 1
    sub_int, r0, r0, r1
    load_address r1, 4
    store_indirect r1, r0
    branch_uncond "label_2"
# od
label_3:
# if Lval (LId "check")
    load_address r0, 0
    load_indirect r0, r0
    branch_on_False, r0, label_5
label_4:
# then
# LBrackets "i_arr" (Lval (LId "n")) <- IntConst 99999
    int_const r0, 99999
    load r1, 1
    load_indirect r1, r1
    load r2, 3
    int_const r3, 1
    mul_int, r1, r1, r3
    sub_offset r1, r2, r1
    store_indirect r1, r0
# fi
label_5:
# LBrackets "b_arr" (IntConst 0) <- BoolConst True
    int_const r0, 1
    int_const r1, 0
    load r2, 2
    int_const r3, 1
    mul_int, r1, r1, r3
    sub_offset r1, r2, r1
    store_indirect r1, r0
# LBrackets "b_arr" (IntConst 1) <- BoolConst True
    int_const r0, 1
    int_const r1, 1
    load r2, 2
    int_const r3, 1
    mul_int, r1, r1, r3
    sub_offset r1, r2, r1
    store_indirect r1, r0
# LBrackets "b_arr" (IntConst 2) <- BoolConst True
    int_const r0, 1
    int_const r1, 2
    load r2, 2
    int_const r3, 1
    mul_int, r1, r1, r3
    sub_offset r1, r2, r1
    store_indirect r1, r0
# epilogue
    pop_stack_frame 5
    return

