; ********************************************************************************************
; ********************************************************************************************
;
;	Name :      const.asm
;	Purpose :   ..
;	Created :   15th Nov 1991
;	Updated :   4th Jan 2021
;	Authors :   Fred Bowen
;
; ********************************************************************************************
; ********************************************************************************************


end_token       = $80                                   ; v2 commands
for_token       = $81
next_token      = $82
data_token      = $83
input_token     = $84
goto_token      = $89
run_token       = $8a
restore_token   = $8c
gosub_token     = $8d
rem_token       = $8f
on_token        = $91
load_token      = $93
save_token      = $94
verify_token    = $95
def_token       = $96
print_token     = $99
clr_token       = $9c
sys_token       = $9e
open_token      = $9f
close_token     = $a0
new_token       = $a2
tab_token       = $a3
to_token        = $a4
fn_token        = $a5
spc_token       = $a6
then_token      = $a7
not_token       = $a8
step_token      = $a9
plus_token      = $aa                                   ; operators
minus_token     = $ab
greater_token   = $b1
equal_token     = $b2
less_token      = $b3
first_function_token= $b4                                   ; v2 functions
left_token      = $c8
mid_token       = $ca
go_token        = $cb                                   ; kludges
rgraphic_token  = $cc                                   ; first new v7 token
esc_function_token= $ce
err_token       = $d3
instr_token     = $d4
last_function_token= $d4
else_token      = $d5
resume_token    = $d6
trap_token      = $d7
color_token     = $e7
do_token        = $eb
loop_token      = $ec
key_token       = $f9
monitor_token   = $fa
using_token     = $fb
until_token     = $fc
while_token     = $fd
esc_command_token= $fe

first_esc_command_token= $02
collision_token = $17
begin_token     = $18
bend_token      = $19
off_token       = $24
ectory_token    = $29
set_token       = $2d
pic_token       = $37
disk_token      = $40
last_esc_command_token= $45                                   ; <<<< last_command_token

first_esc_function_token= $02
pointer_token   = $0a
last_esc_function_token= $0d                                   ; [910820]



; ********************************************************************************************
;
;	Date		Changes
;	====		=======
;
; ********************************************************************************************
