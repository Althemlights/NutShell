import re
import os

def get_line_digit(line):
    digit_str = ""
    digit_list = list(filter(str.isdigit,line))
    for i in range(0,len(digit_list)):
        digit_str = digit_str + digit_list[i]
    return int(digit_str)

emu_log = open('./1.log',encoding='utf-8',mode='r')
bpu_log_path = './bpu.log'
if(os.path.exists(bpu_log_path)):
    os.remove(bpu_log_path)
bpu_log = open(bpu_log_path,encoding='utf-8',mode='w')
emu_log_lines = emu_log.readlines()
branch_right_num = 0
branch_wrong_num = 0
jal_right_num = 0
jal_wrong_num = 0
jalr_right_num = 0
jalr_wrong_num = 0
ret_right_num = 0
ret_wrong_num = 0
for line in emu_log_lines:
    #print(line)
    if(line.find('branchWrong') != -1):
        branch_wrong_num = get_line_digit(line)
    if(line.find('branchRight') != -1):
        branch_right_num = get_line_digit(line)
    if(line.find('jalRight') != -1):
        jal_right_num = get_line_digit(line)
    if(line.find('jalWrong') != -1):
        jal_wrong_num = get_line_digit(line)
    if(line.find('jalrRight') != -1):
        jalr_right_num = get_line_digit(line)
    if(line.find('jalrWrong') != -1):
        jalr_wrong_num = get_line_digit(line)
    if(line.find('retRight') != -1):
        ret_right_num = get_line_digit(line)
    if(line.find('retWrong') != -1):
        ret_wrong_num = get_line_digit(line)
#     if(line.find('cnt:01') != -1 or line.find('cnt:10') != -1):
    if(line.find('800019b4') != -1 ) :
        bpu_log.write(line)

branch_accuracy = float(branch_right_num)/(branch_right_num + branch_wrong_num)*100
jal_accuracy = float(jal_right_num)/(jal_right_num + jal_wrong_num)*100
jalr_accuracy = float(jalr_right_num)/(jalr_right_num + jalr_wrong_num)*100
ret_accuracy = float(ret_right_num)/(ret_right_num + ret_wrong_num)*100
overall_accuracy = (float(branch_right_num+jal_right_num+jalr_right_num+ret_right_num)/
(branch_right_num+jal_right_num+jalr_right_num+ret_right_num+branch_wrong_num+jal_wrong_num+jalr_wrong_num+ret_wrong_num)*100)
bpu_log.write('Branch prediction accuracy is %4.3f%% inst num is %d\n' % (branch_accuracy, branch_right_num + branch_wrong_num))
bpu_log.write('Jal prediction accuracy is %4.3f%% inst num is %d\n' % (jal_accuracy, jal_right_num + jal_wrong_num))
bpu_log.write('Jalr prediction accuracy is %4.3f%% inst num is %d\n' % (jalr_accuracy, jalr_right_num + jalr_wrong_num))
bpu_log.write('Ret prediction accuracy is %4.3f%% inst num is %d\n' % (ret_accuracy, ret_right_num + ret_wrong_num))
bpu_log.write('Overall prediction accuracy is %4.3f%%\n' % overall_accuracy)
bpu_log.seek(0)
#print('CondBranch prediction accuracy is %4.3f%%' % prediction_accuracy)
bpu_log.close

#test all kinds of PHT index

#read again for print
bpu_log_r = open(bpu_log_path,'r')

bpu_log_r_lines = bpu_log_r.readlines()
# for line in bpu_log_r_lines:
#     print(line)
bpu_log_r.close()
