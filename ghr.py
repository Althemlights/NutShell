import re
import os

emu_log = open('./1.log',encoding='utf-8',mode='r')
bpu_log_path = './bpu.log'
if(os.path.exists(bpu_log_path)):
    os.remove(bpu_log_path)
bpu_log = open(bpu_log_path,encoding='utf-8',mode='w')
emu_log_lines = emu_log.readlines()

for line in emu_log_lines:

    if(line.find('800019b4') != -1 ) :
        bpu_log.write(line)

bpu_log.seek(0)
bpu_log.close
bpu_log_r = open(bpu_log_path,encoding='utf-8',mode='r')
bpu_log_r_list = bpu_log_r.readlines()

taken_list = [0]*2**6
nottaken_list = [0]*2**6

for i in range(0,2**6):
    i_bin = '{0:06b}'.format(i)
    for j in bpu_log_r_list:
        if(j.find('ghr:'+i_bin) != -1):
            if(j.find('taken:1') != -1):
                taken_list[i] += 1
            if(j.find('taken:0') != -1):
                nottaken_list[i] += 1

for i in range(0,2**6):
    i_bin = '{0:06b}'.format(i)
#     if((taken_list[i] + nottaken_list[i])!= 0):
#         if(float(taken_list[i])/(taken_list[i] + nottaken_list[i]) > 0.7 or float(nottaken_list[i])/(taken_list[i] + nottaken_list[i]) > 0.7):
    print('ghr: '+ i_bin +' taken_num: %d, not_taken_num: %d\n' %(taken_list[i], nottaken_list[i]))

bpu_log_r.close()