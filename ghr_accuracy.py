import re
import os

emu_log = open('./1.log',encoding='utf-8',mode='r')
ghr_log_path = './ghr.log'
if(os.path.exists(ghr_log_path)):
    os.remove(ghr_log_path)
ghr_log = open(ghr_log_path,encoding='utf-8',mode='w')
emu_log_lines = emu_log.readlines()

ghr_right_num = 0
ghr_wrong_num = 0

for line in emu_log_lines:
    if(line[36:41] == line[56:61]):
        ghr_right_num += 1
    else:
        ghr_wrong_num += 1
        ghr_log.write(line)

ghr_accuracy = float(ghr_right_num)/(ghr_right_num + ghr_wrong_num)*100
print('ghr accuracy is  %4.3f%%\n' % ghr_accuracy)
ghr_log.close()