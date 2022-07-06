import re
import os
from collections import Counter

if(os.path.exists('./profiling.log')):
    os.remove('./profiling.log')
if(os.path.exists('./mispredict.log')):
    os.remove('./mispredict.log')
profiling_log = open('./profiling.log',encoding='utf-8',mode='w')
mispredict_log_w = open('./mispredict.log',encoding='utf-8',mode='w')

cpu_log = open('./1.log',encoding='utf-8',mode='r')
cpu_log_list = cpu_log.readlines()

for i in cpu_log_list:
    if(i.find('isMissPredict:1') != -1):
        mispredict_log_w.write(i)
mispredict_log_w.seek(0)
mispredict_log_w.close()

mispredict_log_r = open('./mispredict.log',encoding='utf-8',mode='r')
mispredict_log_list = mispredict_log_r.readlines()

string_num = 0
misp_pc_list = []
for i in mispredict_log_list:
    misp_pc_list.append(i[18:26])
    string_num += 1
mispredict_log_r.close()

cnt = Counter(misp_pc_list)
cnt_most_common = cnt.most_common(20)
print(cnt_most_common)

