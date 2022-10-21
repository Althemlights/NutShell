
path = './1.txt'
path2 = '/home/bread/xs-env/NEMU/1.txt'

fp1 = open(path,'r',encoding='utf-8')
fp2 = open(path2,'r',encoding='utf-8')
lines1 = fp1.readlines()
lines2 = fp2.readlines()
print(lines1[0][15:24])
print(lines2[0][2:18])

index=0
while(int(lines1[index][15:24],16) == int(lines2[index][2:18],16)):
    index= index +1
print(index)
fp1.close()
fp2.close()