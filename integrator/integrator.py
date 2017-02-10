from sys import argv
from subprocess import Popen
import os
from ast_search import *
# Name of a planet

lenarg=len(argv)
if lenarg < 2 : quit('Error! No arguments found. Please follow the format described in documentation')
if lenarg == 2 :
    astlist=[argv[1]]
if lenarg > 2 :
    if argv[1]=='-list':
        astlist=argv[2:]
    elif argv[1]=='-range':
        astlist=[str(i) for i in range(int(argv[2]),int(argv[3])+1)]
    else:
        astlist=argv[1:]
print(astlist)



statinfo = os.stat('../asteroids_format.csv').st_size
f=open('../asteroids_format.csv','r')

stlen=len(f.readline())
rem=statinfo%stlen

if rem==0:
    print('All is good with source...')
lrec=statinfo//stlen

records=[]

for i in astlist:
    records.append(find_asteroid(i,f,lrec,stlen))


f.close()
    
f=open('aeistatus.dat','rb+')
#for i in records:
    ##print(i)
    #try:
        #pos=int(i[0])
    #except:
        #continue
    #f.seek(pos-1)
    #aeist=f.read(1)
    #print(i[0],' aeistatus is ',aeist)

#f.close()




# Pack size for integrator
kmax=1000
print(kmax)

# Small.in header
smallin=[
    ')O+_06 Small-body initial data  (WARNING: Do not delete this line!!)\n',
    ') Lines beginning with `)\' are ignored.\n',
    ')---------------------------------------------------------------------\n',
    ' style (Cartesian, Asteroidal, Cometary) = Asteroidal\n',
    ')--------------------------------------------d or D is not matter--0d0 is possible too--\n',' \n']

if len(smallin)<6:
    smallin.append(' ')
sheader=smallin.copy()
#reslist=[]
#astlists=[]
#names=set()
#k=0

# Fill small.in and get .aei-s
def proc(smallin,sheader):
    open('../mercury/small.in','w').writelines(smallin)
    p=Popen("cd ../mercury; time ./mercury6; time ./element6",shell=True)
    p.wait()
    p=Popen("cd ../mercury; mv *.aei ../aeibase/ ; ./simple_clean.sh",shell=True)
    p.wait()
    print('got .aei-s')
    return None

k=0

for ast in records:
    try:
        pos=int(ast[0])
    except:
        continue
    f.seek(pos-1)
    aeist=f.read(1)
    if aeist==b'0':
        smallin.insert(5,' '+ast[1]+'d0 '+ast[2]+'d0 '+ast[3]+'d0 '+
                       ast[5]+'d0 '+ast[4]+'d0 '+ast[6]+'d0 0d0 0d0 0d0\n')
        smallin.insert(5,' '+ast[0]+' ep=2457600.5d0\n')
        k+=1
        f.seek(pos-1)
        f.write(b'1')
    # Integrate if pack is filled
    if k>=kmax:
        print('New block of ',kmax,' asteroids')
        k=0
        print('in Proc',len(smallin)/2)
        print(len(sheader))
        proc(smallin,sheader)
        smallin=sheader.copy()
        print('after Proc',len(smallin)/2)
        
        

if k>0:
    print('Some remained asteroids in process...')
    proc(smallin,sheader)
    k=0

f.close()