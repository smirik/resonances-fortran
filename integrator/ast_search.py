
def find_asteroid(i,f2,lrec,stlen):
    print('\nSeeking record about ',repr(i))
    st=[]
    flag=0
    a=0
    z=lrec
    f2.seek(a*stlen)
    st=f2.readline().split()
    if st[0] == i:
        print('Found record\n',' '.join(st),'\nat position ',int(a)+1)
        return st
    f2.seek((z-1)*stlen)
    st=f2.readline().split()
    if st[0] == i:
        print('Found record\n',' '.join(st),'\nat position ',int(z))
        return st
    print('...Start iterations',a,z)
    while z-a>1 and flag==0:
        o=(z+a)//2
        f2.seek(o*stlen)
        st=f2.readline().split()
        if st[0]==i:
            flag=1
            break
        elif st[0]>i:
            z=o
        else:
            a=o
    if st[0] != i:
        print('...Not found any record about ',repr(i))
    else:
        print('Found record\n',' '.join(st),'\nat position ',int(o)+1)
    return st