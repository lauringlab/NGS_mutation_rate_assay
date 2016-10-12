import sys
from collections import Counter
import re

# Create the empty lists to count nucleotides
pos=[]
ref=[]
cov=[]
A=[]

T=[]

G=[]

C=[]
N=[]

with open(sys.argv[1],'r') as pileup:
    for line in pileup:
        line = re.split(r'\t+', line) # split at tab
        pos.append(str(line[1])) # position
        ref.append(str(line[2])) # Reference base
        cov.append(str(line[3])) # Coverage
        counter=Counter(line[4]) # count the occurrence of each character in the bases string
        
        A.append(str(counter['A'])) # how many A's
      
        T.append(str(counter['T'])) # How many T's
       
        G.append(str(counter['G']))
       
        C.append(str(counter['C']))
        N.append(str(counter['N']))
        

total_positions=len(pos)        #how many positions did we look at
with open(sys.argv[2],'w') as out:
    out.write("pos,ref,cov,A,T,G,C,N\n") # Write the header to the out file
    for i in range(0,total_positions) :
        out.write(pos[i]+","+ref[i]+","+cov[i]+","+A[i]+","+T[i]+","+G[i]+","+C[i]+","+N[i]+"\n") # write each line to the outfile
               
