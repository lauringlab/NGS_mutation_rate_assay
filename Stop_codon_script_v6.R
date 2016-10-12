
#This script is designed to determine the number of codons in a gene that can mutate into a premature stop codon.

#function inputs are:
# genomefile - the ".fas" file that you want to analyze
# bases - the number of bases that you want in each window that is analyzed for codons that can mutate to stop codons
# sortcol - the column in the output that you want to sort by  
	# 2- sum of all codons that can mutate to stop codon
	# 3-  codons assessing U2A and U2G
	# 4-	codons assessing U2A
	# 5-	codons assessing C2A and C2G
	# 6-	codons assessing C2A
	# 7-	codons assessing C2U
	# 8-	codons assessing G2A
	# 9-	codons assessing A2U
	# 10-	codons assessing G2U

StopCodon<-function(genomefile, bases, sortcol){

genefile<-read.csv(genomefile,sep=";")			#reads the unzipped file

genefile2<-genefile[,]					#Makes the read-in file usable for the following manipulation	

gene<-toupper(paste(genefile2, collapse=""))			#pastes the read-in file into a list of the entire genome

start<-(regexpr("ATG", gene)[1])            		#Finds the FIRST start codon in the seqience that is given

genesplit<-strsplit(gene,split='')       	#This and next 2 split string into list of individual bases

same<-function(x){x}

singlebase<-sapply(genesplit, same)

len<-length(singlebase)

geneORF<-paste(singlebase[(start:len),], collapse="")    #makes string starting with the forst ATG

geneORF

########

				#The following for loop will break the gene into codons


for (q in 1:length(geneORF)){
  codons<-substring(geneORF[q], seq(1,nchar(geneORF[q])-1,3),seq(3,nchar(geneORF[q]),3))
}

		#The following replaces all codons that can mutate to a stop codon with  "1". All other codons replaced with a "0"
		

lethcodonmut<-function(x){
	y<-gsub("TTA|TAT","1",x)
	y<-gsub("TTG|TGT","1",y)
	y<-gsub("TCA|TAC","1",y)
	y<-gsub("TGC|TCG","1",y)
	y<-gsub("CAA|CAG|CGA","1",y)
	y<-gsub("TGG","1",y)
	y<-gsub("AAA|AAG|AGA","1",y)
	y<-gsub("GAA|GAG|GGA","1",y)
	y<-gsub("...","0",y)
	y
	}

lethcodonmutations<-lethcodonmut(codons)			#changes codons to character numerals

le<-as.numeric(lethcodonmutations)				#makes numeric		

######

lethcodonmuta<-function(x){
	y<-gsub("TTA|TAT","1",x)
	y<-gsub("TTG|TGT","2",y)
	y<-gsub("TCA|TAC","3",y)
	y<-gsub("TGC|TCG","4",y)
	y<-gsub("CAA|CAG|CGA","5",y)
	y<-gsub("TGG","6",y)
	y<-gsub("AAA|AAG|AGA","7",y)
	y<-gsub("GAA|GAG|GGA","8",y)
	y<-gsub("...","0",y)
	y
	}

lethcodonmutationsa<-lethcodonmuta(codons)			#changes codons to character numerals

lea<-as.numeric(lethcodonmutationsa)				#makes numeric		

#######



cod<-round(bases/3)						#converts the bases input into number of codons




windows<-matrix(rep(0),nrow=(length(le)-cod), ncol=10)		#makes vector to populate with scores for windows of codons


								#the following will divide into windows of 100 codons (300 bases) and sum

colnames(windows)<-c("start base","number of lethal codons","U2A and U2G","U2A","C2A and C2G","C2A","C2U","G2A","A2U","G2U")

for (q in 1:(length(le)-cod)){
	windows[q,2]<-sum(le[(0+q):(cod-1+q)]) 
	windows[q,1]<-((3*q)-2)
	windows[q,3]<-sum(lea[(0+q):(cod-1+q)]==1)
	windows[q,4]<-sum(lea[(0+q):(cod-1+q)]==2)
	windows[q,5]<-sum(lea[(0+q):(cod-1+q)]==3)
	windows[q,6]<-sum(lea[(0+q):(cod-1+q)]==4)
	windows[q,7]<-sum(lea[(0+q):(cod-1+q)]==5)
	windows[q,8]<-sum(lea[(0+q):(cod-1+q)]==6)
	windows[q,9]<-sum(lea[(0+q):(cod-1+q)]==7)
	windows[q,10]<-sum(lea[(0+q):(cod-1+q)]==8)

}

windowsort<-windows[order(windows[,sortcol], decreasing=TRUE),]

			#The following will allow the script to return two things (The ORF sequence and the output table)

ret<-(head(windowsort, 100))

retu<-list(geneORF,ret)

return(retu)

}
