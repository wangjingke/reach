outlet = "D:/REACH/MATCH/Dataset/MATCH_EMA/MATCH_EMA_V21_Dictionary.csv"

sourcefile = open('D:/REACH/MATCH/Dataset/MATCH_EMA/Wave2/Beta20160907/MATCH_EMA_W2_2016-09-07.do', 'r')
varlist = sourcefile.readlines()
sourcefile.close()

targetfile = open(outlet, 'w')
for lineX in varlist:
    if lineX.startswith('label variable'):
        segX = lineX.replace('label variable', '').strip(' ').split(' ')
        outputX = '"'+segX[0]+'",'+' '.join(segX[1:])
        targetfile.write(outputX)
targetfile.close()