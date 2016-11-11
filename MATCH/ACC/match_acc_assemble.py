work_dir = "D:/REACH/MATCH/pythonACC"

# import accelerometer module from path
import sys
sys.path.append('D:/GitHub/reach/common')
import accelerometer as acc

import os
# list all files ending with csv
def all_csv(path):
    filelist = []
    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith('.csv') and not 'DataTable' in file:
                filelist.append(os.path.join(root, file))
    return filelist

filelist = all_csv('Y:/MATCH STUDY/Main Study/Data/Meterplus/ACTIGRAPH') + all_csv('Y:/MATCH STUDY/Main Study/Data/Manual Uploads/Actigraph Data')

import re
numPattern = re.compile('[0-9]{5}')
wavePattern = re.compile('(?<=W)[0-9]{1}', re.IGNORECASE)
for i in range(0, len(filelist)):
    fileX = filelist[i]
    idX = numPattern.search(fileX)
    if idX:
        waveX = wavePattern.search(fileX)
        if waveX:
            accX = acc.Meterplus()
            accX.read(idX.group(0), waveX.group(0), fileX)
            if accX.data is None:
                accX.read(idX.group(0), waveX.group(0), fileX.split(".csv")[0]+"DataTable.csv")
            if accX.data is not None:
                accX.label()
                accX.save(os.path.join(work_dir, "MATCH_"+idX.group(0)+"_"+"W"+waveX.group(0)+"_"+accX.basicInfo['start_date']+".p"))
            del accX
    if i%10==0:
        print(str(round(i/len(filelist)*100, 2))+"%")
