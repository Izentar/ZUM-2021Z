import os
import re
from pathlib import Path

ntreeBuffer = [10, 30, 50, 300, 400, 500]
mtryBuffer = [2, 3, 4, 5, 6, 10]
maxnodesBuffer = [30, 50, 100, 150, 200]


class FolderName():
    def __init__(self, word: list):
        self.N = int(word[1])
        self.ntree = str(word[3])
        self.mtry = int(word[5])
        self.maxnod = int(word[7])

def analyzeFolderName(name: str):
    names = re.split(r'/|\\', name)
    folderName = names[-2]
    tmp = re.split('_', folderName)
    return FolderName(tmp)

def generateName(ntree, mtry, maxnodes):
    return 'confusionMatrix_ntree_' + str(ntree) + '_mtry_' + str(mtry) + '_maxnodes_' + str(maxnodes) + '.txt'

def allFileNames():
    names = []
    for nt in ntreeBuffer:
        for mt in mtryBuffer:
            for maxn in maxnodesBuffer:
                names.append(generateName(nt, mt, maxn))
    return names

def getBuffer(names):
    fullBuffer = {}
    for n in names:
        fullBuffer[n] = []
    return fullBuffer

def flushBuffer(fullBuffer, newRoot):
    Path(newRoot).mkdir(parents=True, exist_ok=True)
    for key, val in fullBuffer.items():
        with open(os.path.join(newRoot, key), mode='w+') as handle: 
                handle.writelines(val)


def travelDirs(oldRoot, newRoot):
    names = allFileNames()
    fullBuffer = getBuffer(names)

    for(dirpath, dirs, files) in os.walk(oldRoot):
        for fil in files:
            if('confusionMatrix' in fil):
                data = readFile(os.path.join(dirpath, fil), fullBuffer)
                
    #for key, val in fullBuffer.items():
    #    print(key, len(val))
    flushBuffer(fullBuffer, newRoot)

def readFile(filePath, fullBuffer):
    with open(filePath, mode='r') as handle:
        #print(filePath)
        first = True
        ntreeIdx = 0
        bufferNtree = []
        for i, l in enumerate(handle.readlines()): # differen ntree
            if('Confusion Matrix and Statistics' in l):
                if(first):
                    first = False
                    bufferNtree.append(str(l))
                    continue
                ntree = ntreeBuffer[ntreeIdx]
                ntreeIdx += 1

                folderData = analyzeFolderName(filePath)
                nameIdx = generateName(ntree, folderData.mtry, folderData.maxnod)  

                fullBuffer[nameIdx] += bufferNtree

                bufferNtree = []
                bufferNtree.append(str(l))
            else:
                bufferNtree.append(str(l))
        ntree = ntreeBuffer[ntreeIdx]
        folderData = analyzeFolderName(filePath)
        nameIdx = generateName(ntree, folderData.mtry, folderData.maxnod)  
        fullBuffer[nameIdx] += bufferNtree
        

travelDirs(oldRoot='./test/RRF_goo', newRoot='./domTest')
