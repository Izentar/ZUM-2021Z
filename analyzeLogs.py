import pathlib
import os
import numpy as np

reference = "Reference"

class Data:
    def __init__(self, path):
        self.vals = {}
        self.matrix = {}
        self.longListKappa = []
        self.longListSensitivity = []
        self.file = path

    def add(self, name, val):
        if(name not in self.vals):
            self.vals[name] = []
        self.vals[name].append(val)
    
    def addMatrix(self, name, val):
        if(name not in self.matrix):
            self.matrix[name] = []
        self.matrix[name].append(val)

    def addListKappa(self, listed):
        self.longListKappa.append(listed)

    def addListSensitivity(self, listed):
        self.longListSensitivity.append(listed)

    def getAvg(self):
        avg = {'File:': self.file}
        for k, v in self.matrix.items():
            tmp = 0.0
            for val in v:
                tmp += val
            avg[k] = tmp / len(v)
            #print(k, ": ", tmp / len(v))
        for k, v in self.vals.items():
            tmp = 0.0
            for val in v:
                tmp += val
            avg[k] = tmp / len(v)
            #print(k, ": ", tmp / len(v))

        if(len(self.longListSensitivity) == 0 or len(self.longListKappa) == 0):
            return avg

        lKappa = np.zeros(len(self.longListKappa[0]))
        lSensitivity = np.zeros(len(self.longListSensitivity[0]))
        for v in self.longListKappa: 
            for idx, val in enumerate(v):
                lKappa[idx] += val
        for idx, v in enumerate(self.longListSensitivity): 
            for idx, val in enumerate(v):
                lSensitivity[idx] += val
        avg['one'] = lKappa / len(self.longListKappa)
        avg['two'] = lSensitivity / len(self.longListSensitivity)
        #print(lKappa / len(self.longListKappa))
        #print(lSensitivity / len(self.longListSensitivity))
        return avg

def strToBool(val):
    if(val == "\"TRUE\"" or val == "TRUE" or val == "\"1\"" or val == 1):
        return True
    if(val == "\"FALSE\"" or val == "FALSE" or val == "\"0\"" or val == 0):
        return False

def readFileMat(filePath):
    index = 0
    refCounter = 0
    data = Data(filePath)

    precision_flag = False
    recall_flag = False
    f1_flag = False
    AUC_flag = False
    Reference_flag = False
    Kappa_flag = False
    Sensitivity_flag = False

    with open(filePath, mode='r') as handle:
        for i, l in enumerate(handle.readlines()):

            if(precision_flag):
                precision_flag = False
                data.add('precision', float(l))
            if(recall_flag):
                recall_flag = False
                data.add('recall', float(l))
            if(f1_flag):
                f1_flag = False
                data.add('f1', float(l))
            if(AUC_flag):
                AUC_flag = False
                data.add('AUC', float(l))
            if(Reference_flag):
                refCounter += 1
                if(refCounter == 4):
                    Reference_flag = False
                    refCounter = 0
                tmp = l.split(',')
                data.addMatrix((strToBool(tmp[0]), strToBool(tmp[1])), float(tmp[2]))
            if(Kappa_flag):
                Kappa_flag = False
                tmp = l.split(',')
                data.addListKappa([float(i) for i in tmp])
            if(Sensitivity_flag):
                Sensitivity_flag = False
                tmp = l.split(',')
                data.addListSensitivity([float(i) for i in tmp])

            if('Index for' in l):
                index += 1
            if('precision' in l):
                precision_flag = True
            if('recall' in l):
                recall_flag = True
            if('f1 score' in l):
                f1_flag = True
            if('AUC' in l):
                AUC_flag = True
            if(reference in l):
                Reference_flag = True
            if('Kappa' in l):
                Kappa_flag = True
            if('Sensitivity' in l):
                Sensitivity_flag = True

    return data

def readL(key, pos, line, data, ignore = None):
    tmp = line.split(' ')
    tmp = list(filter(None, tmp))
    if(ignore is not None):
        for i in ignore:
            if(i in tmp):
                return
    if(key in line):
        data.add(key, float(tmp[pos]))


def readFileDom(filePath):
    index = 0
    refCounter = 0
    data = Data(filePath)

    Reference_flag = False

    with open(filePath, mode='r') as handle:
        for i, l in enumerate(handle.readlines()):
            if(Reference_flag):
                refCounter += 1
                if(refCounter == 4):
                    Reference_flag = False
                    refCounter = 0
                tmp = l.split(' ')
                tmp = list(filter(None, tmp))
                if(refCounter == 1):
                    pass
                if(refCounter == 2):
                    data.addMatrix((False, False), float(tmp[1]))
                    data.addMatrix((True, False), float(tmp[2]))
                if(refCounter == 3):
                    data.addMatrix((False, True), float(tmp[1]))
                    data.addMatrix((True, True), float(tmp[2]))

            if('Index for' in l):
                index += 1

            if('Reference' in l):
                Reference_flag = True
            readL('Accuracy', 2, l, data, ['Balanced'])  #['Balanced Accuracy']
            if('95% CI' in l):
                tmp = l.split(' ')
                tmp = list(filter(None, tmp))
                tmp[3] = tmp[3].replace(',', '')
                data.add('95% CI Low', float(tmp[3].replace('(', '')))
                data.add('95% CI High', float(tmp[4].replace(')', '')))
            readL('No Information Rate', 4, l, data)
            readL('P-Value [Acc > NIR]', 5, l, data)
            readL('Kappa', 2, l, data)
            readL('Mcnemars Test P-Value', 4, l, data)
            readL('Sensitivity', 2, l, data)
            readL('Specificity', 2, l, data)
            readL('Pos Pred Value', 4, l, data)
            readL('Neg Pred Value', 4, l, data)
            readL('Prevalence', 2, l, data, ['Detection'])  # ['Detection Prevalence']
            readL('Detection Rate', 3, l, data)
            readL('Detection Prevalence', 3, l, data)
            readL('Balanced Accuracy', 3, l, data)

    return data

def dictPrint(d):
    for key, val in d.items():
        print(key, val)

def isBetter(best: dict, two: dict):
    if(best is None or two is None):
        return True
    b1 = best[(True, False)]
    b2 = best[(False, True)]
    t1 = two[(True, False)]
    t2 = two[(False, True)]
    if(b1 + b2 > t1 + t2):
        return True
    return False

def travelDirs(root):
    best = None
    for(dirpath, dirs, files) in os.walk(root):
        for fil in files:
            if('result_' in fil):
                data = readFileMat(os.path.join(dirpath, fil))
                avg = data.getAvg()
                if(isBetter(best, avg)):
                    best = avg
            if('confusionMatrix' in fil):
                data = readFileDom(os.path.join(dirpath, fil))
                avg = data.getAvg()
                if(isBetter(best, avg)):
                    best = avg
    #print(best)
    dictPrint(best)



#data = readFile('outOne/gamma_0.01/nu_0.01/result_gamma_0.01_nu_0.01.txt')
#data.getAvg()

travelDirs('./test/outForest')


