
stop0 = 5000
stop1 = 1000
whereNumber = -3

with open('data/creditcard.txt', mode='r') as handle, open('data/creditcardtest.txt', mode='w') as hwrite:
    count0 = 0
    count1 = 0
    for i, l in enumerate(handle.readlines()):
        if(i == 0): # save header
            hwrite.write(l)
        if(l[whereNumber] == "0" and count0 < stop0):
            count0 += 1
            hwrite.write(l)
            continue
        if(l[whereNumber] == "1" and count1 < stop1):
            count1 += 1
            hwrite.write(l)
            continue
        if(count0 >= stop0 and count1 >= stop1):
            break