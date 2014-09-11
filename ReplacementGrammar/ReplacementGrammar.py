text = open('ActualInput.txt').read()
#each line is an element
lines = text.split('\r\n')
#remove null strings
lines = [l for l in lines if l]

#last element is the message
message = lines[-1]
#everything before is a rewrite rule
rules = lines[:-1]
print message
print rules

#loop through rules, rewrite all of message with each token
for rule in rules:
    #print rule
    car=rule.split('|')[0]
    caar=rule.split('|')[1]        
    message=message.replace(car,caar)

print message 
