import string
import csv


rules = {}
# with open('SampleInput.txt', 'rb') as rulefile:
#     csv_reader = csv.reader(rulefile, delimiter='|')
#     for row in csv_reader:
#         if len(row)==2:
#             #print row
#             #print row[0]
#            # print row[1]
#            rules.update({row[0]:row[1]})
with open('ActualInput.txt', 'rb') as rulefile:
    csv_reader = csv.reader(rulefile, delimiter='\r')
    for row in csv_reader:
        print row
          # if len(row)==2:
          #     #print row
          #   #print row[0]
          #  # print row[1]
          #  rules.update({row[0]:row[1]})


print rules
print 
 
# with open("SampleInput.txt") as f:
#     for line in f:
#         if line not in ['\n', '\r\n']:
#             rows = ( line.split('|') for line in f )
#             rules = { row[0]:row[1:] for row in rows }

sample = open('ActualInput.txt')

for line in sample:
    if not "|" in line:
        message = line

print message

def decode(text, rules):
    for key in rules:
        text = text.replace(key, rules[key])
    return text

message = decode(message, rules)

print message
