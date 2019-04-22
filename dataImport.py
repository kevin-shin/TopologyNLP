import nltk
import csv

def readCSV(csvFilename):
  fileObj = open(csvFilename, 'rU')
  myReader = csv.DictReader(fileObj)
  table = []
  for row in myReader:
    lyrics = row['lyrics'].replace("\n"," ")
    cleanObject = {"artist":row['artist'],
                   "song":row['song'],
                   'genre':row['genre'],
                   'year':row['year'],
                   "lyrics":cleanString(lyrics)}
    print(cleanObject)
    return
  fileObj.close()
  return table

def cleanString(String):
    string = String.split()
    toReturn = []
    for word in string:
        toReturn.append(word.lower().replace("?","").replace("!",""))
    return toReturn

readCSV("lyrics.csv")

