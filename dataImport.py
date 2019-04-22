import csv
from nltk.corpus import stopwords
import numpy

stopWordsInit = set(stopwords.words('english'))
stopWords = ["i"]
for word in stopWordsInit:
    stopWords.append(word.encode('ascii','ignore'))

def readCSV(csvFilename):
  fileObj = open(csvFilename, 'rU')
  myReader = csv.DictReader(fileObj)
  table = []
  for row in myReader:
    lyrics = row['lyrics']
    lyrics = lyrics.split("\n")
    cleanObject = {"artist":row['artist'],
                   "song":row['song'],
                   'genre':row['genre'],
                   'year':row['year'],
                   "lyrics":cleanLyric(lyrics)}
    print(cleanObject)
    return
  fileObj.close()
  return table

def cleanLyric(song):
    toReturn = []
    for sentence in song:
        eachSentence = []
        for word in sentence.split():
            if word.lower() not in stopWords and word.lower() != "i":
                eachSentence.append(word.lower().replace("?","").replace("!","").replace(",","").replace("'",""))
        toReturn.append(eachSentence)
    return toReturn

def dimensionWordArray(corpus):
    dimensionWord = []
    for document in corpus:
        for word in document:
            if word not in dimensionWord:
                dimensionWord.append(word)
    return dimensionWord

def tfScore(document):
    length = len(document)
    tfDictionary = {}
    for word in document:
        tfDictionary[word] = 0
    for word in document:
        tfDictionary[word] = tfDictionary[word] + 1
    tfScores = {}
    for word in tfDictionary.keys():
        tfScores[word] = float(tfDictionary[word])/length
    return tfScores

def IDFScore(corpus, wordArray):
    IDFDict = {}
    corpusLength = len(corpus)
    for word in wordArray.keys():
        num = 0
        for document in corpus:
            if word in document:
                num += 1
        IDFDict[word] = numpy.log(float(corpusLength)/num)
    return IDFDict


#print(tfScore(["word", "word", "hello", "lightning"]))
# example = {'word': 0.5, 'hello': 0.25, 'lightning': 0.25}
# exampleCorpus = [["word","happy","popsicle"],['word', 'lucky', 'hello', 'big', 'deal'],['talk', 'lightning', 'cause', 'back']]
#
# print(IDFScore(exampleCorpus,example))
# print(numpy.log(float(3)/2))


readCSV("../TopologyData/lyrics.csv")

