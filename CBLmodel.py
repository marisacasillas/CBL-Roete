#!/usr/bin/env python2.7

from __future__ import division
import argparse
import numpy as np
import cPickle as pickle
from random import shuffle
from copy import deepcopy
import csv
import os

class Word:
    def __init__(self, ortho, count):
        self.ortho = ortho  # A string, orthographic representation of the word
        self.count = count # An integer-value, how often the word has been counted so far

    # returns a printable string version of the word
    def show(self):
        return self.ortho

class WordList:
    def __init__(self):
        self.size = 0 # An integer-value, how many words are stored in the list
        self.all = [] # List of Word-objects

    # adds a Word to WordList or updates WordList values if Word is already there, and return the count of the Word-object
    def add_word(self, word):
        for k in range(0, self.size):
            if self.all[k].ortho == word:
                self.all[k].count += 1
                return self.all[k].count
        # if the word is new, add it to the list
        ww = Word(word, 1)
        self.all.append(ww)
        self.size += 1
        return ww.count

    # returns the index of the Word-object within the WordList
    def findWord(self, word):
        for k in range(0, self.size):
            if self.all[k].ortho == word.ortho:
                return k
        return None

class ChunkList:
    def __init__(self):
        self.size = 0 # An integer-value, how many chunks are stored in the list
        self.max_chunk_length = 0 # The size of the largest chunk
        self.all = {} # Dictionary of Chunk-objects

    def add_chunk(self, chunk):
        chunk_key = tuple(chunk.ortho)
        self.all[chunk_key] = chunk
        self.max_chunk_length = max(self.max_chunk_length, len(chunk.ortho))
        self.size += 1

    def lookup(self, chunk):
        chunk_key = tuple(chunk.ortho)
        return self.all.get(chunk_key)

    def lookup_by_key(self, word_sequence):
        chunk_key = tuple(word_sequence)
        return self.all.get(chunk_key)

    # counts how often pair is (part of) an already stored chunk
    def count_pair(self, pair):
        pair_key = tuple(pair)
        count = 0
        match = None
        for chunk_key, chunk in self.all.iteritems():
            # check for perfect match of pair with chunk
            if chunk_key == pair_key:
                match = chunk
                count += chunk.count
            # check if pair is subset of stored chunk
            elif findsubset(chunk_key, pair_key):
                count += chunk.count
        # count: how often the pair is (part of) a chunk,
        # match: the chunk (if any) that exactly matches pair
        return count, match

class Chunk:
    def __init__(self):
        self.ortho = [] # List of strings, orthographic representating of the chunk
        self.count = 0 # An integer-value, how often the chunk has been seen

# returns a printable string version of the chunk
def show(chunk):
    return (str(chunk.ortho) + "\t counted: " + str(chunk.count) + " times")

class Wordpair:
    def __init__(self, pair, count):
        self.ortho = pair #String, orthographic representation of Word pair
        self.count = count #An integer-value, how often the pair has been seen


"class PairList: "
class PairList:
    def __init__(self):
        self.all = [] # List, containing Pair-object
        self.size = 0 # An integer-value, how many pairs are stored in the PairList

    # updates word-pair list, by adding pair if it is new, or increasing the count of an already stored Pair,
    # and returns the count of the pair
    def update_pairlist(self, pair):
        for k in range(0, self.size):
            if self.all[k].ortho == pair:
                self.all[k].count += 1
                return self.all[k].count
        # if the word-pair is new, add it to the list
        wordpair = Wordpair(pair, 1)
        self.all.append(wordpair)
        self.size += 1
        return wordpair.count

"class ChunkPairList: "
class ChunkPairList:
    def __init__(self):
        self.all = [] # List, containing ChunkPair-objects
        self.size = 0 # An integer-value, how many ChunkPairs are stored in the ChunkList

    # updates chunkpair list,  by adding pair if it is new, or increasing the count of an already stored ChunkPair
    # and returns the count of the chunkpair
    def update_pairlist(self, pair):
        temp_pair_0 = ""
        temp_pair_1 = ""
        if isinstance(pair[0], Chunk):
            temp_pair_0 += str(pair[0].ortho)
        else:
            temp_pair_0 += str(pair[0])
        temp_pair_1 += str(pair[1].ortho)

        for k in range(0, self.size):
            temp_0 = ""
            temp_1 = ""
            # if the word-pair is already in the list, adjust the associated word-pair count
            if isinstance(self.all[k].ortho[0], Chunk):
                temp_0 += str(self.all[k].ortho[0].ortho)
            else:
                temp_0 += self.all[k].ortho[0]
            temp_1 += str(self.all[k].ortho[1].ortho)
            if (temp_0 == temp_pair_0) and (temp_1 == temp_pair_1):
                self.all[k].count += 1
                return self.all[k].count
        # if the word-pair is new, add it to the list
        wordpair = Wordpair(pair, 1)
        self.all.append(wordpair)
        self.size += 1
        return wordpair.count

# Utterance-class for use in the production task, to store all information about an utterance in one place
class Utterance:
    def __init__(self):
        self.ortho = None # String, orthographic representation of Utterance
        self.num = None # Integer-value, utterance number
        self.skipped = False # Boolean, False if Utterance is not skipped during reconstruction
        self.reconstructed = None # Boolean, True if Utterance is reconstructed correctly
        self.bag_of_ortho = None # List, shuffled Chunks build from utterance
        self.prediction = None # String, reconstructed version of the utterance

# List of all utterances
class allUtterances:
    def __init__(self):
        self.all = [] # List, containing Utterance-objects
        self.size = 0 # Integer-value, number of Utterances stored in allUtterances


"----------------------------"
"functions outside of classes"
"----------------------------"

# Computes the backward transitional probability between a pair of Words with w_count: word count and p_count: pair count,
# using BTP = P(Y|x) = frequency XY / frequency Y
def compute_btp(w_count, p_count):
    assert (p_count >= 0)
    assert (w_count >= 0)
    if w_count == 0:
        return 0
    btp = p_count / w_count
    return btp

# Reads in corpus files, filename: corpus file, sfilename: size of corpus file and returns the corpus, number of utterances,
# and the utterances as a list of Word-objects, and average, minimum and maximum utterance lengths
def fileread(filename, sfilename):
    MIN = float('Inf')
    MAX = 0
    # reads in the size of the corpusfile from a -.txt file
    sfile = open(sfilename, "r")
    NUM_UTTERANCES = int(sfile.readline())
    sfile.close()
    # reads in '\n'-separated utterances from -.txt file
    file = open(filename, "r")
    corpus = np.ndarray((NUM_UTTERANCES,), dtype=np.object)
    NUM_WORDS = 0

    # reads in the utterances one by one and saves each utterance as a list of Word-objects
    for i in range(0, NUM_UTTERANCES):
        temp = file.readline()
        temp = temp.split()
        u = np.ndarray((len(temp),), dtype=np.object)
        NUM_WORDS += len(temp)

        if MIN > len(temp):
            MIN = len(temp)
        if MAX < len(temp):
            MAX = len(temp)

        for j in range(0, len(temp)):
            w = Word(temp[j], None)
            u[j] = w
        corpus[i] = u
    file.close()

    # computes the average utterance length
    AVERAGE_UT_LENGTH = NUM_WORDS / NUM_UTTERANCES
    UT_MEASURES = [AVERAGE_UT_LENGTH, MIN, MAX]
    return corpus, NUM_UTTERANCES, NUM_WORDS, UT_MEASURES


# returns whether Pair-object pair is a subset of Chunk-object chunk
def findsubset(chunk, pair):
    for subtuple_length in reversed(xrange(1, len(chunk))):
        for start_index in xrange(0, (len(chunk) + 1 - subtuple_length)):
            temp = chunk[start_index:start_index + subtuple_length]
            if temp == pair:
                return True
    return False


# returns the running average backward transitional probability from a list of BTPs
def averageBTP(btps):
    # average is set to zero when there are no btp's calculated yet
    if len(btps) == 0:
        average = 0
    else:
        average = np.mean(btps)
    return average

# determines the chunks in the corpus, algorithm follows McCauley & Christiansen (2011)
def chunk_corpus(corpus):
    # initializations
    chunks = ChunkList()
    ortho = WordList()
    pairs = PairList()
    btps = []
    chunkpairs = ChunkPairList()

    # loop through all utterances in the corpus one-by-one to find chunks
    for i in range(0, NUM_UTTERANCES):

        if i%1000 == 0:
            print("currently processing utterance: "  + str(i))

        utterance = corpus[i]
        # reset chunk for each new utterance
        chunk = Chunk()
        previous_chunk = ''

        # update word count for first word in utterance, compensation for the next
        # for-loop starting at the second word
        word = utterance[0].ortho
        w_count = ortho.add_word(word)

        # for the second-until-the-last word in the utterance
        for j in range(1, len(utterance)):
            #update word and wordpair count for current word
            word = utterance[j].ortho
            w_count = ortho.add_word(word)
            pair = [utterance[j - 1].ortho, utterance[j].ortho]
            p_count = pairs.update_pairlist(pair)

            # compute running average backward transitional probability
            average = averageBTP(btps)

            # compute backward transitional probability of current word pair
            btp = compute_btp(w_count, p_count)
            btps.append(btp)

            # search for chunks in utterances
            # when chunkatory is empty
            if chunks.size == 0:
                # add first word of pair to chunk, but do not insert boundary yet
                chunk.ortho = chunk.ortho + [pair[0]]
                # if btp indicates boundary
                if btp <= average:
                    # add chunk to chunk-array
                    chunk.count = 1
                    chunks.add_chunk(chunk)
                    chunkpair = [previous_chunk, chunk]
                    c_count = chunkpairs.update_pairlist(chunkpair)
                    previous_chunk = chunk
                    # reset chunk
                    chunk = Chunk()
                    continue

            # when chunkatory is not empty
            else:
                # count how often pair (is part of) an already stored chunk
                count, matched_chunk = chunks.count_pair(pair)
                # if pair has been seen at least twice before as (part of) a chunk
                if count >= 2:
                    # no perfect match in the dictionary, but count is high enough
                    if matched_chunk is None:
                        # add chunk to chunkatory, and frame to frame dictionary
                        chunk.ortho = pair
                        chunk.count = 1
                        chunks.add_chunk(chunk)
                        chunkpair = [previous_chunk, chunk]
                        c_count = chunkpairs.update_pairlist(chunkpair)
                        previous_chunk = chunk
                    # if perfect match was found
                    else:
                        # up count of existing matching chunk
                        matched_chunk.count += 1
                        chunkpair = [previous_chunk, chunk]
                        c_count = chunkpairs.update_pairlist(chunkpair)
                        previous_chunk = matched_chunk
                    # reset chunk
                    chunk = Chunk()

                # if the pair is not (subset) of stored chunk (new chunk)
                else:
                    # add first word of pair to chunk, but do not store in chunkatory yet
                    chunk.ortho = chunk.ortho + [pair[0]]
                    stop = False
                    # if btp indicates boundary
                    if btp <= average:
                        # check whether chunk is already in chunkatory
                        matched_chunk = chunks.lookup(chunk)
                        if matched_chunk:
                            matched_chunk.count += 1
                            chunkpair = [previous_chunk, chunk]
                            c_count = chunkpairs.update_pairlist(chunkpair)
                            previous_chunk = matched_chunk
                            # reset chunk
                            chunk = Chunk()
                            stop = True

                        # if chunk is new to the chunkatory
                        if not stop:
                            # add chunk
                            chunk.count = 1
                            chunks.add_chunk(chunk)

                            # for production task
                            chunkpair = [previous_chunk, chunk]
                            c_count = chunkpairs.update_pairlist(chunkpair)
                            previous_chunk = chunk
                            chunks.size += 1
                            chunk = Chunk()
    return chunks, chunkpairs, ortho

# utterance production task from McCauley & Christiansen (2011) paper
# reconstructing child utterances from the corpus using the chunks and TPs discovered in the caregivers' utterances
def production_task(child_corpus, chunks, chunkpairs, keep_all=False):

    # initialization of variable to store all (reconstructed) utterances
    utterances = allUtterances()

    # determine size of largest chunk in the corpus
    maxlen_chunk = chunks.max_chunk_length

    # loop through all child utterances one-by-one to reconstruct them
    for i in range(0, NUM_UTTERANCES):
        if i % 1000 == 0:
            print("Currently processing utterance: "  + str(i))

        # save child utterance in new variable
        utterance = child_corpus[i]
        ut = Utterance()
        ut.num = i
        bag_of_chunks = []

        # adjust child utterance formatting
        u = []

        # NOTE: starting from "1" means removing "#" marker from child utterances,
        # Otherwise, the "#" can get parsed as part of one the chunks, which in turn
        # causes problems when trying to find the first chunk with the highest BTP
        # wrt to the beginning of the utterance
        for j in range(1, len(utterance)):
            u.append(utterance[j].ortho)
        copy_u = deepcopy(u)

        # making sure n (size of utterance matching chunk the algorithm searches for) isn't larger than the size of the utterance
        n = min(maxlen_chunk, len(utterance))

        # until utterance is found completely, or the size of the matching
        # chunk that is sought for is zero, search for chunks that match
        # (part of) the child utterance. If keep_all, then all utterances will
        # be reconstructed, even if they contain unseen words (as in McCauley &
        # Christiansen 2011).
        while len(u) != 0 and (n > 0 or keep_all):
            # reset stop-criterion and make copy of (part of) utterance
            stop = False
            temp_u = u[0:n]

            # when no chunk match is found, if keep_all, make new chunk with
            # btp of 0.0 to everything
            if n == 0 and keep_all:
                new_chunk = Chunk()
                new_chunk.ortho = [u[0]]
                new_chunk.count = 0
                bag_of_chunks.append(new_chunk)
                u = u[1:]
                n = min(maxlen_chunk, len(u))

            # check for a (partial) match
            matched_chunk = chunks.lookup_by_key(temp_u)
            if matched_chunk:
                # add chunk to bag of words and remove matching part from utterance
                chunk = matched_chunk
                bag_of_chunks.append(chunk)
                u = u[n:]
                stop = True
                # reset n
                n = min(maxlen_chunk, len(u))

            # if no match of length n could be found, decrease n to search for
            # a smaller matching chunk
            if not stop and n!= 0:
                n = n - 1

        # store original utterance, without the #-marker
        ut.ortho = utterance[1:]

        # if throwing away utterances with unseen words, mark utterances with
        # unaccounted-for words as skipped, and move on to the next utterance
        if not keep_all and len(u) > 0:
            ut.skipped = True
            ut.bag_of_chunks = []
            utterances.all.append(ut)
            utterances.size += 1
            continue

        # randomize order of chunks in bag_of_chunks
        shuffle(bag_of_chunks)
        ut.bag_of_chunks = deepcopy(bag_of_chunks)
        previous = ['#'] # beginning of utterance marker
        reconstructed_utterance = []
        first = True # True when searching for which chunk should be the first one in the utterance

        # as long as there still are unordered chunks in the bag of chunks, find the next chunk to rebuild the utterance
        # using the stored btps between chunks
        while not len(bag_of_chunks) == 0:
            max_btp = 0
            selected_chunk = bag_of_chunks[0]

            # determine chunk with highest btp compared to previously selected chunk (or # if first)
            for current in bag_of_chunks:
                # determine pair of previous selected chunk + current chunk c
                pair = []
                if first:
                    pair = [['#'],current.ortho]
                else:
                    pair = [previous,current.ortho]
                # determine how often the pair of chunks has been seen in corpus
                current_count = current.count
                if current_count == 0:
                    pair_count = 0
                else:
                    pair_count = determine_pc(pair, chunkpairs)

                # check whether the btp of the current chunk is highest
                temp_btp = compute_btp(current_count, pair_count)
                if max_btp < temp_btp:
                    max_btp = temp_btp
                    selected_chunk = current
                # if equal btps, randomly select old or new chunk
                elif max_btp == temp_btp:
                    rand = np.random.choice(2,1)
                    if rand == 0:
                        selected_chunk = current
            # save the selected chunk in the reconstruction, remove it from bag of chunks and update previous chunk
            reconstructed_utterance.append(selected_chunk)
            bag_of_chunks.remove(selected_chunk)
            previous = selected_chunk.ortho
            first = False

        # store reconstructed utterance
        ut.prediction = reconstructed_utterance
        utterances.all.append(ut)
        utterances.size += 1
    return utterances

# returns how often the chunkpair has been seen
def determine_pc(chunkpair, chunkpairs):
    for i in range(0, chunkpairs.size):
        temp_0 = []
        temp_1 = []
        # convert representation
        if isinstance(chunkpairs.all[i].ortho[0], Chunk):
            temp_0 += chunkpairs.all[i].ortho[0].ortho
        else:
            temp_0 += chunkpairs.all[i].ortho[0]
        if isinstance(chunkpairs.all[i].ortho[1], Chunk):
            temp_1 += chunkpairs.all[i].ortho[1].ortho
        else:
            temp_1 += chunkpairs.all[i].ortho[1]
        if chunkpair[0] == temp_0 and chunkpair[1] == temp_1:
            return chunkpairs.all[i].count
    return 0


# evaluates the reconstructed utterances and saves results in .csv file
def evaluate_and_save(utterances, child, age, output_filename):
    # create outputfile with file header
    with open(output_filename, "w") as output:
        writer = csv.writer(output, delimiter=";")
        header = ["num", "utterance", "child", "age", "skipped", "reconstructed", "bow", "gold", "prediction","chance","controlledscore"]
        writer.writerow(header)
        
        # evaluate reconstructed utterances one-by-by one
        for i in range(0, utterances.size):
            utterance = utterances.all[i].ortho
            num = utterances.all[i].num
            skip = utterances.all[i].skipped
            bow = []
            for chunk in utterances.all[i].bag_of_chunks:
                bow.append(chunk.ortho)
            # if utterance was not skipped because of lack of matching chunks, check if it was reconstructed correctly
            if not skip:
                true_temp = ""
                # determine true utterance
                for j in range(0, len(utterance)):
                    true_temp += str(utterance[j].ortho)
                # determine reconstruction
                reconstructed_temp = ""
                for j in range(0, len(utterances.all[i].prediction)):
                    reconstructed_temp += ''.join(utterances.all[i].prediction[j].ortho)
                #compare true utterance and reconstructed utterance
                if true_temp == reconstructed_temp:
                    utterances.all[i].reconstructed = True
                else:
                    utterances.all[i].reconstructed = False
                gold = true_temp
                prediction = reconstructed_temp
                reconstructed = utterances.all[i].reconstructed
                n_chunks = len(utterances.all[i].bag_of_chunks)
                chance = 1/(np.math.factorial(n_chunks))
                if utterances.all[i].reconstructed:
                    controlledscore = np.math.log(np.math.factorial(n_chunks))
                else:
                    controlledscore = np.math.log(1 - (1/np.math.factorial(n_chunks)))
            # if utterance was skipped, set all irrelevant variables to NaN
            else:
                true_temp = ""
                for j in range(0, len(utterance)):
                    true_temp += str(utterance[j].ortho)
                gold = true_temp
                prediction = 'NaN'
                bow = 'NaN'
                reconstructed = 'NaN'
                chance = 'NaN'
                controlledscore = 'NaN'

            # write utterance data to output file
            row = [str(num), str(utterance), child, age, str(skip), str(reconstructed), str(bow), str(gold),
                   str(prediction),str(chance),str(controlledscore)]
                writer.writerow(row)
print("Wrote output to: {}".format(os.path.relpath(output_filename)))
    return


def parse_arguments():
    p = argparse.ArgumentParser(description="Run the model")
    p.add_argument('--type', metavar='T', required=True,
            choices=['c', 'l'],
            help="'c' (cumulative) or 'l' (local)")
    p.add_argument('--child', metavar='C', required=True,
            choices=['Alex', 'Ethan', 'Lily', 'Naima', 'Violet', 'William'],
            help="'Alex', 'Ethan', 'Lily', 'Naima', 'Violet', or 'William'")
    p.add_argument('--age', metavar='A', required=True,
            choices=['1_0','1_6', '2_0', '2_6', '3_0', '3_6', '4_0'],
            help="'1_0','1_6', '2_0', '2_6', '3_0', '3_6', or '4_0'")
    p.add_argument('--max-utterances', metavar='N', type=int,
            help="train and test on up to N utterances")
    p.add_argument('--keep-all', action='store_true',
            help="keep utterances with previously-unseen words during test")
    return p.parse_args()

if __name__ == "__main__":
    args = parse_arguments()

    cwd = os.getcwd()
    if args.type == "c":
        LOC = os.path.join(cwd, 'model_input/cumulativesampledcorpus')
    elif args.type == "l":
        LOC = os.path.join(cwd, 'model_input/localsampledcorpus')
    else:
        raise ValueError('Unexpected type: {}'.format(args.type))

    caregiver_filename =  os.path.join(LOC,
            args.type +
            "_corpusProvidence_caregivers_" + args.child +
            "_age" + args.age +
            ".txt")

    caregiver_size_filename = os.path.join(LOC,
            args.type +
            "_corpusProvidence_caregivers_size" + args.child +
            "_age" + args.age +
            ".txt")

    child_filename = os.path.join(LOC,
            args.type +
            "_corpusProvidence_child" + args.child +
            "_age" + args.age +
            ".txt")

    child_size_filename = os.path.join(LOC,
            args.type +
            "_corpusProvidence_child_size" + args.child +
            "_age" + args.age +
            ".txt")

    output_filename = os.path.join(os.getcwd(), 'model_output',
            args.type +
            "_corpusProvidence_child" + args.child +
            "_age" + args.age +
            "_productiontask" +
            ("_keep_all" if args.keep_all else "") +
            ".csv")

    print('{} {} {}'.format(args.child, args.age, args.type))

    corpus, NUM_UTTERANCES, NUM_WORDS, PHRASE_MEASURES = fileread(
            caregiver_filename, caregiver_size_filename)

    NUM_PAIRS = 2 * NUM_WORDS
    NUM_CHUNKS = NUM_PAIRS
    NUM_FRAMES = NUM_PAIRS

    if (args.max_utterances):
        NUM_UTTERANCES = min(args.max_utterances, NUM_UTTERANCES)

    print("NUM_UTTERANCES = {}".format(NUM_UTTERANCES))

    print("Chunking...")
    chunks, chunkpairs, all = chunk_corpus(corpus)

    child_corpus, NUM_UTTERANCES, NUM_WORDS, PHRASE_MEASURES = fileread(
            child_filename, child_size_filename)

    if (args.max_utterances):
        NUM_UTTERANCES = min(args.max_utterances, NUM_UTTERANCES)

    print("NUM_UTTERANCES = {}".format(NUM_UTTERANCES))

    print("Reconstructing utterances...")

    utterances = production_task(child_corpus, chunks, chunkpairs, args.keep_all)

    print("Utterance reconstruction complete")

    evaluate_and_save(utterances, args.child, args.age, output_filename)

    print("Program done")