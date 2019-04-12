import numpy as np

def test(filename,sfilename):
    sfile = open(sfilename, "r")
    NUM_UTTERANCES = int(sfile.readline())
    sfile.close()

    file = open(filename, "r")
    corpus = np.ndarray((NUM_UTTERANCES,), dtype=np.object)
    NUM_WORDS = 0

    for i in range(0, NUM_UTTERANCES):
        temp = file.readline()
        temp = temp.split()
        #print temp
        if len(temp) <= 1:
            print "empty line here!: " + str(i)
        #temp = temp.split()





if __name__ == "__main__":
    """
	Structure idea similar to Feldman et al. 2013:
	- process corpus:
		- determine frames from chunks and occurence probability
		- determine probability of ortho in frame-slots
	"""

    ##ADJUST THESE###
    CHILD = "Lily"
    AGE = "3_6"
    ##################

    print ('Start'+ CHILD)
    filename = "a_corpusProvidence_caregivers_" + CHILD + "_age" + AGE + ".txt"
    sfilename = "a_corpusProvidence_caregivers_size" + CHILD + "_age" + AGE + ".txt"#
    test(filename, sfilename)