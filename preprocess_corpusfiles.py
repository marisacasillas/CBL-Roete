# -*- coding: utf-8 -*-
import glob, os, re

# IMPORTANT: for this script to run correctly, place it in the same folder
# as the set of *.cha-files from the corpus of one child at one age you'd like to
# convert into two model input - files, one containing the child's utterances and
# one containing the caregivers' utterances.
# It cleans up the minCHAT lines by removing the speaker tier, punctuation marks
# (question marks, exclamation marks & comma's), interruption markers (+/ and +//),
# unintelligentable speech ('xxx'), # assimilation remarks ('(...)') and
# explanations ('[...]').

if __name__ == "__main__":

    # Create output files
    child_name = "Alex" # Set child name to the appropriate child name: "Alex", "Ethan", "Lily", "Naima", "Violet" or "William"
    age = "1_6" # Set child age to the appropriate child age "1_0" "1_6" "2_0" "2_6" "3_0" "3_6" or "4_0"
    output_caregivers_filename = "corpusProvidence_caregivers_"  + child_name + "_age" + age + ".txt"
    soutput_caregivers_filename = "corpusProvidence_caregivers_size" + child_name +"_age" + age + ".txt"
    outputfile = open(output_caregivers_filename, "w")
    output_child_filename = "corpusProvidence_child" + child_name +"_age" + age + ".txt"
    soutput_child_filename = "corpusProvidence_child_size" + child_name +"_age" + age + ".txt"
    outputchildfile = open(output_child_filename, "w")

    # Initialize variables
    num_utterances = 0
    num_words = 0
    num_child_utterances = 0
    num_child_words = 0

    # For all *.cha-files in the current folder, remove punctation marks and other unwanted minCHAT formatting from utterances and write them to the output files
    for file in glob.glob("*.cha"):
        with open(file, "r") as f:
            for line in f:
                # If the utterance is spoken by the child, apply the relevant cleaning actions.
                if "*CHI:" in line:
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*CHI:\t', 1)
                    nline = nline[1]
                    nline = nline.replace('[?]', '')
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(0.*?\s)", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('‹', '')
                    nline = nline.replace('›', '')
                    nline = nline.replace('yyy', '')
                    nline = nline.replace('<', '')
                    nline = nline.replace('>', '')
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(",", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")
                    # if the line still contains text after removing punctation marks and other unwanted minCHAT formatting, write utterance to output file, including a #-marker to signal the beginning of a new utterance
                    if not len(nline) == 0:
                        nline = '# ' + nline
                        num_child_utterances += 1
                        outputchildfile.write(nline + "\n")

                # If the utterance is spoken by one of the caregivers, apply the relevant cleaning actions and add utterance to the output file. This cleaning process is done separately for each caregiver.
                # if spoken by mother
                if "*MOT:" in line:
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*MOT:\t', 1)
                    nline = nline[1]
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(", ", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        nline = '# ' + nline
                        num_utterances += 1
                        outputfile.write(nline + "\n")

                # if spoken by father
                if "*FAT:" in line:
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*FAT:\t', 1)
                    nline = nline[1]
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(", ", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        nline = '# ' + nline
                        num_utterances += 1
                        outputfile.write(nline + "\n")

                # if spoken by grandmother
                if "*GRA:" in line:
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*GRA:\t', 1)
                    nline = nline[1]
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(", ", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        nline = '# ' + nline
                        num_utterances += 1
                        outputfile.write(nline + "\n")

                if "*GRN:" in line:
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*GRN:\t', 1)
                    nline = nline[1]
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(", ", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        nline = '# ' + nline
                        num_utterances += 1
                        outputfile.write(nline + "\n")

                if "*ADU:" in line:
                    nline = line.split('.', 1)
                    nline = nline[0]
                    nline = nline.split('*ADU:\t', 1)
                    nline = nline[1]
                    nline = nline.split('?', 1)
                    nline = nline[0]
                    nline = nline.split('!', 1)
                    nline = nline[0]
                    nline = re.sub(r"(\(.*?\))", "", nline)
                    nline = re.sub(r"(\[.*?\])", "", nline)
                    nline = re.sub(r"(\+.*\s*)", " ", nline)
                    nline = re.sub(r"(\@.*?\s)", " ", nline)
                    nline = nline.replace('xxx', '')
                    nline = nline.replace(", ", "")
                    nline = nline.replace("(", "")
                    nline = nline.replace(":", "")
                    nline = nline.replace("  ", " ")
                    nline = nline.strip(" ")

                    if not len(nline) == 0:
                        nline = '# ' + nline
                        num_utterances += 1
                        outputfile.write(nline + "\n")

    # Close the outputfiles
    outputfile.close()
    soutputfile = open(soutput_caregivers_filename, "w")
    soutputfile.write(str(num_utterances))
    soutputfile.close()
    outputchildfile.close()
    soutput_child_file = open(soutput_child_filename, "w")
    soutput_child_file.write(str(num_child_utterances))
    soutput_child_file.close()
    print("Program done")
