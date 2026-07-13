
# mistyR: An R Implementation of Musical Set Theory (MST)

Musical Set Theory is a mathematical formalization of various common objects found in the 12-TET tuning system. Although called set theory, musical set theory is more-so an application of group theory and combinatorics to 'pitch classes'. 
From pitch classes (the theoretical variant of a musical note), we are able to formally study objects like scales, musical inversions, transpositions, and various schools of harmony. 

## Misty

Misty is my current project to develop various objects of Musical Set Theory in R. I hope to one day generalize this for uses in practical musical analysis; vectorization of tonal features for neural network composition. There are two R modules for musical stylometry analysis, one for .krn files and one for .midi files, sharing a single piece-frame interface.

Currently, there are implementations for the following objects:
- Scales (Diatonic, Modal, Circle of Fifths)
- Chords (Inversions, Extensions, Tertiary Stacks)

## KRN Module

With Misty comes a module for extracting features from musical scores in the .kern file format. In addition to the feature extraction functionality, the module comes with accompanying functions to perform musical stylometry analysis of the scores. The source code for this descends from Emily Palmer's **museR** package.

Currently, there are implementations for the following features:
- Modal feature analysis (of scale degree frequencies)
- Rhythmic entropy
- Rhythmic subdivision frequencies

## MIDI Module

The MIDI module reads .mid binaries (via **tuneR**) into the same tidy piece frame the KRN module produces — `midi2df()` and `kern2df()` honor one column contract, so every analysis function runs on either format unchanged. Note spelling is key-aware per key-signature window (mid-piece modulations spell their own accidentals), rhythms are snapped to kern-style tokens, and simultaneous attacks form chords with the bass in slot 1.

## Citations

Emily Palmer's museR package could be found in the link here: https://github.com/empalmer/museR
