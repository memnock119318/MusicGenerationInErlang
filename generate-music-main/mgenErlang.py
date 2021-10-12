import click
from typing import List, Dict
from pyo import *
from algorithms.genetic import generate_genome, Genome, selection_pair, single_point_crossover, mutation

# The whole music composition genetic algorithm is implemented in this file with calling some function
# from algorithms.genetic

# the number of bits for a note
BITS_PER_NOTE = 4

# 11 kinds of keys
KEYS = ["C", "C#", "Db", "D", "D#", "Eb", "E", "F", "F#", "Gb", "G", "G#", "Ab", "A", "A#", "Bb", "B"]

# scales of melody
SCALES = ["major", "minorH", "minorM", "ionian", "dorian", "phrygian", "lydian", "mixolydian",
          "aeolian", "locrian", "wholeTone", "majorPenta", "minorPenta",
          "egyptian", "majorBlues", "minorBlues", "minorHungarian"]

# instruments to be used in track
INSTRUMENTS = ["prophet", "dsaw", "fm", "tb303", "pulse", "piano", "blade", "beep"]

# note name list, it is used to map MIDI numbers to the note() type in Erlang library
NOTES = ['c', 'cs', 'd', 'ds', 'e', 'f', 'fs', 'g', 'gs', 'a', 'as', 'b']

# octave is from 0 to 10
OCTAVES = list(range(11))
NOTES_IN_OCTAVE = len(NOTES)

# convert MIDI number to note name(like e5)
def number_to_note(number: int, velocity: int) -> str:
    if velocity == 0:
        note_str = "rest"
    else:
        octave = number // NOTES_IN_OCTAVE  # Integral part of division

        note = NOTES[number % NOTES_IN_OCTAVE]

        note_str = note + str(octave)

    return note_str


# transfer beat number to Erlang library 'beats' atom
def beat_to_duration(beat: int) -> str:
    return {
        2: "minim",
        1: "crotchet",
        0.5: "quaver",
        0.25: "semiquaver",
        0.125: "demisemiquaver",
        0.0625: "hemidemisemiquaver",

    }[beat]


# 0101->1+4 = 5  Binary to decimal
def int_from_bits(bits: List[int]) -> int:
    return int(sum([bit * pow(2, index) for index, bit in enumerate(bits)]))

# transfer genome to melody structure.
def genome_to_melody(genome: Genome, num_bars: int, num_notes: int, num_steps: int, instruments: List[str],
                     pauses: int, key: str, scale: str, root: int) -> Dict[str, list]:
    # iterate each note, in range of total number of notes
    notes = [genome[i * BITS_PER_NOTE:i * BITS_PER_NOTE + BITS_PER_NOTE] for i in range(num_bars * num_notes)]

    # max value of num_notes is 64, so num_notes can be 2, 4, 8, 16, 32, 64
    note_length = 4 / float(num_notes)

    # scale object in Pyo library, used to generate MIDI number array in this scale
    scl = EventScale(root=key, scale=scale, first=root)

    melody = {
        "notes": [],
        "velocity": [],
        "beat": [],
        "instruments": []
    }

    for note in notes:
        integer = int_from_bits(note)

        # if there is no pauses in this melody, rest bit can be removed
        if not pauses:
            # shift all notes to 3 bits
            integer = int(integer % pow(2, BITS_PER_NOTE - 1))

        # if there is pause, 1 bit for pause, 1 for T, 0 for F

        # if pause bit is 1, 1XXX, this is a rest note
        if integer >= pow(2, BITS_PER_NOTE - 1):
            melody["notes"] += [0]
            melody["velocity"] += [0]
            melody["beat"] += [note_length]

        # it is not a rest note, pause bit is 0. e.g. 0111
        else:
            melody["notes"] += [integer]
            melody["velocity"] += [127]
            melody["beat"] += [note_length]

    # number of steps decides number of tracks
    steps = []
    for step in range(num_steps):
        # append(): append a list of scale[] item, like for D major, scale[] in Pyo contains 15 items, 2 octave
        # D major: scale [] -> 62, 64， 66， 67， 69， 71， 73， 74， 76, 78， 79， 81， 83， 85， 86
        # step * 2 means move forwards two steps in the scale[] in each 'step'(iteration)
        # basically just randomly generate num_steps(like 3，4) notes

        # rest note can also change into  midi note number like 62 (D5), but the velocity is still 0,
        # so it can still be rest note.
        steps.append([scl[(note + step * 2) % len(scl)] for note in melody["notes"]])

    melody["notes"] = steps
    melody["instruments"] = instruments

    return melody


# transfer genome to Erlang code
def genome_to_code(genome: Genome, num_bars: int, num_notes: int, num_steps: int, instruments: List[str],
                   pauses: bool, key: str, scale: str, root: int, bpm: int, population_num: int) -> str:

    melody = genome_to_melody(genome, num_bars, num_notes, num_steps, instruments, pauses, key, scale, root)

    sounds = ""

    # len() is the number of tracks in melody.
    # length is used to judge whether the end of current sound track has reached
    length = len(melody["notes"]) - 1

    # len() is the total number of notes.
    # note_length is used to judge whether the end of note list has reached
    note_length = len(melody["velocity"]) - 1

    for i, step in enumerate(melody["notes"]):
        sounds += "{\n"
        sounds += "\"step" + str(i) + "\",\n"
        sounds += "\"" + melody["instruments"][i] + "\",\n[\n"

        for j, note in enumerate(step):
            sounds += "{"

            # rest node
            # non rest node
            sounds += number_to_note(note, melody["velocity"][j]) + ", "
            sounds += beat_to_duration(melody["beat"][j])

            if j < note_length:
                sounds += "},\n"
            else:
                sounds += "}"

        if i < length:
            sounds += "\n],\nno_effect,\nno_envelope\n},\n\n"
        else:
            sounds += "\n],\nno_effect,\nno_envelope\n}\n"

    code = "-module(" + scale + key + str(population_num) + ").\n-behaviour(melody).\n-compile(export_all).\n" \
                                                            "choose_play() -> [].\nsample_pattern_track() -> [].\n" \
                                                            "beats_per_minute() ->" + str(
        bpm) + ".\nsounds() -> [\n" + sounds + "]."

    # debug use: print the generated code
    # print(code)

    return code

# This function is used to calculate the fitness score for a genome, namely a melody, to evaluate the quality of it.
# It considers three aspects of generated melody: smoothness, rhythm and harmony
def fitness(genome: Genome, num_bars: int, num_notes: int, num_steps: int, instruments: List[str],
            pauses: bool, key: str, scale: str, root: int, bpm: int) -> int:

    smoothnessWeight = 0.375
    rhythmWeight = 0.125
    harmonyWeight = 0.5

    smoothnessScore = 0
    rhythmScore = 0
    harmonyScore = 0

    # Table that assigns different values to different intervals (in semitones). Higher valued intervals will
    # be picked more. Currently the 3rd(3 intervals,4 intervals) and 5th(7 intervals) are valued the highest,
    # and penalizes repeated notes, tritones(6 intervals) and sevenths(minor 10, major 11)


    harmonyIntervalsTable = {0: -20, 1: 5, 2: 5, 3: 50, 4: 50, 5: 30, 6: -10, 7: 50, 8: 10, 9: 40, 10: -2, 11: -2,
                             12: 10,
                             13: -5, 14: 5, 15: 5, 16: 50, 17: 50, 18: 30, 19: -10, 20: 50, 21: 10, 22: 40, 23: -2,
                             24: -2, 25: 10}

    melody = genome_to_melody(genome, num_bars, num_notes, num_steps, instruments, pauses, key, scale, root)

    #number of tatal rest notes
    numRests = 0

    #number of consecutive rest notes
    consecutiveRests = 0

    for j, vel in enumerate(melody["velocity"]):
        if vel == 0:
            numRests += 1

        # Calculate number of consecutive rests there are
        if j != 0 and melody["velocity"][j] == 0 and melody["velocity"][j - 1] == 0:
            consecutiveRests += 1

    for i, step in enumerate(melody["notes"]):

        for j, note in enumerate(step):

            # We can only determine smoothness in melody if we aren't at the first note of the genome
            # AND current node is not a rest, the preceding note isn't a rest
            if j != 0 and melody["velocity"][j] != 0 and melody["velocity"][j - 1] != 0:
                prevNote = step[j - 1]

                # ABSOLUTE SMOOTHNESS CALCULATION
                # Calculate how many semitones away this note is from the previous one
                noteDifference = abs(note - prevNote)

                # Add corresponding harmony score based on interval
                harmonyScore += harmonyIntervalsTable[noteDifference]

                # Penalize for repeating notes, smoothnessScore
                # if note difference = 0, if it is same note
                if not noteDifference:
                    smoothnessScore /= 10
                elif noteDifference <= 2:
                    smoothnessScore += 1

                # Penalize the major 7th interval
                elif noteDifference == 11:
                    smoothnessScore /= 2
                else:
                    if noteDifference != 0:
                        smoothnessScore += 1 / noteDifference

                # RELATIVE SMOOTHNESS CALCULATION
                # Relative pitch disregards the actual octave of the note:
                # if the notes are next to each other in the scale, then we can consider them being relatively smooth
                # This algorithm assume it only deals with notes in a two octaves range
                # so that's all it needs to consider

                # two octave is the default parameter in EventScale

                if abs(note - (prevNote + 12)) == 1 or abs(note - (prevNote + 12)) == 2 or abs(
                        (note + 12) - prevNote) == 1 or abs((note + 12) - prevNote) == 2:
                    smoothnessScore += 0.5

    # Penalizes any sequences that have too many rests
    if numRests * 10 <= len(melody["velocity"]):
        rhythmScore += 10

    penalty = 10

    # We don't want too many consecutive rests
    if consecutiveRests:
        rhythmScore -= (consecutiveRests * penalty)

    # Apply corresponding weights to scores in order to favour different characteristics
    fitness = (smoothnessScore * smoothnessWeight) + (rhythmScore * rhythmWeight) + (harmonyScore * harmonyWeight)

    # debug
    # print(f"smoothnessScore : {smoothnessScore * smoothnessWeight}")
    # print(f"rhythmScore : {rhythmScore * rhythmWeight}")
    # print(f"harmonyScore : {harmonyScore * harmonyWeight}")

    return fitness



# write Erlang code to the file.
def save_code_to_file(filename: str, code: str):
    os.makedirs(os.path.dirname(filename), exist_ok=True)
    with open(filename, "w") as f:
        f.write(code)
        f.close()

#command line user interface
@click.command()
@click.option("--num-bars", default=8, prompt='Number of bars:', type=int)
@click.option("--num-notes", default=4, prompt='Notes per bar:', type=int)
@click.option("--num-steps", default=1, prompt='Number of steps:', type=int)
@click.option("--instruments", default=','.join(INSTRUMENTS), prompt='Instruments:', type=click.STRING)
@click.option("--pauses", default=True, prompt='Pauses', type=bool)
@click.option("--key", default="C", prompt='Key:', type=click.Choice(KEYS, case_sensitive=False))
@click.option("--scale", default="major", prompt='Scale:', type=click.Choice(SCALES, case_sensitive=False))
@click.option("--root", default=4, prompt='Scale Root:', type=int)
@click.option("--population-size", default=10, prompt='Population size:', type=int)
@click.option("--num-mutations", default=2, prompt='Number of mutations:', type=int)
@click.option("--mutation-probability", default=0.5, prompt='Mutations probability:', type=float)
@click.option("--bpm", default=128, prompt='Bpm:', type=int)
@click.option("--max-generation", default=100, prompt='Max Generation:', type=int)

# Evolutionary function to evolve a population within given iterations
def main(num_bars: int, num_notes: int, num_steps: int, instruments: str, pauses: bool, key: str, scale: str, root: int,
         population_size: int, num_mutations: int, mutation_probability: float, bpm: int, max_generation: int):

    # split instruments by ',' and remove whitespace
    instruments_list = [c.strip() for c in instruments.split(',')]

    # validate passed columns
    for c in instruments_list:
        if c not in INSTRUMENTS:
            raise click.BadOptionUsage("%s is not an available instrument." % c)

    # input the src path of MusicGeneration project in system
    # e.g. "E:/EclipseIDE_Project/MusicGeneration/src"
    folder = input("Please input the src path of MusicGeneration project "
                   "(e.g. E:/EclipseIDE_Project/MusicGeneration/src) : ")

    population = [generate_genome(num_bars * num_notes * BITS_PER_NOTE) for _ in range(population_size)]

    # Debug use: to check genome information in the first population
    # for genome in population:
    #     print(f"Genome: {genome}")

    population_id = 0

    for i in range(max_generation):
        random.shuffle(population)

        population_fitness = [
            (genome, fitness(genome, num_bars, num_notes, num_steps, instruments_list, pauses, key, scale, root, bpm))
            for genome in population]

        # sort population according to fitness score
        sorted_population_fitness = sorted(population_fitness, key=lambda e: e[1], reverse=True)

        # sorted genomes
        population = [e[0] for e in sorted_population_fitness]

        # the best two genomes are retained in the new generation
        next_generation = population[0:2]

        # select two parents to reproduce two children
        for j in range(int(len(population) / 2) - 1):

            # look up the fitness score for the given genome
            def fitness_lookup(genome):
                length = len(population)
                for index, e in enumerate(population_fitness):
                    if e[0] == genome:
                        return length - index
                return 0

            # reproduce process
            parents = selection_pair(population, fitness_lookup)
            offspring_a, offspring_b = single_point_crossover(parents[0], parents[1])
            offspring_a = mutation(offspring_a, num=num_mutations, probability=mutation_probability)
            offspring_b = mutation(offspring_b, num=num_mutations, probability=mutation_probability)
            next_generation += [offspring_a, offspring_b]

        print(f"population {population_id} done")

        # when come to the last generation
        if i == (max_generation - 1):

            #save the last population Erlang files
            print("saving population Erlang files …")
            for i, genome in enumerate(population):
                population_code = genome_to_code(population[i], num_bars, num_notes, num_steps, instruments_list,
                                                 pauses, key,
                                                 scale, root, bpm, i)

                save_code_to_file(f"{folder}/{scale}{key}{i}.erl", population_code)
            print("done")

        population = next_generation
        population_id += 1


if __name__ == '__main__':
    main()
