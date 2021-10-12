%%The Melody module defines data structures that we use to represent music elements in the library
-module(melody).

%% envelope attribute name type: atom  
-type envelope_attribute_name() :: attack | decay | sustain | release | amplitude |
                                   attack_level | decay_level | sustain_level.

%% envelope attribute value type: float 
-type envelope_attribute_value() :: float().
              
%%envelope type 
-type envelope() ::  [{envelope_attribute_name(), envelope_attribute_value()}] | no_envelope.

%%note type, octave from 0 to 10
-type note() :: c0 | cs0 | db0 | d0 | ds0 | eb0 | e0 | f0 | fs0 | gb0 | g0 | gs0 | ab0 | a0 |
                as0 | bb0 | b0 |
                c1 | cs1 | db1 | d1 | ds1 | eb1 | e1 | f1 | fs1 | gb1 | g1 | gs1 | ab1 | a1 |
                as1 | bb1 | b1 |
                c2 | cs2 | db2 | d2 | ds2 | eb2 | e2 | f2 | fs2 | gb2 | g2 | gs2 | ab2 | a2 |
                as2 | bb2 | b2 |
                c3 | cs3 | db3 | d3 | ds3 | eb3 | e3 | f3 | fs3 | gb3 | g3 | gs3 | ab3 | a3 |
                as3 | bb3 | b3 |
                c4 | cs4 | db4 | d4 | ds4 | eb4 | e4 | f4 | fs4 | gb4 | g4 | gs4 | ab4 | a4 |
                as4 | bb4 | b4 |
                c5 | cs5 | db5 | d5 |ds5 | eb5 | e5 | f5 | fs5 | gb5 | g5 |gs5 | ab5 | a5 |
                as5 | bb5 | b5 |
                c6 | cs6 | db6 | d6 | ds6 | eb6 | e6 | f6 | fs6 | gb6 | g6 | gs6 | ab6 | a6 |
                as6 | bb6 | b6 |
                c7 | cs7 | db7 | d7 | ds7 | eb7 | e7 | f7 | fs7 | gb7 | g7 | gs7 | ab7 | a7 |
                as7 | bb7 | b7 |
                c8 | cs8 | db8 | d8 | ds8 | eb8 | e8 | f8 | fs8 | gb8 | g8 | gs8 | ab8 | a8 |
                as8 | bb8 | b8 |
                c9 | cs9 | db9 | d9 | ds9 | eb9 | e9 | f9 | fs9 | gb9 | g9 | gs9 | ab9 | a9 |
                as9 | bb9 | b9 |
                c10 | cs10 | db10 | d10 | ds10 | eb10 | e10 | f10 | fs10 | gb10 | g10 | gs10 | ab10 | a10 |
                as10 | bb10 | b10 | rest.

%% beats type, the duration of a note
-type beats() :: hemidemisemiquaver | demisemiquaver | semiquaver | quaver | crotchet |
          minim | semibreve | breve | dottedcrotchet .

%% used to input BPM of a melody
-callback beats_per_minute() -> non_neg_integer().   

%% sound is a list of notes and theis beats
-type sound() :: [{note(), beats()}].

%% name of the sound, a string  
-type soundname() :: string().

%% the sounds track, it is a list of sounds, each sound has a set of notes, beats
%% its sound name, instrument to play it, sound effect, envelope setting 
-callback sounds() -> [{soundname(), instrument(), sound(), effect(), envelope()}].

%% effect of sound 
-type effect() :: reverb | echo | wobble | slicer | distortion | no_effect.

%%instrument for sound
-type instrument() :: string().

%% sample name, sample is a sound sample in Sonic Pi  
-type sample() :: string().

%% rest note 
-type rest() :: o.
%% hit note 
-type hit() :: x.

%% a pattern for notes 
%% x lasts 1 beat 
%% o rests 1 beat
-type pattern() :: [hit() | rest()].

%% the pattern track, it is a list of samples.
%%each sample is played in a given pattern, effect and envelope setting 
%% only amplitude is useful for sample, duration needs to be updated by beat_stretch
-callback sample_pattern_track() -> [{sample(), pattern(), effect(), envelope()}].

%% octave to transpose 
-type octave() :: integer().

%%the name of choose_play track
-type choose_name() :: string().

%%the choose_play track, it is a list of choose tuples 
%% each choose tuple has its name, instrument and 
%% the lists of note, beats and octaves for randomly choosing 
-callback choose_play() -> [{choose_name(), instrument(), [note()], [beats()], [octave()]}].






