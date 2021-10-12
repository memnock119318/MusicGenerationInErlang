%% This module is used to is transfer the song in music file 
%%to Sonic Pi code and plays song with Sonic Pi server using the Sonic Pi Tool

-module(sound_generate).

-export([main/2]).

%% main function is used to play music using Erlang files or Ruby files
%% when 'Choice' equals to 1, play music using Erlang files, 
%%'Arg' is file name, e.g. main(majorC0, 1)
%%when 'Choice' is 2, play music using Ruby files, 
%%'Arg' is file name, e.g. main(sonicpi_correctness, 2)

main(Arg, Choice) -> 
    case integer_to_list(Choice) of
    "1" -> use_eval_play_sounds(Arg);
    "2" -> playFromFile(atom_to_list(Arg))
    end.



%% use Eval command of Sonic Pi tool to play the whole music file 
%% it transfers the music file into Sonic Pi code and writes code into 'out.rb'
use_eval_play_sounds(Arg) ->
    RawSounds = Arg:sounds(),

    %%write headline command of sonic-pi-tool
    writeHeadline(),
    
    %%write bmp as global variable
    Bmp = integer_to_list(Arg:beats_per_minute()),
    write("use_bpm " ++ Bmp ++ "; "),
    
    %%extract each sound tuple in sound track and transfer it 
    EachSound = lists:map(
        fun({Soundname, Instrument, Sound, Effect, Envelope}) ->
            sound(Soundname, Instrument, Sound, Effect, Envelope)
        end, RawSounds),

    %%transfer the pattern track 
    use_eval_play_pattern_track(Arg),
    
    %%transfer the choose_play track  
    use_eval_play_choose(Arg),
    
    %%write tail command of sonic-pi-tool
    writeTail(), 
    
    %%run the eval command of Sonic PI tool to play the music 
    play().

%% transfer sound tuple into Sonic Pi code, each tuple contains a set of notes 
sound(Soundname, Instrument, Sound, Effect, Envelope) ->

    %write live_loop
    write("live_loop :" ++ Soundname ++ " do; "),

    %write FX (effect)
    if 
        Effect == no_effect ->
            ok;
        true ->
             write("with_fx :"++ atom_to_list(Effect) ++" do; ")
    end,
    
    %write synth (instrument)
    Synth = Instrument,
    write("use_synth :" ++ Synth ++ "; "),
    
    %write envelope
    if
        Envelope == no_envelope ->
            ok;
        true ->
            write("use_synth_defaults "),
            
            %% divide envelope list into two parts: the last one and the others 
            Last_envelope = lists:last(Envelope),
            Dropped_envelope =  lists:droplast(Envelope),

            %deal with each envelope element in dropped envelope list 
            lists:map(
                fun({EnvelopeName, EnvelopeValue}) ->
                    envelope(EnvelopeName, EnvelopeValue)
                end, Dropped_envelope),
            
            %deal with the last envelope element
            last_envelope(Last_envelope)
    end,

    
    %write play and sleep
    Soundcontent = lists:map(
        fun({Note, Beats}) ->
            soundcontent(Note, Beats)
        end, Sound),
    
     %end of FX
     if 
        Effect == no_effect ->
            ok;
        true ->
            write("end;")
     end,
    
     %write end
     write("end; ").

%%tranfer the last envelop element in envelope list into Sonic Pi code 
last_envelope(Last_envelope) ->
    {EnvelopeName, EnvelopeValue} = Last_envelope,
    write(atom_to_list(EnvelopeName) ++ ": " ++ float_to_list(EnvelopeValue, [{decimals, 4}, compact]) ++ "; ").
    
%% transfer the envelope element into Sonic Pi code 
envelope(EnvelopeName, EnvelopeValue) ->
     write(atom_to_list(EnvelopeName) ++ ": " ++ float_to_list(EnvelopeValue, [{decimals, 4}, compact]) ++ ", ").

%% transfer one sound tuple, which has a note and its duration 
%%duration which is more than 1 beat needs to change the envelope attributes to stretch Beat 
soundcontent(Note, Beats) when Beats > 0.0 ->
     
     Cmd = "play :" ++ atom_to_list(Note),
     case Beats of 
         hemidemisemiquaver -> New_Cmd  = Cmd ++ "; sleep 0.0625; ";
         demisemiquaver -> New_Cmd  = Cmd ++ "; sleep 0.125; ";
         semiquaver -> New_Cmd  = Cmd ++ "; sleep 0.25; ";
         quaver -> New_Cmd  = Cmd ++ "; sleep 0.5; ";
         crotchet -> New_Cmd  = Cmd ++ "; sleep 1.0; ";
         minim -> New_Cmd  = Cmd ++ ", release: 2.0; " ++ "sleep 2.0; ";
         semibreve -> New_Cmd  = Cmd ++ ", release: 4.0; " ++ "sleep 4.0; ";
         breve -> New_Cmd  = Cmd ++ ", release: 8.0; " ++ "sleep 8.0; ";
         dottedcrotchet -> New_Cmd  = Cmd ++ ", release: 1.5; " ++ "sleep 1.5; "
     end,
     write(New_Cmd),
     New_Cmd.

%%write the command into output file 
write(Cmd) ->
    %% Get file handler and append
    {ok, Fh} = file:open("out.rb", [append]),
    file:write(Fh, Cmd).

%%write the head line of eval command 
writeHeadline() ->
    %% Get file handler and write
    {ok, Fh} = file:open("out.rb", [write]),
    
    %% Write the text
    N = "python sonic-pi-tool.py eval " ++ "\"",
    file:write(Fh, N).

%%write the tail line of eval command 
writeTail() ->
    %% Get file handler and append
    {ok, Fh} = file:open("out.rb", [append]),
    N = "\"",
    file:write(Fh, N).

%%run the eval command to play music 
play() ->
    {Status,Value} = file:open("out.rb",read),
    if Status =:= ok ->
            io:format("==== open successed ====~n"),
            %%read file, read one line from file 
            OneLine = io:get_line(Value,""),
            io:format("read file_value~p ~n",[OneLine]),
            os:cmd (OneLine);
        Status =/= ok ->
            io:format("open file error!")
    end.

%%use run-file command from Sonic Pi tool to play Ruby file 
playFromFile(OldFilename) ->
    Filename = OldFilename ++ ".rb",
    {Status,Value} = file:open(Filename,read),
    if Status =:= ok ->
            Cmd = "python sonic-pi-tool.py run-file " ++ Filename,
            os:cmd (Cmd);
        Status =/= ok ->
            io:format("open file error!")
    end.


%%transfer pattern track into Sonic Pi code 
use_eval_play_pattern_track(Arg) ->
    RawTracks = Arg:sample_pattern_track(),
    
    %%extract each track from the list and deal with it 
    Tracks = lists:map(
        fun({Sample, Pattern, Effect, Envelope}) ->
            makeTrack(Sample, Pattern, Effect, Envelope)
        end, RawTracks).

%%transfer each pattern track 
makeTrack(Sample, Pattern, Effect, Envelope) ->
    %%write live_loop
    write("live_loop :"++ Sample ++" do; "),
     
    %%write FX
    if 
        Effect == no_effect ->
            ok;
        true ->
             write("with_fx :"++ atom_to_list(Effect) ++" do; ")
    end,
    
    %%write envelope
    if
        Envelope == no_envelope ->
            ok;
        true ->
            write("use_sample_defaults "),
            Last_envelope = lists:last(Envelope),
            Dropped_envelope =  lists:droplast(Envelope),

            %%each envelope element
            lists:map(
                fun({EnvelopeName, EnvelopeValue}) ->
                    envelope(EnvelopeName, EnvelopeValue)
                end, Dropped_envelope),
            
            %%last envelope element
            last_envelope(Last_envelope)
    end,
    
    %%write sample and sleep
    lists:map(
       fun(P) ->
           makePattern(P, Sample)
       end, Pattern),
    
    %%end of FX
    if 
        Effect == no_effect ->
            ok;
        true ->
            write("end;")
    end,

    %%end of live_loop
    write("end; ").

%%write sample name and sleep
%% the beat for sample is always one beat
makePattern(P, Sample) ->
     if 
        P == x ->
            Cmd = "sample :" ++ Sample ++ "; sleep 1; ",
            write(Cmd);
        true ->
            Cmd = "sleep 1; ",
            write(Cmd)
     end.

%% transfer choose_play track into Sonic Pi code 
use_eval_play_choose(Arg) ->
    RawChooses = Arg:choose_play(),
    
    %%extract and deal with each choose tuple
    Chooses = lists:map(
        fun({ChooseName, Instrument, Note, Beats, Octave}) ->
            makeChoose(ChooseName, Instrument, Note, Beats, Octave)
        end, RawChooses).

%% transfer each choose tuple into Sonic Pi code 
makeChoose(ChooseName, Instrument, Note, Beats, Octave) ->
    %%write live_loop
    write("live_loop :"++ ChooseName ++" do; "),
    
    %%write synth
    Synth = Instrument,
    write("use_synth :" ++ Synth ++ "; "),
    
    %%write use_octave
    TransferOctave = lists:map(
       fun(O) ->
           makeOctave(O)
       end, Octave),
    WrittenOctave = string:join(TransferOctave, ","),
    write("use_octave ["++ WrittenOctave ++"].choose; "),
    
    %%write play
    TransferNote = lists:map(
       fun(N) ->
           makeNote(N)
       end, Note),
    WrittenNote = string:join(TransferNote, ","),
    write("play ["++ WrittenNote ++"].choose; "),

    %%write sleep
    TransferBeats = lists:map(
       fun(B) ->
           makeBeats(B)
       end, Beats),
    WrittenBeats = string:join(TransferBeats, ","),
    write("sleep ["++ WrittenBeats ++"].choose; "),
    
    %%end of live_loop
    write("end; ").

%% transfer the  octave type item in a choose_play tuple 
makeOctave(O) ->
    New_O = integer_to_list(O),
    New_O.

%%tranfer the note type item in a choose_play tuple 

makeNote(N) ->
    New_N = ":" ++ atom_to_list(N),
    New_N.

%%tranfer the beats type item in a choose_play tuple 
makeBeats(B) ->
    case B of 
         hemidemisemiquaver -> New_B = "0.0625";
         demisemiquaver -> New_B  = "0.125";
         semiquaver -> New_B  = "0.25";
         quaver -> New_B  = "0.5";
         crotchet -> New_B  = "1.0";
         minim -> New_B  = "2.0";
         semibreve -> New_B  = "4.0";
         breve -> New_B  = "8.0";
         dottedcrotchet -> New_B  = "1.5"
    end,
    New_B.


