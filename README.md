# Knights-And-Rage
PROG2006 - ADVANCED PROGRAMMING
> Version 0.8<br>

##### By Daniel Hao Huynh
Use `stack run` or `stack exec Knights-And-Rage-exe` in order to start the game via stack(this is for without flags)

Use `.stack-work\dist\d53b6a14\build\Knights-And-Rage-exe\Knights-And-Rage-exe`<br> from current project directory, in order to run the game directly <br>then in order to start the application with flags run:<br> ` --h` or ` --help` after the original call for the application(this is for flags)
> gamemode is coded in application instead of pipes
#### All commands for running application without flags
    stack run

    stack exec Knights-And-Rage-exe

#### All commands for running application with flags( from current directory)
    .\.stack-work\dist\d53b6a14\build\Knights-And-Rage-exe\Knights-And-Rage-exe.exe --h

    .\.stack-work\dist\d53b6a14\build\Knights-And-Rage-exe\Knights-And-Rage-exe.exe --help
#### Testing
Use `stack test` to run testing

> Used HUnit for unit testing

# Gameplay
Recommended to play by running `main` in `stack ghci` for best gameplay experience(since some functionality in the code(wait) on putStr only works in stack ghci)

> The help flag will give addition information, such as explainations about the game
<br><br>
The game has yet to include character concurrency, however that would be the next thing implemented.

