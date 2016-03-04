# rogueceiver
A simple interactive-fiction "roguelike" focused on detailed weapon manipulation, a la "Receiver". Written in QB64. 

# Compilation
You will need [QB64](http://www.qb64.net/) to compile the sourcecode. I may make Windows or Linux binaries available at a later date. Simply run QB64, load the code, and press F5.

# Gameplay
Roguecevier works much like other roguelikes, featuring permadeath, procedural/random world generation, and fine-grained (read: overly complex) controls. However, overall gameplay is more like a interactive fiction game, with the world and player actions being described in detailed text rather than symbolic ASCII or tile graphics. I may add some more roguelike interface elements in the future.

The main focus of gameplay follows and was inspired by the FPS game Receiver, namely detailed manipulation of the player's weapon. Every possible action is available, from ejecting the magazine and loading individual rounds, to press-checking the chamber and toggling the safety.

Currently only the weapon simulation is implemented. See the ToDo file for planned features.

# Controls

* F = Pull trigger
* P = Pull slide
* E = Eject/Insert magazine
* C = Check chamber
* U = Unload a round (if mag in hand)
* L = Load a round (if mag in hand)
* M = Swap magazines (if mag in hand)
* S = Toggle safety
* G = Get loose rounds off floor
* Arrow Keys = Move NSEW

All controls are subject to change, and may become case- or context-sensitive in the future.
